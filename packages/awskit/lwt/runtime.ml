open Base

let src = Logs.Src.create "awskit-lwt" ~doc:"AWS Lwt HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Client : Cohttp_lwt.S.Client) = struct
  let default_max_response_drain_bytes = 64 * 1024 * 1024

  type conn = {
    ctx : Client.ctx option;
    endpoint : Awskit.Endpoint.t option;
    region : Awskit.Region.t;
    credentials_provider :
      unit -> (Awskit.Credentials.t, Awskit.Error.t) Result.t Lwt.t;
    clock : unit -> Ptime.t;
    retry_policy : Awskit.Retry.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    max_response_drain_bytes : int;
  }

  type request_body_writer = {
    push : string option -> unit;
    remaining : int64 option ref;
    mutable write_error : Awskit.Error.t option;
  }

  type request_body =
    | Body of Awskit.Body.Request.descriptor * Cohttp_lwt.Body.t
    | Stream of
        Awskit.Body.Request.descriptor
        * (request_body_writer -> (unit, Awskit.Error.t) Result.t Lwt.t)

  type response_body = {
    body : Cohttp_lwt.Body.t;
    max_response_drain_bytes : int;
  }

  type response_body_reader = {
    stream : string Lwt_stream.t;
    mutable chunk : string;
    mutable offset : int;
  }

  let validate_create_args ?endpoint ~max_response_drain_bytes () =
    if max_response_drain_bytes <= 0 then
      invalid_arg
        "Awskit_lwt.Make.create: max_response_drain_bytes must be positive";
    Option.iter endpoint ~f:(fun endpoint ->
        ignore (Awskit.Endpoint.to_url_prefix endpoint))

  let create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider ~clock ?(retry_policy = Awskit.Retry.default)
      ?(sleep = fun _ -> Lwt.return_unit)
      ?(max_response_drain_bytes = default_max_response_drain_bytes) () =
    validate_create_args ?endpoint ~max_response_drain_bytes ();
    {
      ctx;
      endpoint;
      region;
      credentials_provider;
      clock;
      retry_policy;
      sleep;
      max_response_drain_bytes;
    }

  let create ?ctx ?endpoint ~region ~credentials ~clock ?retry_policy ?sleep
      ?max_response_drain_bytes () =
    create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider:(fun () -> Lwt.return_ok credentials)
      ~clock ?retry_policy ?sleep ?max_response_drain_bytes ()

  (* URI construction *)

  let make_uri (request : Awskit.Request.t) =
    let target = request.target in
    let scheme_str = Awskit.Endpoint.Scheme.to_string target.scheme in
    let host_port =
      match target.port with
      | Some port -> Fmt.str "%s:%d" target.host port
      | None -> target.host
    in
    Uri.of_string
      (Fmt.str "%s://%s%s" scheme_str host_port
         (Awskit.Request.Target.path_and_query target))

  (* Method conversion *)

  let to_cohttp_meth = function
    | `GET -> `GET
    | `PUT -> `PUT
    | `POST -> `POST
    | `DELETE -> `DELETE
    | `HEAD -> `HEAD
    | `PATCH -> `PATCH

  (* Response conversion *)

  let to_aws_response http_response =
    Awskit.Response.create_exn
      ~status:
        (Cohttp.Response.status http_response |> Cohttp.Code.code_of_status)
      ~headers:(Cohttp.Response.headers http_response |> Cohttp.Header.to_list)
      ()

  let descriptor_for_string body =
    {
      Awskit.Body.Request.content_length =
        Some (String.length body |> Int64.of_int);
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let empty_request_body = Body (descriptor_for_string "", Cohttp_lwt.Body.empty)

  let string_request_body body =
    Body (descriptor_for_string body, Cohttp_lwt.Body.of_string body)

  let bytes_request_body body =
    let body = Bytes.to_string body in
    string_request_body body

  let stream_request_body descriptor ~write = Stream (descriptor, write)

  let request_body_descriptor = function
    | Body (descriptor, _) -> descriptor
    | Stream (descriptor, _) -> descriptor

  let body_error message = Awskit.Error.body message

  let writer_for descriptor push =
    {
      push;
      remaining = ref descriptor.Awskit.Body.Request.content_length;
      write_error = None;
    }

  let check_write_length writer string =
    match !(writer.remaining) with
    | None -> Ok ()
    | Some remaining ->
        let length = Int64.of_int (String.length string) in
        if Stdlib.Int64.compare length remaining > 0 then
          Error (body_error "request body exceeded declared content_length")
        else (
          writer.remaining := Some (Stdlib.Int64.sub remaining length);
          Ok ())

  let check_finished_length writer =
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match !(writer.remaining) with
        | None | Some 0L -> Ok ()
        | Some _ ->
            Error
              (body_error "request body ended before declared content_length"))

  let write_request_body_string writer string =
    match writer.write_error with
    | Some error -> Lwt.return_error error
    | None -> (
        match check_write_length writer string with
        | Error error ->
            writer.write_error <- Some error;
            Lwt.return_error error
        | Ok () ->
            writer.push (Some string);
            Lwt.return_ok ())

  let body_to_cohttp = function
    | Body (_, body) -> (body, Lwt.return_ok ())
    | Stream (descriptor, write) ->
        let stream, push = Lwt_stream.create () in
        let writer = writer_for descriptor push in
        let finished, wake_finished = Lwt.wait () in
        let wake_finished_once =
          let woken = ref false in
          fun result ->
            if not !woken then (
              woken := true;
              Lwt.wakeup_later wake_finished result)
        in
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                Lwt.bind (write writer) (function
                  | Ok () ->
                      let result = check_finished_length writer in
                      push None;
                      wake_finished_once result;
                      Lwt.return_unit
                  | Error error ->
                      Log.warn (fun m ->
                          m "request body stream failed: %s"
                            (Awskit.Error.to_string_hum error));
                      push None;
                      wake_finished_once (Error error);
                      Lwt.return_unit))
              (fun exn ->
                let error = body_error (Exn.to_string exn) in
                Log.warn (fun m ->
                    m "request body stream raised: %s"
                      (Awskit.Error.to_string_hum error));
                push None;
                wake_finished_once (Error error);
                Lwt.return_unit));
        (Cohttp_lwt.Body.of_stream stream, finished)

  (* HTTP call *)

  let do_with_response (conn : conn) (request : Awskit.Request.t) request_body
      ~f =
    let uri = make_uri request in
    let headers = Cohttp.Header.of_list request.headers in
    let body, request_body_finished = body_to_cohttp request_body in
    let meth = to_cohttp_meth request.method_ in
    let response =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (Client.call ?ctx:conn.ctx ~headers ~body ~chunked:false meth uri)
            (fun (response, response_body) ->
              Lwt.bind request_body_finished (function
                | Error error -> Lwt.return_error error
                | Ok () ->
                    Log.debug (fun m ->
                        m "HTTP %d"
                          (Cohttp.Response.status response
                          |> Cohttp.Code.code_of_status));
                    Lwt.return_ok
                      ( to_aws_response response,
                        {
                          body = response_body;
                          max_response_drain_bytes =
                            conn.max_response_drain_bytes;
                        } ))))
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn ->
              let message = Exn.to_string exn in
              Log.warn (fun m -> m "HTTP call failed: %s" message);
              Lwt.return_error (Awskit.Error.transport ~retryable:true message))
    in
    Lwt.bind response (function
      | Error _ as error -> Lwt.return error
      | Ok (response, body) -> f response body)

  (* Module satisfying Awskit.Runtime.S *)

  module Runtime = struct
    type +'a t = 'a Lwt.t

    let return = Lwt.return
    let bind = Lwt.bind

    type connection = conn
    type nonrec request_body = request_body
    type nonrec response_body = response_body
    type nonrec request_body_writer = request_body_writer
    type nonrec response_body_reader = response_body_reader

    let now c = c.clock ()
    let region c = c.region
    let credentials c = c.credentials_provider ()
    let endpoint c = c.endpoint
    let retry_policy c = c.retry_policy
    let sleep c span = c.sleep span
    let empty_request_body = empty_request_body
    let string_request_body = string_request_body
    let bytes_request_body = bytes_request_body
    let stream_request_body = stream_request_body
    let request_body_descriptor = request_body_descriptor
    let write_request_body_string = write_request_body_string

    module Request_body = struct
      let empty = empty_request_body
      let of_string = string_request_body
      let of_bytes = bytes_request_body
      let of_stream = stream_request_body
      let descriptor = request_body_descriptor
      let write_string = write_request_body_string
    end

    let rec read_from_current reader bytes ~off ~len =
      if len = 0 then Lwt.return_ok 0
      else if reader.offset < String.length reader.chunk then begin
        let available = String.length reader.chunk - reader.offset in
        let copied = min available len in
        Stdlib.String.blit reader.chunk reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        Lwt.return_ok copied
      end
      else
        Lwt.bind (Lwt_stream.get reader.stream) (function
          | None -> Lwt.return_ok 0
          | Some chunk ->
              reader.chunk <- chunk;
              reader.offset <- 0;
              read_from_current reader bytes ~off ~len)

    let invalid_read_bounds bytes ~off ~len =
      off < 0 || len < 0 || len > Bytes.length bytes - off

    let read_response_body reader bytes ~off ~len =
      if invalid_read_bounds bytes ~off ~len then
        Lwt.return_error (Awskit.Error.body "invalid read bounds")
      else
        Lwt.catch
          (fun () -> read_from_current reader bytes ~off ~len)
          (function
            | Lwt.Canceled -> Lwt.fail Lwt.Canceled
            | exn -> Lwt.return_error (Awskit.Error.body (Exn.to_string exn)))

    let drain_limit_error max_response_drain_bytes =
      Awskit.Error.body
        ~limit:(Int64.of_int max_response_drain_bytes)
        "response body exceeded max_response_drain_bytes"

    let rec drain_reader reader ~remaining ~max_response_drain_bytes =
      let buffer = Bytes.create 8192 in
      let len =
        if remaining <= 0 then 1 else min (Bytes.length buffer) remaining
      in
      Lwt.bind (read_response_body reader buffer ~off:0 ~len) (function
        | Error _ as error -> Lwt.return error
        | Ok 0 -> Lwt.return_ok ()
        | Ok n ->
            if n > remaining then
              Lwt.return_error (drain_limit_error max_response_drain_bytes)
            else
              drain_reader reader ~remaining:(remaining - n)
                ~max_response_drain_bytes)

    let drain_response_body_reader reader body =
      drain_reader reader ~remaining:body.max_response_drain_bytes
        ~max_response_drain_bytes:body.max_response_drain_bytes

    let with_response_body body ~consume =
      let reader =
        { stream = Cohttp_lwt.Body.to_stream body.body; chunk = ""; offset = 0 }
      in
      Lwt.bind (consume reader) (fun result ->
          Lwt.bind (drain_response_body_reader reader body) (function
            | Ok () -> Lwt.return result
            | Error error -> Lwt.return_error error))

    let discard_response_body body =
      let reader =
        { stream = Cohttp_lwt.Body.to_stream body.body; chunk = ""; offset = 0 }
      in
      drain_response_body_reader reader body

    module Response_body = struct
      let read = read_response_body
      let with_reader = with_response_body
      let discard = discard_response_body
    end

    let with_response = do_with_response
  end

  type t = conn
end
