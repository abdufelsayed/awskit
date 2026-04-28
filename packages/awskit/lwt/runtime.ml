open Base

let src = Logs.Src.create "awskit-lwt" ~doc:"AWS Lwt HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Client : Cohttp_lwt.S.Client) = struct
  let default_max_response_body_bytes = 64 * 1024 * 1024

  type conn = {
    ctx : Client.ctx option;
    endpoint : Awskit.Endpoint.t option;
    region : Awskit.Region.t;
    credentials : Awskit.Credentials.t;
    clock : unit -> Ptime.t;
    retry_policy : Awskit.Retry.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    max_response_body_bytes : int;
  }

  type upload_writer = { push : string option -> unit }

  type upload_body =
    | Body of Awskit.Body.Upload.descriptor * Cohttp_lwt.Body.t
    | Stream of
        Awskit.Body.Upload.descriptor
        * (upload_writer -> (unit, Awskit.Error.t) Result.t Lwt.t)

  type download_body = Cohttp_lwt.Body.t

  type download_reader = {
    stream : string Lwt_stream.t;
    mutable chunk : string;
    mutable offset : int;
  }

  let validate_create_args ?endpoint ~max_response_body_bytes () =
    if max_response_body_bytes <= 0 then
      invalid_arg
        "Awskit_lwt.Make.create: max_response_body_bytes must be positive";
    Option.iter endpoint ~f:(fun endpoint ->
        ignore (Awskit.Endpoint.to_url_prefix endpoint))

  let create ?ctx ?endpoint ~region ~credentials ~clock
      ?(retry_policy = Awskit.Retry.default) ?(sleep = fun _ -> Lwt.return_unit)
      ?(max_response_body_bytes = default_max_response_body_bytes) () =
    validate_create_args ?endpoint ~max_response_body_bytes ();
    {
      ctx;
      endpoint;
      region;
      credentials;
      clock;
      retry_policy;
      sleep;
      max_response_body_bytes;
    }

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
      Awskit.Body.Upload.content_length =
        Some (String.length body |> Int64.of_int);
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let empty_body = Body (descriptor_for_string "", Cohttp_lwt.Body.empty)

  let string_body body =
    Body (descriptor_for_string body, Cohttp_lwt.Body.of_string body)

  let bytes_body body =
    let body = Bytes.to_string body in
    string_body body

  let stream_body descriptor ~write = Stream (descriptor, write)

  let upload_descriptor = function
    | Body (descriptor, _) -> descriptor
    | Stream (descriptor, _) -> descriptor

  let write_string writer string =
    writer.push (Some string);
    Lwt.return_ok ()

  let body_to_cohttp = function
    | Body (_, body) -> body
    | Stream (_, write) ->
        let stream, push = Lwt_stream.create () in
        let writer = { push } in
        Lwt.async (fun () ->
            Lwt.bind (write writer) (function
              | Ok () ->
                  push None;
                  Lwt.return_unit
              | Error error ->
                  Log.warn (fun m ->
                      m "upload stream failed: %s"
                        (Awskit.Error.to_string_hum error));
                  push None;
                  Lwt.return_unit));
        Cohttp_lwt.Body.of_stream stream

  (* HTTP call *)

  let do_call (conn : conn) (request : Awskit.Request.t) upload_body =
    let uri = make_uri request in
    let headers = Cohttp.Header.of_list request.headers in
    let body = body_to_cohttp upload_body in
    let meth = to_cohttp_meth request.method_ in
    Lwt.catch
      (fun () ->
        Lwt.bind
          (Client.call ?ctx:conn.ctx ~headers ~body ~chunked:false meth uri)
          (fun (response, response_body) ->
            Log.debug (fun m ->
                m "HTTP %d"
                  (Cohttp.Response.status response |> Cohttp.Code.code_of_status));
            Lwt.return_ok (to_aws_response response, response_body)))
      (fun exn ->
        let message = Exn.to_string exn in
        Log.warn (fun m -> m "HTTP call failed: %s" message);
        Lwt.return_error (Awskit.Error.transport ~retryable:false message))

  (* Module satisfying Awskit.Runtime.S *)

  module Runtime = struct
    type +'a t = 'a Lwt.t

    let return = Lwt.return
    let bind = Lwt.bind

    type connection = conn
    type nonrec upload_body = upload_body
    type nonrec download_body = download_body
    type nonrec upload_writer = upload_writer
    type nonrec download_reader = download_reader

    let now c = c.clock ()
    let region c = c.region
    let credentials c = Lwt.return_ok c.credentials
    let endpoint c = c.endpoint
    let retry_policy c = c.retry_policy
    let sleep c span = c.sleep span
    let empty_body = empty_body
    let string_body = string_body
    let bytes_body = bytes_body
    let stream_body = stream_body
    let upload_descriptor = upload_descriptor
    let write_string = write_string

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

    let read reader bytes ~off ~len =
      Lwt.catch
        (fun () -> read_from_current reader bytes ~off ~len)
        (fun exn -> Lwt.return_error (Awskit.Error.body (Exn.to_string exn)))

    let rec drain_reader reader =
      let buffer = Bytes.create 8192 in
      Lwt.bind
        (read reader buffer ~off:0 ~len:(Bytes.length buffer))
        (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok ()
          | Ok _ -> drain_reader reader)

    let with_download_body body ~consume =
      let reader =
        { stream = Cohttp_lwt.Body.to_stream body; chunk = ""; offset = 0 }
      in
      Lwt.bind (consume reader) (fun result ->
          Lwt.bind (drain_reader reader) (function
            | Ok () -> Lwt.return result
            | Error error -> Lwt.return_error error))

    let discard_download_body body =
      with_download_body body ~consume:(fun reader -> drain_reader reader)

    let call = do_call
  end

  type t = conn
end
