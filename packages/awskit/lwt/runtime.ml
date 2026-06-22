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
    random_float : upper_bound:float -> float;
    timeout_policy : Awskit.Timeout.policy;
    max_response_drain_bytes : int;
  }

  type request_body_writer = {
    push : string -> unit Lwt.t;
    close : unit -> unit;
    cancelled : unit Lwt.t;
    remaining : int64 option ref;
    mutable write_error : Awskit.Error.t option;
  }

  type request_body_bridge = {
    body : Cohttp_lwt.Body.t;
    finished : (unit, Awskit.Error.t) Result.t Lwt.t;
    cancel : unit -> unit;
  }

  type request_body =
    | Body of Awskit.Body.Request.descriptor * Cohttp_lwt.Body.t
    | Stream of
        Awskit.Body.Request.descriptor
        * (request_body_writer -> (unit, Awskit.Error.t) Result.t Lwt.t)

  type response_body = {
    body : Cohttp_lwt.Body.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    timeout_policy : Awskit.Timeout.policy;
    max_response_drain_bytes : int;
  }

  type response_body_reader = {
    stream : string Lwt_stream.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    timeout_policy : Awskit.Timeout.policy;
    mutable chunk : string;
    mutable offset : int;
  }

  let validate_create_args ?endpoint ~max_response_drain_bytes () =
    if max_response_drain_bytes <= 0 then
      invalid_arg
        "Awskit_lwt.Make.create: max_response_drain_bytes must be positive";
    Option.iter endpoint ~f:(fun endpoint ->
        ignore (Awskit.Endpoint.to_url_prefix endpoint))

  let parse_endpoint = function
    | None -> Ok None
    | Some endpoint ->
        Result.map (Awskit.Endpoint.of_string endpoint) ~f:Option.some

  let retry_capability_error ~field =
    Awskit.Error.Internal.validation ~field
      "enabled retries require an explicit runtime sleep and random capability"

  let validate_retry_capabilities ~retry_policy ~sleep ~random_float =
    if Awskit.Retry.max_attempts retry_policy <= 1 then Ok ()
    else
      match (sleep, random_float) with
      | Some _, Some _ -> Ok ()
      | None, _ -> Error (retry_capability_error ~field:"sleep")
      | Some _, None -> Error (retry_capability_error ~field:"random_float")

  let timeout_capability_error =
    Awskit.Error.Internal.validation ~field:"timeout"
      "enabled timeouts require an explicit runtime sleep capability"

  let timeout_policy_has_spans policy =
    List.exists
      [
        Awskit.Timeout.span policy `Connect;
        Awskit.Timeout.span policy `Attempt;
        Awskit.Timeout.span policy `Operation;
        Awskit.Timeout.span policy `Request_body;
        Awskit.Timeout.span policy `Response_body;
        Awskit.Timeout.span policy `Drain;
      ]
      ~f:Option.is_some

  let validate_timeout_capability ~timeout_policy ~sleep =
    if timeout_policy_has_spans timeout_policy && Option.is_none sleep then
      Error timeout_capability_error
    else Ok ()

  let create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider ~clock ?retry_policy ?sleep ?random_float
      ?timeout_policy
      ?(max_response_drain_bytes = default_max_response_drain_bytes) () =
    let retry_policy =
      Option.value retry_policy ~default:Awskit.Retry.default
    in
    let timeout_policy =
      match timeout_policy with
      | Some timeout_policy -> timeout_policy
      | None ->
          if Option.is_some sleep then Awskit.Timeout.default
          else Awskit.Timeout.disabled
    in
    match (Awskit.Region.of_string region, parse_endpoint endpoint) with
    | Error error, _ | _, Error error -> Error error
    | Ok region, Ok endpoint -> (
        validate_create_args ?endpoint ~max_response_drain_bytes ();
        match
          validate_retry_capabilities ~retry_policy ~sleep ~random_float
        with
        | Error _ as error -> error
        | Ok () -> (
            match validate_timeout_capability ~timeout_policy ~sleep with
            | Error _ as error -> error
            | Ok () ->
                let sleep =
                  Option.value sleep ~default:(fun _ -> Lwt.return_unit)
                in
                let random_float =
                  Option.value random_float ~default:(fun ~upper_bound:_ -> 0.0)
                in
                Ok
                  {
                    ctx;
                    endpoint;
                    region;
                    credentials_provider;
                    clock;
                    retry_policy;
                    sleep;
                    random_float;
                    timeout_policy;
                    max_response_drain_bytes;
                  }))

  let create ?ctx ?endpoint ~region ~credentials ~clock ?retry_policy ?sleep
      ?random_float ?timeout_policy ?max_response_drain_bytes () =
    create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider:(fun () -> Lwt.return_ok credentials)
      ~clock ?retry_policy ?sleep ?random_float ?timeout_policy
      ?max_response_drain_bytes ()

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

  let body_error message = Awskit.Error.Internal.body message

  let timeout_phase_name = function
    | `Connect -> "connect"
    | `Attempt -> "attempt"
    | `Operation -> "operation"
    | `Request_body -> "request body"
    | `Response_body -> "response body"
    | `Drain -> "response drain"

  let timeout_error phase span =
    let phase_name = timeout_phase_name phase in
    Awskit.Error.Internal.timeout ~operation:phase_name
      (Fmt.str "%s timed out after %.3fs" phase_name
         (Ptime.Span.to_float_s span))

  let with_timeout_result ~sleep timeout_policy phase promise =
    match Awskit.Timeout.span timeout_policy phase with
    | None -> promise
    | Some span ->
        Lwt.pick
          [
            promise;
            Lwt.bind (sleep span) (fun () ->
                Lwt.return_error (timeout_error phase span));
          ]

  let writer_for descriptor ~push ~close ~cancelled =
    {
      push;
      close;
      cancelled;
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
            Lwt.catch
              (fun () ->
                Lwt.bind
                  (Lwt.pick
                     [
                       writer.push string;
                       Lwt.bind writer.cancelled (fun () ->
                           Lwt.fail Lwt.Canceled);
                     ])
                  (fun () -> Lwt.return_ok ()))
              (function
                | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                | Lwt_stream.Closed ->
                    let error = body_error "request body stream closed" in
                    writer.write_error <- Some error;
                    Lwt.return_error error
                | exn ->
                    let error = body_error (Exn.to_string exn) in
                    writer.write_error <- Some error;
                    Lwt.return_error error))

  let body_to_cohttp (conn : conn) = function
    | Body (_, body) ->
        { body; finished = Lwt.return_ok (); cancel = (fun () -> ()) }
    | Stream (descriptor, write) ->
        let stream, push = Lwt_stream.create_bounded 16 in
        let finished, wake_finished = Lwt.wait () in
        let wake_finished_once =
          let woken = ref false in
          fun wake ->
            if not !woken then (
              woken := true;
              wake ())
        in
        let wake_finished_result_once result =
          wake_finished_once (fun () -> Lwt.wakeup_later wake_finished result)
        in
        let wake_finished_exn_once exn =
          wake_finished_once (fun () -> Lwt.wakeup_later_exn wake_finished exn)
        in
        let cancel_requested = ref false in
        let cancelled, wake_cancelled = Lwt.wait () in
        let wake_cancelled_once =
          let woken = ref false in
          fun () ->
            if not !woken then (
              woken := true;
              Lwt.wakeup_later wake_cancelled ())
        in
        let writer =
          writer_for descriptor
            ~push:(fun chunk -> push#push chunk)
            ~close:(fun () -> push#close)
            ~cancelled
        in
        let producer =
          Lwt.catch
            (fun () ->
              let write_promise = write writer in
              let write_with_timeout =
                match Awskit.Timeout.span conn.timeout_policy `Request_body with
                | None -> write_promise
                | Some span ->
                    Lwt.pick
                      [
                        write_promise;
                        Lwt.bind (conn.sleep span) (fun () ->
                            Lwt.cancel write_promise;
                            Lwt.return_error (timeout_error `Request_body span));
                      ]
              in
              let write_or_cancelled =
                Lwt.pick
                  [
                    write_with_timeout;
                    Lwt.bind cancelled (fun () ->
                        Lwt.cancel write_promise;
                        Lwt.fail Lwt.Canceled);
                  ]
              in
              Lwt.bind write_or_cancelled (function
                | Ok () ->
                    let result = check_finished_length writer in
                    writer.close ();
                    wake_finished_result_once result;
                    Lwt.return_unit
                | Error error ->
                    Log.warn (fun m ->
                        m "request body stream failed: %s"
                          (Awskit.Error.to_string_hum error));
                    writer.close ();
                    wake_finished_result_once (Error error);
                    Lwt.return_unit))
            (fun exn ->
              writer.close ();
              match exn with
              | Lwt.Canceled when !cancel_requested ->
                  wake_finished_result_once
                    (Error (body_error "request body stream canceled"));
                  Lwt.return_unit
              | Lwt.Canceled ->
                  wake_finished_exn_once Lwt.Canceled;
                  Lwt.return_unit
              | exn ->
                  let error = body_error (Exn.to_string exn) in
                  Log.warn (fun m ->
                      m "request body stream raised: %s"
                        (Awskit.Error.to_string_hum error));
                  wake_finished_result_once (Error error);
                  Lwt.return_unit)
        in
        Lwt.async (fun () -> producer);
        {
          body = Cohttp_lwt.Body.of_stream stream;
          finished;
          cancel =
            (fun () ->
              cancel_requested := true;
              writer.close ();
              wake_cancelled_once ();
              Lwt.cancel producer);
        }

  (* HTTP call *)

  let do_with_response (conn : conn) (request : Awskit.Request.t) request_body
      ~f =
    let uri = make_uri request in
    let headers = Cohttp.Header.of_list request.headers in
    let bridge = body_to_cohttp conn request_body in
    let with_transport_timeout phase promise =
      match Awskit.Timeout.span conn.timeout_policy phase with
      | None -> promise
      | Some span ->
          Lwt.pick
            [
              promise;
              Lwt.bind (conn.sleep span) (fun () ->
                  bridge.cancel ();
                  Lwt.return_error (timeout_error phase span));
            ]
    in
    let meth = to_cohttp_meth request.method_ in
    let successful_status status = status >= 200 && status < 300 in
    let make_response_body body =
      {
        body;
        sleep = conn.sleep;
        timeout_policy = conn.timeout_policy;
        max_response_drain_bytes = conn.max_response_drain_bytes;
      }
    in
    let exception Callback_raised of exn in
    let ready_request_body_result () =
      match Lwt.state bridge.finished with
      | Lwt.Return result -> Some result
      | Lwt.Fail Lwt.Canceled -> raise Lwt.Canceled
      | Lwt.Fail exn -> Some (Error (body_error (Exn.to_string exn)))
      | Lwt.Sleep -> None
    in
    let call_f response response_body =
      Log.debug (fun m -> m "HTTP %d" (Awskit.Response.status response));
      Lwt.catch
        (fun () -> f response response_body)
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn -> Lwt.fail (Callback_raised exn))
    in
    let response =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (with_transport_timeout `Connect
               (Lwt.map
                  (fun response -> Ok response)
                  (Client.call ?ctx:conn.ctx ~headers ~body:bridge.body
                     ~chunked:false meth uri)))
            (function
              | Error error -> Lwt.return_error error
              | Ok (response, response_body) -> (
                  let status =
                    Cohttp.Response.status response
                    |> Cohttp.Code.code_of_status
                  in
                  let response = to_aws_response response in
                  let response_body = make_response_body response_body in
                  if successful_status status then
                    Lwt.bind bridge.finished (function
                      | Error error -> Lwt.return_error error
                      | Ok () -> call_f response response_body)
                  else
                    match ready_request_body_result () with
                    | Some (Error error) -> Lwt.return_error error
                    | Some (Ok ()) -> call_f response response_body
                    | None ->
                        Lwt.finalize
                          (fun () -> call_f response response_body)
                          (fun () ->
                            bridge.cancel ();
                            Lwt.return_unit))))
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | Callback_raised exn -> Lwt.fail exn
          | exn ->
              let message = Exn.to_string exn in
              Log.warn (fun m -> m "HTTP call failed: %s" message);
              Lwt.return_error
                (Awskit.Error.Internal.transport ~retryable:true message))
    in
    response
    |> with_transport_timeout `Attempt
    |> with_transport_timeout `Operation
    |> fun response ->
    Lwt.bind response (function
      | Error error when Awskit.Error.is_timeout error ->
          bridge.cancel ();
          Lwt.return_error error
      | result -> Lwt.return result)

  (* Module satisfying Awskit.Runtime.S *)

  module Runtime = struct
    type +'a t = 'a Lwt.t
    type connection = conn
    type nonrec request_body = request_body
    type nonrec response_body = response_body
    type nonrec request_body_writer = request_body_writer
    type nonrec response_body_reader = response_body_reader

    module IO = struct
      type +'a t = 'a Lwt.t

      let return = Lwt.return
      let bind = Lwt.bind
    end

    let empty_request_body = empty_request_body
    let string_request_body = string_request_body
    let bytes_request_body = bytes_request_body
    let stream_request_body = stream_request_body
    let request_body_descriptor = request_body_descriptor
    let write_request_body_string = write_request_body_string

    module Request_body = struct
      type 'a io = 'a Lwt.t
      type t = request_body
      type writer = request_body_writer

      let empty = empty_request_body
      let of_string = string_request_body
      let of_bytes = bytes_request_body
      let of_stream = stream_request_body
      let descriptor = request_body_descriptor
      let content_length body = (request_body_descriptor body).content_length
      let write_string = write_request_body_string

      let write_bytes writer bytes =
        write_request_body_string writer (Bytes.to_string bytes)
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
        Lwt.return_error (Awskit.Error.Internal.body "invalid read bounds")
      else
        with_timeout_result ~sleep:reader.sleep reader.timeout_policy
          `Response_body
          (Lwt.catch
             (fun () -> read_from_current reader bytes ~off ~len)
             (function
               | Lwt.Canceled -> Lwt.fail Lwt.Canceled
               | exn ->
                   Lwt.return_error
                     (Awskit.Error.Internal.body (Exn.to_string exn))))

    let next_response_body ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Lwt.return_error
          (Awskit.Error.Internal.body "chunk_size must be positive")
      else
        let buffer = Bytes.create chunk_size in
        Lwt.bind (read_response_body reader buffer ~off:0 ~len:chunk_size)
          (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok None
          | Ok n -> Lwt.return_ok (Some (Bytes.sub buffer ~pos:0 ~len:n)))

    let drain_limit_error max_response_drain_bytes =
      Awskit.Error.Internal.body
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

    let drain_response_body_reader reader (body : response_body) =
      with_timeout_result ~sleep:body.sleep body.timeout_policy `Drain
        (drain_reader reader ~remaining:body.max_response_drain_bytes
           ~max_response_drain_bytes:body.max_response_drain_bytes)

    let drain_response_body_after_exception reader body exn =
      let drain =
        Lwt.catch
          (fun () ->
            Lwt.bind (drain_response_body_reader reader body) (fun _ ->
                Lwt.return_unit))
          (fun _ -> Lwt.return_unit)
      in
      Lwt.bind (Lwt.protected drain) (fun () -> Lwt.fail exn)

    let with_response_body body ~consume =
      let reader =
        {
          stream = Cohttp_lwt.Body.to_stream body.body;
          sleep = body.sleep;
          timeout_policy = body.timeout_policy;
          chunk = "";
          offset = 0;
        }
      in
      Lwt.catch
        (fun () ->
          Lwt.bind (consume reader) (fun result ->
              match result with
              | Ok _ ->
                  Lwt.bind (drain_response_body_reader reader body) (function
                    | Ok () -> Lwt.return result
                    | Error error -> Lwt.return_error error)
              | Error _ ->
                  Lwt.bind (drain_response_body_reader reader body) (fun _ ->
                      Lwt.return result)))
        (drain_response_body_after_exception reader body)

    let discard_response_body body =
      let reader =
        {
          stream = Cohttp_lwt.Body.to_stream body.body;
          sleep = body.sleep;
          timeout_policy = body.timeout_policy;
          chunk = "";
          offset = 0;
        }
      in
      drain_response_body_reader reader body

    module Response_body = struct
      type 'a io = 'a Lwt.t
      type t = response_body
      type reader = response_body_reader

      let read = read_response_body
      let next = next_response_body
      let with_reader = with_response_body
      let discard = discard_response_body
    end

    module Transport = struct
      type +'a io = 'a Lwt.t
      type connection = conn
      type nonrec request_body = request_body
      type nonrec response_body = response_body

      let with_response conn request ~body ~consume =
        do_with_response conn request body ~f:consume
    end

    module Clock = struct
      type connection = conn

      let now c = c.clock ()
    end

    module Sleeper = struct
      type +'a io = 'a Lwt.t
      type connection = conn

      let sleep (c : conn) span = c.sleep span
    end

    module Random = struct
      type connection = conn

      let float c ~upper_bound = c.random_float ~upper_bound
    end

    module Credentials = struct
      type +'a io = 'a Lwt.t
      type connection = conn

      let resolve c = c.credentials_provider ()
    end

    module Endpoint = struct
      type connection = conn

      let region c = c.region
      let endpoint c = c.endpoint
    end

    module Retry = struct
      type connection = conn

      let policy c = c.retry_policy
    end

    module Timeout = struct
      type connection = conn

      let policy (c : conn) = c.timeout_policy
    end
  end

  type t = conn
end
