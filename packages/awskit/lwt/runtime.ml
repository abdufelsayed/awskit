open Base
module Http_observation = Awskit.Observability.For_runtime.Http

module type CONNECTOR = sig
  module Client : Cohttp_lwt.S.Client

  type call
  (** A connector-owned in-flight HTTP call. The handle must remain valid from
      [call] initiation until [abort] is complete, including while response
      headers are still pending. *)

  val call :
    ?ctx:Client.ctx ->
    headers:Cohttp.Header.t ->
    body:Cohttp_lwt.Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    call

  val response : call -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  val abort : call -> unit Lwt.t
  (** Close or otherwise abandon the in-flight call. This operation is
      idempotent and must complete before the runtime releases a cancellation or
      cleanup result. *)
end

module Make_with_connector (Connector : CONNECTOR) = struct
  module Client = Connector.Client

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
    observability : Observer.t;
  }

  type request_body_writer = {
    push : string -> unit Lwt.t;
    close : unit -> unit;
    cancelled : unit Lwt.t;
    streaming : Observer.lease;
    streaming_active : bool ref;
    produced_bytes : int64 ref;
    remaining : int64 option ref;
    mutable write_error : Awskit.Error.t option;
  }

  type attempt_stats = {
    connector_request_bytes : int64 option ref;
    connector_response_bytes : int64 ref;
    connector_drained_bytes : int64 ref;
    draining : bool ref;
    streaming_active : bool ref;
    request_streaming : Observer.lease;
    response_streaming : Observer.lease;
  }

  type request_body_bridge = {
    body : Cohttp_lwt.Body.t;
    finished : (unit, Awskit.Error.t) Result.t Lwt.t;
    settled : unit Lwt.t;
    cancel : unit -> unit;
  }

  type request_body =
    | Body of Awskit.Body.Request.descriptor * Cohttp_lwt.Body.t
    | Stream of
        Awskit.Body.Request.descriptor
        * (request_body_writer -> (unit, Awskit.Error.t) Result.t Lwt.t)

  type response_body_framing =
    | Response_unknown
    | Response_content_length of int64
    | Response_chunked

  type response_body = {
    body : Cohttp_lwt.Body.t;
    method_ : Awskit.Request.Method.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    timeout_policy : Awskit.Timeout.policy;
    max_response_drain_bytes : int;
    framing : response_body_framing;
    bodiless : bool;
    stats : attempt_stats;
    observability : Observer.t;
    abort : unit -> unit Lwt.t;
    cleanup : response_cleanup;
  }

  and response_cleanup = {
    mutable promise : (unit, Awskit.Error.t) Result.t Lwt.t option;
    mutable cancel : unit -> unit;
  }

  type response_body_reader = {
    stream : string Lwt_stream.t;
    sleep : Ptime.Span.t -> unit Lwt.t;
    timeout_policy : Awskit.Timeout.policy;
    bodiless : bool;
    mutable active : bool;
    mutable chunk : string;
    mutable offset : int;
    mutable eof : bool;
    mutable remaining : int64 option;
    stats : attempt_stats;
  }

  let validate_create_args ?endpoint ~max_response_drain_bytes () =
    if max_response_drain_bytes <= 0 then
      Error
        (Awskit.Error.Producer.validation ~field:"max_response_drain_bytes"
           "max_response_drain_bytes must be positive")
    else (
      Option.iter endpoint ~f:(fun endpoint ->
          ignore (Awskit.Endpoint.to_url_prefix endpoint));
      Ok ())

  let parse_endpoint = function
    | None -> Ok None
    | Some endpoint ->
        Result.map (Awskit.Endpoint.of_string endpoint) ~f:Option.some

  let retry_capability_error ~field =
    Awskit.Error.Producer.validation ~field
      "enabled retries require an explicit runtime sleep and random capability"

  let validate_retry_capabilities ~retry_policy ~sleep ~random_float =
    if Awskit.Retry.max_attempts retry_policy <= 1 then Ok ()
    else
      match (sleep, random_float) with
      | Some _, Some _ -> Ok ()
      | None, _ -> Error (retry_capability_error ~field:"sleep")
      | Some _, None -> Error (retry_capability_error ~field:"random_float")

  let timeout_capability_error =
    Awskit.Error.Producer.validation ~field:"timeout"
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
      ?(max_response_drain_bytes = default_max_response_drain_bytes)
      ?(observability = Observer.default ()) () =
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
        match validate_create_args ?endpoint ~max_response_drain_bytes () with
        | Error _ as error -> error
        | Ok () -> (
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
                      Option.value random_float ~default:(fun ~upper_bound:_ ->
                          0.0)
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
                        observability;
                      })))

  let create ?ctx ?endpoint ~region ~credentials ~clock ?retry_policy ?sleep
      ?random_float ?timeout_policy ?max_response_drain_bytes ?observability ()
      =
    create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider:(fun () -> Lwt.return_ok credentials)
      ~clock ?retry_policy ?sleep ?random_float ?timeout_policy
      ?max_response_drain_bytes ?observability ()

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

  let to_cohttp_meth = function
    | `GET -> `GET
    | `PUT -> `PUT
    | `POST -> `POST
    | `DELETE -> `DELETE
    | `HEAD -> `HEAD
    | `PATCH -> `PATCH

  let to_aws_response http_response =
    Awskit.Response.create_exn
      ~status:
        (Cohttp.Response.status http_response |> Cohttp.Code.code_of_status)
      ~headers:(Cohttp.Response.headers http_response |> Cohttp.Header.to_list)
      ()

  let descriptor_for_string body =
    Awskit.Body.Request.descriptor_exn
      ~content_length:(String.length body |> Int64.of_int)
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
      ~replayable:true ()

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

  let body_error message = Awskit.Error.Producer.body message
  let int64_equal left right = Int64.compare left right = 0

  let split_header_values values =
    values
    |> List.concat_map ~f:(fun value -> String.split value ~on:',')
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun value -> not (String.is_empty value))

  let split_content_length_values values =
    values
    |> List.concat_map ~f:(fun value -> String.split value ~on:',')
    |> List.map ~f:String.strip

  let ascii_digits value =
    (not (String.is_empty value))
    && String.for_all value ~f:(function '0' .. '9' -> true | _ -> false)

  let parse_content_length_value value =
    if ascii_digits value then
      match Int64.of_string_opt value with
      | Some length -> Ok length
      | None -> Error (body_error "invalid response Content-Length header")
    else Error (body_error "invalid response Content-Length header")

  let rec parse_content_lengths = function
    | [] -> Ok []
    | value :: values -> (
        match parse_content_length_value value with
        | Error _ as error -> error
        | Ok length ->
            Result.map (parse_content_lengths values) ~f:(fun lengths ->
                length :: lengths))

  let content_length_from_headers values =
    match values with
    | [] -> Ok None
    | _ -> (
        match split_content_length_values values with
        | [] -> Ok None
        | values -> (
            match parse_content_lengths values with
            | Error _ as error -> error
            | Ok [] -> Ok None
            | Ok (length :: lengths) ->
                if List.for_all lengths ~f:(int64_equal length) then
                  Ok (Some length)
                else
                  Error
                    (body_error
                       "conflicting response Content-Length header values")))

  let has_chunked_transfer_encoding values =
    split_header_values values
    |> List.exists ~f:(fun value ->
        String.equal (String.lowercase value) "chunked")

  let response_body_framing headers =
    let transfer_encoding_values =
      split_header_values (Cohttp.Header.get_multi headers "transfer-encoding")
    in
    let has_transfer_encoding = not (List.is_empty transfer_encoding_values) in
    let transfer_encoding_chunked =
      has_chunked_transfer_encoding transfer_encoding_values
    in
    match
      content_length_from_headers
        (Cohttp.Header.get_multi headers "content-length")
    with
    | Error _ as error -> error
    | Ok (Some _) when has_transfer_encoding ->
        Error
          (body_error "response has both Transfer-Encoding and Content-Length")
    | Ok (Some content_length) -> Ok (Response_content_length content_length)
    | Ok None when transfer_encoding_chunked -> Ok Response_chunked
    | Ok None -> Ok Response_unknown

  let validate_bodiless_response_headers headers =
    Result.map
      (content_length_from_headers
         (Cohttp.Header.get_multi headers "content-length"))
      ~f:(fun _ -> ())

  let timeout_phase_name = function
    | `Connect -> "connect"
    | `Attempt -> "attempt"
    | `Operation -> "operation"
    | `Request_body -> "request body"
    | `Response_body -> "response body"
    | `Drain -> "response drain"

  let timeout_error phase span =
    let phase_name = timeout_phase_name phase in
    Awskit.Error.Producer.timeout ~operation:phase_name
      (Fmt.str "%s timed out after %.3fs" phase_name
         (Ptime.Span.to_float_s span))

  let with_timeout_value ?cancel ?(on_timeout = fun () -> ()) ~sleep
      timeout_policy phase promise =
    let cancel_promise = Option.value cancel ~default:promise in
    match Awskit.Timeout.span timeout_policy phase with
    | None -> Lwt.map (fun value -> Ok value) promise
    | Some span ->
        let timeout =
          Lwt.bind (sleep span) (fun () ->
              Lwt.return (`Timeout (timeout_error phase span)))
        in
        let result = Lwt.map (fun value -> `Result value) promise in
        Lwt.bind
          (Lwt.pick [ result; timeout ])
          (function
            | `Result value ->
                Lwt.cancel timeout;
                Lwt.return_ok value
            | `Timeout error ->
                on_timeout ();
                Lwt.cancel cancel_promise;
                Lwt.return_error error)

  let with_timeout_result ?cancel ?on_timeout ~sleep timeout_policy phase
      promise =
    match Awskit.Timeout.span timeout_policy phase with
    | None -> promise
    | Some _ ->
        Lwt.bind
          (with_timeout_value ?cancel ?on_timeout ~sleep timeout_policy phase
             promise) (function
          | Ok result -> Lwt.return result
          | Error error -> Lwt.return_error error)

  let observe_result_phase observability definition ~method_ f =
    Observer.with_operation observability ~operation:definition
      ~start:(fun () -> Http_observation.phase_start ~method_)
      f

  let writer_for descriptor ~push ~close ~cancelled ~streaming ~streaming_active
      =
    {
      push;
      close;
      cancelled;
      streaming;
      streaming_active;
      produced_bytes = ref 0L;
      remaining = ref descriptor.Awskit.Body.Request.content_length;
      write_error = None;
    }

  let check_write_length (writer : request_body_writer) length =
    match !(writer.remaining) with
    | None -> Ok ()
    | Some remaining ->
        let length64 = Int64.of_int length in
        if Int64.compare length64 remaining > 0 then
          Error (body_error "request body exceeded declared content_length")
        else (
          writer.remaining := Some Int64.(remaining - length64);
          Ok ())

  let invalid_write_bounds bytes ~off ~len =
    off < 0 || len < 0 || len > Bytes.length bytes - off

  let check_finished_length (writer : request_body_writer) =
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match !(writer.remaining) with
        | None | Some 0L -> Ok ()
        | Some _ ->
            Error
              (body_error "request body ended before declared content_length"))

  let push_request_body (writer : request_body_writer) chunk =
    let length = Int64.of_int (String.length chunk) in
    if !(writer.streaming_active) then Observer.add writer.streaming length;
    Lwt.try_bind
      (fun () -> writer.push chunk)
      (fun () ->
        (writer.produced_bytes := Int64.(!(writer.produced_bytes) + length));
        Lwt.return_unit)
      (fun exn ->
        if !(writer.streaming_active) then
          Observer.add writer.streaming Int64.(neg length);
        Lwt.fail exn)

  let write_request_body_string writer string =
    match writer.write_error with
    | Some error -> Lwt.return_error error
    | None -> (
        match check_write_length writer (String.length string) with
        | Error error ->
            writer.write_error <- Some error;
            Lwt.return_error error
        | Ok () ->
            Lwt.catch
              (fun () ->
                Lwt.bind
                  (Lwt.pick
                     [
                       push_request_body writer string;
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

  let write_request_body_subbytes writer bytes ~off ~len =
    if invalid_write_bounds bytes ~off ~len then
      Lwt.return_error (body_error "invalid write bounds")
    else
      match writer.write_error with
      | Some error -> Lwt.return_error error
      | None -> (
          match check_write_length writer len with
          | Error error ->
              writer.write_error <- Some error;
              Lwt.return_error error
          | Ok () ->
              Lwt.catch
                (fun () ->
                  Lwt.bind
                    (Lwt.pick
                       [
                         push_request_body writer
                           (Bytes.sub bytes ~pos:off ~len |> Bytes.to_string);
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

  let count_request_body stats body ~on_settled =
    let stream = Cohttp_lwt.Body.to_stream body in
    let stream =
      Lwt_stream.from (fun () ->
          Lwt.try_bind
            (fun () -> Lwt_stream.get stream)
            (function
              | None ->
                  on_settled ();
                  Lwt.return_none
              | Some chunk ->
                  if !(stats.streaming_active) then begin
                    let length = Int64.of_int (String.length chunk) in
                    let connector_request_bytes =
                      Option.value !(stats.connector_request_bytes) ~default:0L
                    in
                    stats.connector_request_bytes :=
                      Some Int64.(connector_request_bytes + length);
                    Observer.add stats.request_streaming Int64.(neg length)
                  end;
                  Lwt.return_some chunk)
            (fun exn ->
              on_settled ();
              Lwt.fail exn))
    in
    Cohttp_lwt.Body.of_stream stream

  let drain_limit_error max_response_drain_bytes =
    Awskit.Error.Producer.body
      ~limit:(Int64.of_int max_response_drain_bytes)
      "response body exceeded max_response_drain_bytes"

  let await_connector_abort abort =
    Lwt.no_cancel (Lwt.catch (fun () -> abort ()) (fun _ -> Lwt.return_unit))

  let response_cleanup () = { promise = None; cancel = (fun () -> ()) }

  (* Raw-response cleanup and scoped body cleanup operate on the same stream.
     Keep one promise for that physical response so an exception that escapes a
     body helper can join the already-completed drain instead of pulling again. *)
  let run_response_cleanup cleanup f =
    match cleanup.promise with
    | Some promise -> Lwt.protected promise
    | None ->
        let promise = Lwt.catch f (fun exn -> Lwt.fail exn) in
        cleanup.promise <- Some promise;
        cleanup.cancel <- (fun () -> Lwt.cancel promise);
        let joined = Lwt.protected promise in
        Lwt.on_cancel joined cleanup.cancel;
        joined

  (* A response can arrive before request production has finished, or before
     framing validation has succeeded. In those paths the response body has
     not been handed to [Response_body], so drain it at the connector boundary
     before returning the original request/framing result. *)
  let drain_raw_response_body (conn : conn) stats ~method_ ~abort ~cleanup body
      =
    run_response_cleanup cleanup (fun () ->
        let stream = Cohttp_lwt.Body.to_stream body in
        let was_draining = !(stats.draining) in
        let drained_before = !(stats.connector_drained_bytes) in
        stats.draining := true;
        let rec loop remaining =
          Lwt.bind (Lwt_stream.get stream) (function
            | None -> Lwt.return_ok ()
            | Some chunk ->
                let length = String.length chunk in
                (* The connector has already delivered this chunk, so count all
                   bytes pulled even when the configured drain limit rejects it. *)
                (stats.connector_drained_bytes :=
                   Int64.(!(stats.connector_drained_bytes) + of_int length));
                if length > remaining then
                  Lwt.return_error
                    (drain_limit_error conn.max_response_drain_bytes)
                else loop (remaining - length))
        in
        let drain =
          Lwt.catch
            (fun () -> loop conn.max_response_drain_bytes)
            (function
              | Lwt.Canceled -> Lwt.fail Lwt.Canceled
              | exn -> Lwt.return_error (body_error (Exn.to_string exn)))
        in
        Lwt.finalize
          (fun () ->
            let result =
              observe_result_phase conn.observability
                (fun () ->
                  Http_observation.response_body_drain ~bytes:(fun () ->
                      Int64.max 0L
                        Int64.(
                          !(stats.connector_drained_bytes) - drained_before)))
                ~method_
                (fun () ->
                  with_timeout_result ~sleep:conn.sleep conn.timeout_policy
                    `Drain drain)
            in
            Lwt.catch
              (fun () ->
                Lwt.bind result (function
                  | Ok () -> Lwt.return_ok ()
                  | Error _ as error ->
                      Lwt.bind (await_connector_abort abort) (fun () ->
                          Lwt.return error)))
              (fun exn ->
                Lwt.bind (await_connector_abort abort) (fun () -> Lwt.fail exn)))
          (fun () ->
            stats.draining := was_draining;
            Lwt.return_unit))

  let body_to_cohttp (conn : conn) stats ~method_ = function
    | Body (_, body) ->
        {
          body;
          finished = Lwt.return_ok ();
          settled = Lwt.return_unit;
          cancel = (fun () -> ());
        }
    | Stream (descriptor, write) ->
        let stream, push = Lwt_stream.create_bounded 16 in
        let connector_settled, wake_connector_settled = Lwt.wait () in
        let connector_settled_once =
          let settled = ref false in
          fun () ->
            if not !settled then (
              settled := true;
              Lwt.wakeup_later wake_connector_settled ())
        in
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
            ~cancelled ~streaming:stats.request_streaming
            ~streaming_active:stats.streaming_active
        in
        let producer =
          Lwt.catch
            (fun () ->
              let write_promise = ref None in
              let write_with_timeout =
                observe_result_phase conn.observability
                  (fun () ->
                    Http_observation.request_body_production ~bytes:(fun () ->
                        !(writer.produced_bytes)))
                  ~method_
                  (fun () ->
                    let promise = write writer in
                    write_promise := Some promise;
                    with_timeout_result ~sleep:conn.sleep conn.timeout_policy
                      `Request_body promise)
              in
              let write_or_cancelled =
                Lwt.pick
                  [
                    write_with_timeout;
                    Lwt.bind cancelled (fun () ->
                        Option.iter !write_promise ~f:Lwt.cancel;
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
              | exn -> (
                  match Awskit.Body.Request.escaped_exn exn with
                  | Some _ ->
                      wake_finished_exn_once exn;
                      Lwt.return_unit
                  | None ->
                      let error = body_error (Exn.to_string exn) in
                      wake_finished_result_once (Error error);
                      Lwt.return_unit))
        in
        Lwt.async (fun () -> producer);
        {
          body =
            count_request_body stats
              (Cohttp_lwt.Body.of_stream stream)
              ~on_settled:connector_settled_once;
          finished;
          settled = Lwt.join [ producer; connector_settled ];
          cancel =
            (fun () ->
              cancel_requested := true;
              writer.close ();
              wake_cancelled_once ();
              connector_settled_once ();
              Lwt.cancel producer);
        }

  let do_with_response_raw (conn : conn) (request : Awskit.Request.t)
      request_body ~stats ~f =
    let uri = make_uri request in
    let headers = Cohttp.Header.of_list request.headers in
    let bridge =
      body_to_cohttp conn stats ~method_:request.method_ request_body
    in
    let with_transport_timeout phase promise =
      with_timeout_result ~sleep:conn.sleep conn.timeout_policy phase promise
    in
    let meth = to_cohttp_meth request.method_ in
    let successful_status status = status >= 200 && status < 300 in
    let is_head = match request.method_ with `HEAD -> true | _ -> false in
    (* A response to HEAD, or a 1xx/204/304 status, has no message body even
       if Content-Length is present; reading it would block until timeout. *)
    let response_is_bodiless status =
      is_head || status = 204 || status = 304 || (status >= 100 && status < 200)
    in
    let make_response_body ~status response body ~abort ~cleanup =
      let bodiless = response_is_bodiless status in
      let headers = Cohttp.Response.headers response in
      let framing =
        if bodiless then
          Result.map (validate_bodiless_response_headers headers) ~f:(fun () ->
              Response_unknown)
        else response_body_framing headers
      in
      match framing with
      | Error _ as error -> error
      | Ok framing ->
          Ok
            {
              body;
              method_ = request.method_;
              sleep = conn.sleep;
              timeout_policy = conn.timeout_policy;
              max_response_drain_bytes = conn.max_response_drain_bytes;
              framing = (if bodiless then Response_unknown else framing);
              bodiless;
              stats;
              observability = conn.observability;
              abort;
              cleanup;
            }
    in
    let exception Callback_raised of exn in
    let ready_request_body_result () =
      match Lwt.state bridge.finished with
      | Lwt.Return result -> Some result
      | Lwt.Fail Lwt.Canceled -> raise Lwt.Canceled
      | Lwt.Fail exn -> (
          match Awskit.Body.Request.escaped_exn exn with
          | Some _ -> raise exn
          | None -> Some (Error (body_error (Exn.to_string exn))))
      | Lwt.Sleep -> None
    in
    let ready_request_body_exception () =
      match ready_request_body_result () with
      | Some _ | None -> None
      | exception Lwt.Canceled -> Some Lwt.Canceled
      | exception exn -> Awskit.Body.Request.escaped_exn exn
    in
    let cancel_unfinished_request_body () =
      bridge.cancel ();
      Lwt.catch (fun () -> bridge.settled) (fun _ -> Lwt.return_unit)
    in
    let call_f response response_body =
      Lwt.catch
        (fun () -> f response response_body)
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn -> Lwt.fail (Callback_raised exn))
    in
    let call_handle : Connector.call option ref = ref None in
    let call_aborted = ref false in
    let abort_call () =
      match !call_handle with
      | None -> Lwt.return_unit
      | Some call ->
          if !call_aborted then Lwt.return_unit
          else (
            call_aborted := true;
            await_connector_abort (fun () -> Connector.abort call))
    in
    let response =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (observe_result_phase conn.observability
               Http_observation.response_headers_wait ~method_:request.method_
               (fun () ->
                 let call =
                   Connector.call ?ctx:conn.ctx ~headers ~body:bridge.body meth
                     uri
                 in
                 call_handle := Some call;
                 (* Keep the connector's promise alive while [abort] closes
                    the physical call. Without this boundary, cancellation
                    can stop at a connector that deliberately ignores
                    [Lwt.cancel], leaving the runtime waiting for headers and
                    never invoking its owned cleanup path. *)
                 let response = Lwt.protected (Connector.response call) in
                 with_timeout_value ~sleep:conn.sleep conn.timeout_policy
                   `Connect response))
            (function
              | Error error ->
                  Lwt.bind (abort_call ()) (fun () -> Lwt.return_error error)
              | Ok (response, response_body) ->
                  let abort = abort_call in
                  let cleanup = response_cleanup () in
                  let status =
                    Cohttp.Response.status response
                    |> Cohttp.Code.code_of_status
                  in
                  let aws_response = to_aws_response response in
                  let fail_after_response_cleanup error =
                    Lwt.bind
                      (drain_raw_response_body conn stats
                         ~method_:request.method_ ~abort ~cleanup response_body)
                      (fun _ -> Lwt.return_error error)
                  in
                  let fail_after_response_cleanup_exn exn =
                    let cleanup =
                      Lwt.catch
                        (fun () ->
                          drain_raw_response_body conn stats
                            ~method_:request.method_ ~abort ~cleanup
                            response_body
                          |> Lwt.protected)
                        (fun _ ->
                          Lwt.return_error
                            (body_error "response cleanup failed"))
                    in
                    Lwt.bind cleanup (fun _ -> Lwt.fail exn)
                  in
                  let make_and_call () =
                    match
                      make_response_body ~status response response_body ~abort
                        ~cleanup
                    with
                    | Error error -> fail_after_response_cleanup error
                    | Ok response_body ->
                        Lwt.bind (call_f aws_response response_body) (function
                          | Ok value -> Lwt.return_ok value
                          | Error error -> fail_after_response_cleanup error)
                  in
                  Lwt.catch
                    (fun () ->
                      if successful_status status then
                        Lwt.bind bridge.finished (function
                          | Error error -> fail_after_response_cleanup error
                          | Ok () -> make_and_call ())
                      else
                        match ready_request_body_result () with
                        | Some (Error error) ->
                            fail_after_response_cleanup error
                        | Some (Ok ()) -> make_and_call ()
                        | None ->
                            Lwt.finalize make_and_call (fun () ->
                                bridge.cancel ();
                                Lwt.return_unit))
                    (fun exn -> fail_after_response_cleanup_exn exn)))
        (fun exn ->
          Lwt.bind (abort_call ()) (fun () ->
              match exn with
              | Lwt.Canceled -> Lwt.fail Lwt.Canceled
              | Callback_raised exn -> Lwt.fail exn
              | exn -> (
                  match Awskit.Body.Request.escaped_exn exn with
                  | Some escaped -> Lwt.fail escaped
                  | None -> (
                      match ready_request_body_exception () with
                      | Some escaped -> Lwt.fail escaped
                      | None ->
                          let message = Exn.to_string exn in
                          let error =
                            Awskit.Error.Producer.transport ~retryable:true
                              message
                          in
                          Lwt.return_error error))))
    in
    Lwt.finalize
      (fun () ->
        response
        |> with_transport_timeout `Attempt
        |> with_transport_timeout `Operation
        |> fun response ->
        Lwt.bind response (function
          | Error error when Awskit.Error.is_timeout error ->
              bridge.cancel ();
              Lwt.return_error error
          | result -> Lwt.return result))
      cancel_unfinished_request_body

  let do_with_response (conn : conn) (request : Awskit.Request.t) request_body
      ~f =
    let descriptor = request_body_descriptor request_body in
    let replayability =
      if descriptor.replayable then Http_observation.Replayable
      else Non_replayable
    in
    Observer.with_instrument conn.observability
      Http_observation.attempts_in_flight
      ~labels:(fun () ->
        Http_observation.request_state ~method_:request.method_)
      1L
      (fun () ->
        let request_streaming =
          Observer.acquire conn.observability
            Http_observation.streaming_bytes_in_flight
            ~labels:(fun () ->
              Http_observation.streaming_state Http_observation.Request)
            0L
        in
        let response_streaming =
          Observer.acquire conn.observability
            Http_observation.streaming_bytes_in_flight
            ~labels:(fun () ->
              Http_observation.streaming_state Http_observation.Response)
            0L
        in
        let response_seen = ref None in
        let stats =
          {
            connector_request_bytes =
              ref
                (match request_body with Body _ -> None | Stream _ -> Some 0L);
            connector_response_bytes = ref 0L;
            connector_drained_bytes = ref 0L;
            draining = ref false;
            streaming_active = ref true;
            request_streaming;
            response_streaming;
          }
        in
        let response () =
          Option.map !response_seen ~f:(fun response ->
              Http_observation.response
                ~status:(Awskit.Response.status response)
                ?request_id:(Awskit.Response.request_id response)
                ?host_id:(Awskit.Response.host_id response)
                ())
        in
        let stats_value () =
          Http_observation.request_stats
            ~connector_request_bytes:!(stats.connector_request_bytes)
            ~connector_response_bytes:!(stats.connector_response_bytes)
            ~connector_drained_bytes:!(stats.connector_drained_bytes)
        in
        Lwt.finalize
          (fun () ->
            Observer.with_operation conn.observability
              ~operation:(fun () ->
                Http_observation.request ~response ~stats:stats_value)
              ~start:(fun () ->
                Http_observation.request_start ~method_:request.method_
                  ~replayability)
              (fun () ->
                do_with_response_raw conn request request_body ~stats
                  ~f:(fun response body ->
                    response_seen := Some response;
                    f response body)))
          (fun () ->
            stats.streaming_active := false;
            Observer.release request_streaming;
            Observer.release response_streaming;
            Lwt.return_unit))

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
    let write_request_body_subbytes = write_request_body_subbytes

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
      let write_subbytes = write_request_body_subbytes

      let write_bytes writer bytes =
        write_request_body_subbytes writer bytes ~off:0
          ~len:(Bytes.length bytes)
    end

    let initial_response_body_remaining = function
      | Response_content_length content_length -> Some content_length
      | Response_unknown | Response_chunked -> None

    let record_response_body_read reader length =
      if not !(reader.stats.streaming_active) then ()
      else
        let length64 = Int64.of_int length in
        (if !(reader.stats.draining) then
           reader.stats.connector_drained_bytes :=
             Int64.(!(reader.stats.connector_drained_bytes) + length64)
         else
           reader.stats.connector_response_bytes :=
             Int64.(!(reader.stats.connector_response_bytes) + length64));
        match reader.remaining with
        | None -> ()
        | Some remaining ->
            reader.remaining <- Some Int64.(remaining - length64)

    let response_body_eof reader =
      match reader.remaining with
      | Some remaining when Int64.compare remaining 0L > 0 ->
          Lwt.return_error
            (body_error "response body ended before declared Content-Length")
      | None | Some _ ->
          reader.eof <- true;
          Lwt.return_ok 0

    let rec read_from_current reader bytes ~off ~len =
      if len = 0 then Lwt.return_ok 0
      else if reader.offset < String.length reader.chunk then begin
        let available = String.length reader.chunk - reader.offset in
        let copied = min available len in
        Bytes.From_string.blit ~src:reader.chunk ~src_pos:reader.offset
          ~dst:bytes ~dst_pos:off ~len:copied;
        reader.offset <- reader.offset + copied;
        record_response_body_read reader copied;
        if !(reader.stats.streaming_active) then
          Observer.add reader.stats.response_streaming
            Int64.(neg (of_int copied));
        Lwt.return_ok copied
      end
      else if reader.eof then Lwt.return_ok 0
      else
        Lwt.bind (Lwt_stream.get reader.stream) (function
          | None -> response_body_eof reader
          | Some chunk ->
              if !(reader.stats.streaming_active) then
                Observer.add reader.stats.response_streaming
                  (Int64.of_int (String.length chunk));
              reader.chunk <- chunk;
              reader.offset <- 0;
              read_from_current reader bytes ~off ~len)

    let invalid_read_bounds bytes ~off ~len =
      off < 0 || len < 0 || len > Bytes.length bytes - off

    let inactive_reader_error =
      Awskit.Error.Producer.body "response body reader used outside its scope"

    let close_response_body_reader reader = reader.active <- false

    let read_response_body reader bytes ~off ~len =
      if not reader.active then Lwt.return_error inactive_reader_error
      else if invalid_read_bounds bytes ~off ~len then
        Lwt.return_error (Awskit.Error.Producer.body "invalid read bounds")
      else if reader.bodiless then Lwt.return_ok 0
      else
        let read = read_from_current reader bytes ~off ~len in
        let result =
          with_timeout_result ~sleep:reader.sleep reader.timeout_policy
            `Response_body ~cancel:read
            ~on_timeout:(fun () -> close_response_body_reader reader)
            (Lwt.catch
               (fun () -> read)
               (function
                 | Lwt.Canceled ->
                     close_response_body_reader reader;
                     Lwt.fail Lwt.Canceled
                 | exn ->
                     Lwt.return_error
                       (Awskit.Error.Producer.body (Exn.to_string exn))))
        in
        Lwt.on_cancel result (fun () ->
            close_response_body_reader reader;
            Lwt.cancel read);
        Lwt.bind result (function
          | Error _ as error ->
              close_response_body_reader reader;
              Lwt.return error
          | Ok _ as ok -> Lwt.return ok)

    let next_response_body ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Lwt.return_error
          (Awskit.Error.Producer.body "chunk_size must be positive")
      else
        let buffer = Bytes.create chunk_size in
        Lwt.bind (read_response_body reader buffer ~off:0 ~len:chunk_size)
          (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok None
          | Ok n -> Lwt.return_ok (Some (Bytes.sub buffer ~pos:0 ~len:n)))

    let drain_reader reader ~remaining ~max_response_drain_bytes =
      let buffer = Bytes.create 8192 in
      let rec loop remaining =
        let len =
          if remaining <= 0 then 1 else min (Bytes.length buffer) remaining
        in
        Lwt.bind (read_response_body reader buffer ~off:0 ~len) (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok ()
          | Ok n ->
              if n > remaining then
                Lwt.return_error (drain_limit_error max_response_drain_bytes)
              else loop (remaining - n))
      in
      loop remaining

    let drain_response_body_reader reader (body : response_body) =
      run_response_cleanup body.cleanup (fun () ->
          let was_draining = !(reader.stats.draining) in
          let drained_before = !(reader.stats.connector_drained_bytes) in
          reader.stats.draining := true;
          let drain =
            Lwt.finalize
              (fun () ->
                observe_result_phase body.observability
                  (fun () ->
                    Http_observation.response_body_drain ~bytes:(fun () ->
                        Int64.max 0L
                          Int64.(
                            !(reader.stats.connector_drained_bytes)
                            - drained_before)))
                  ~method_:body.method_
                  (fun () ->
                    with_timeout_result ~sleep:body.sleep body.timeout_policy
                      `Drain
                      (drain_reader reader
                         ~remaining:body.max_response_drain_bytes
                         ~max_response_drain_bytes:body.max_response_drain_bytes)))
              (fun () ->
                reader.stats.draining := was_draining;
                Lwt.return_unit)
          in
          Lwt.catch
            (fun () ->
              Lwt.bind drain (function
                | Ok () -> Lwt.return_ok ()
                | Error _ as error ->
                    Lwt.bind (await_connector_abort body.abort) (fun () ->
                        Lwt.return error)))
            (fun exn ->
              Lwt.bind (await_connector_abort body.abort) (fun () ->
                  Lwt.fail exn)))

    let drain_response_body_after_exception reader body exn =
      let drain =
        Lwt.catch
          (fun () ->
            Lwt.bind (drain_response_body_reader reader body) (fun _ ->
                Lwt.return_unit))
          (fun _ -> Lwt.return_unit)
      in
      Lwt.bind (Lwt.protected drain) (fun () ->
          close_response_body_reader reader;
          Lwt.fail exn)

    let with_response_body body ~consume =
      let reader =
        {
          stream = Cohttp_lwt.Body.to_stream body.body;
          sleep = body.sleep;
          timeout_policy = body.timeout_policy;
          bodiless = body.bodiless;
          active = true;
          chunk = "";
          offset = 0;
          eof = false;
          remaining = initial_response_body_remaining body.framing;
          stats = body.stats;
        }
      in
      let connector_response_bytes_before =
        !(body.stats.connector_response_bytes)
      in
      Lwt.catch
        (fun () ->
          Lwt.bind
            (observe_result_phase body.observability
               (fun () ->
                 Http_observation.response_body_consumption ~bytes:(fun () ->
                     Int64.max 0L
                       Int64.(
                         !(body.stats.connector_response_bytes)
                         - connector_response_bytes_before)))
               ~method_:body.method_
               (fun () -> consume reader))
            (fun result ->
              match result with
              | Ok _ ->
                  Lwt.bind (drain_response_body_reader reader body) (function
                    | Ok () ->
                        close_response_body_reader reader;
                        Lwt.return result
                    | Error error ->
                        close_response_body_reader reader;
                        Lwt.return_error error)
              | Error _ ->
                  Lwt.bind (drain_response_body_reader reader body) (fun _ ->
                      close_response_body_reader reader;
                      Lwt.return result)))
        (drain_response_body_after_exception reader body)

    let discard_response_body body =
      let reader =
        {
          stream = Cohttp_lwt.Body.to_stream body.body;
          sleep = body.sleep;
          timeout_policy = body.timeout_policy;
          bodiless = body.bodiless;
          active = true;
          chunk = "";
          offset = 0;
          eof = false;
          remaining = initial_response_body_remaining body.framing;
          stats = body.stats;
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

      let consumed_bytes (body : response_body) =
        Int64.max 0L !(body.stats.connector_response_bytes)
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

      let resolve (c : conn) =
        Observer.with_operation c.observability
          ~operation:(fun () ->
            Awskit.Observability.For_service.Credential_resolution.operation)
          ~start:(fun () -> ())
          c.credentials_provider
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

    module Observability = struct
      type 'a io = 'a Lwt.t
      type connection = conn
      type lease = Observer.lease

      let with_operation (connection : connection) =
        Observer.with_operation connection.observability

      let emit_event (connection : connection) =
        Observer.emit_event connection.observability

      let acquire (connection : connection) =
        Observer.acquire connection.observability

      let add = Observer.add
      let release = Observer.release

      let with_instrument (connection : connection) =
        Observer.with_instrument connection.observability
    end
  end

  type t = conn
end

module Make (Client : Cohttp_lwt.S.Client) = struct
  module Connector = struct
    module Client = Client

    type call = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

    let call ?ctx ~headers ~body meth uri =
      Client.call ?ctx ~headers ~body meth uri

    let response call = call
    let abort _call = Lwt.return_unit
  end

  include Make_with_connector (Connector)
end
