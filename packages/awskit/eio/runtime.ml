open Base
module Http_observation = Awskit.Observability.For_runtime.Http

type net = Net : _ Eio.Net.t -> net
type time_clock = Time_clock : _ Eio.Time.clock -> time_clock
type http_connection = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r

type 'flow https = (Uri.t -> http_connection -> 'flow) option
  constraint 'flow = [> Eio.Resource.close_ty ] Eio.Flow.two_way

let http_only = None

type conn = {
  net : net;
  time_clock : time_clock;
  sw : Eio.Switch.t;
  https : (Uri.t -> http_connection -> http_connection) option;
  region : Awskit.Region.t;
  credentials : Awskit.Credentials.t;
  clock : unit -> Ptime.t;
  retry_policy : Awskit.Retry.t;
  random_float : upper_bound:float -> float;
  timeout_policy : Awskit.Timeout.policy;
  endpoint : Awskit.Endpoint.t option;
  max_response_drain_bytes : int;
  observability : Observer.t;
}

type stream_item = Chunk of string | End | Failed of string

module Stream_source = struct
  type t = {
    stream : stream_item Eio.Stream.t;
    connector_request_bytes : int64 option ref;
    streaming : Observer.lease;
    streaming_active : bool ref;
    mutable chunk : string;
    mutable offset : int;
  }

  let rec next_chunk t =
    match Eio.Stream.take t.stream with
    | End -> raise End_of_file
    | Failed _ ->
        (* The producer's SDK error is carried by [finished]. Ending the
           connector stream normally keeps Cohttp's request writer alive long
           enough for a response that already arrived to be cleaned up. *)
        raise End_of_file
    | Chunk chunk when String.is_empty chunk -> next_chunk t
    | Chunk chunk ->
        t.chunk <- chunk;
        t.offset <- 0

  let single_read t dst =
    if t.offset >= String.length t.chunk then next_chunk t;
    let len = min (Cstruct.length dst) (String.length t.chunk - t.offset) in
    Cstruct.blit_from_string t.chunk t.offset dst 0 len;
    t.offset <- t.offset + len;
    if !(t.streaming_active) then begin
      t.connector_request_bytes :=
        Option.map !(t.connector_request_bytes) ~f:(fun bytes ->
            Int64.(bytes + of_int len));
      Observer.add t.streaming Int64.(neg (of_int len))
    end;
    len

  let read_methods = []
end

let stream_source =
  let ops = Eio.Flow.Pi.source (module Stream_source) in
  fun stream connector_request_bytes streaming streaming_active ->
    Eio.Resource.T
      ( {
          Stream_source.stream;
          connector_request_bytes;
          streaming;
          streaming_active;
          chunk = "";
          offset = 0;
        },
        ops )

type request_body_writer = {
  stream : stream_item Eio.Stream.t;
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
  request_streaming : Observer.lease option;
  response_streaming : Observer.lease;
}

type request_body =
  | Source of Awskit.Body.Request.descriptor * string
  | Stream of
      Awskit.Body.Request.descriptor
      * (request_body_writer -> (unit, Awskit.Error.t) Result.t)

type response_body_framing =
  | Response_unknown
  | Response_content_length of int64
  | Response_chunked

type response_cleanup = { mutable started : bool }

type response_body = {
  body : Cohttp_eio.Body.t;
  method_ : Awskit.Request.Method.t;
  time_clock : time_clock;
  timeout_policy : Awskit.Timeout.policy;
  max_response_drain_bytes : int;
  framing : response_body_framing;
  bodiless : bool;
  stats : attempt_stats;
  observability : Observer.t;
  cleanup : response_cleanup;
      (* True when the response carries no message body per RFC 7230 §3.3.3
         (a response to HEAD, or status 1xx/204/304). The framing headers may
         still advertise a Content-Length, so we must NOT read the body flow
         or the read blocks until the timeout. *)
}

type response_body_reader = {
  body : Cohttp_eio.Body.t;
  time_clock : time_clock;
  timeout_policy : Awskit.Timeout.policy;
  bodiless : bool;
  mutable active : bool;
  mutable chunk : string;
  mutable offset : int;
  mutable eof : bool;
  mutable remaining : int64 option;
  stats : attempt_stats;
}

type request_body_bridge = {
  body : Cohttp_eio.Body.t option;
  finished : (unit, Awskit.Error.t) Result.t Eio.Promise.t;
}

let default_max_response_drain_bytes = 64 * 1024 * 1024

let now_from_eio_clock (Time_clock clock) =
  Eio.Time.now clock |> Ptime.of_float_s |> Option.value ~default:Ptime.epoch

let close_https (https : 'flow https) =
  Option.map https ~f:(fun connector uri flow ->
      (connector uri flow :> http_connection))

let parse_endpoint = function
  | None -> Ok None
  | Some endpoint ->
      Result.map (Awskit.Endpoint.of_string endpoint) ~f:Option.some

let create ~env ~sw ~https ~region ~credentials ?clock
    ?(retry_policy = Awskit.Retry.default) ?random_float
    ?(timeout_policy = Awskit.Timeout.default) ?endpoint
    ?(max_response_drain_bytes = default_max_response_drain_bytes)
    ?(observability = Observer.default ()) () =
  let validate_max_response_drain_bytes () =
    if max_response_drain_bytes <= 0 then
      Error
        (Awskit.Error.Producer.validation ~field:"max_response_drain_bytes"
           "max_response_drain_bytes must be positive")
    else Ok ()
  in
  match
    ( validate_max_response_drain_bytes (),
      Awskit.Region.of_string region,
      parse_endpoint endpoint )
  with
  | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
  | Ok (), Ok region, Ok endpoint ->
      let net = Net (env :> < net : _ Eio.Net.t ; .. >)#net in
      let time_clock =
        Time_clock (env :> < clock : _ Eio.Time.clock ; .. >)#clock
      in
      let clock =
        Option.value clock ~default:(fun () -> now_from_eio_clock time_clock)
      in
      let random_float =
        match random_float with
        | Some random_float -> random_float
        | None ->
            let state = Random.State.make_self_init () in
            fun ~upper_bound -> Random.State.float state upper_bound
      in
      Ok
        {
          net;
          time_clock;
          sw;
          https = close_https https;
          region;
          credentials;
          clock;
          retry_policy;
          random_float;
          timeout_policy;
          endpoint;
          max_response_drain_bytes;
          observability;
        }

let to_cohttp_meth = function
  | `GET -> `GET
  | `PUT -> `PUT
  | `POST -> `POST
  | `DELETE -> `DELETE
  | `HEAD -> `HEAD
  | `PATCH -> `PATCH

let to_aws_response http_response =
  Awskit.Response.create_exn
    ~status:(Http.Response.status http_response |> Http.Status.to_int)
    ~headers:(Http.Response.headers http_response |> Http.Header.to_list)
    ()

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

let missing_https_connector_error =
  Awskit.Error.Producer.transport ~retryable:false
    "HTTPS endpoint requires an HTTPS connector. Pass ~https with a connector \
     compatible with Cohttp_eio.Client.make. For local HTTP endpoints, pass \
     ~https:Awskit_eio.http_only and an explicit http:// endpoint."

let descriptor_for_string body =
  Awskit.Body.Request.descriptor_exn
    ~content_length:(String.length body |> Int64.of_int)
    ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
    ~replayable:true ()

let empty_request_body = Source (descriptor_for_string "", "")
let string_request_body body = Source (descriptor_for_string body, body)

let bytes_request_body body =
  let body = Bytes.to_string body in
  string_request_body body

let stream_request_body descriptor ~write = Stream (descriptor, write)

let request_body_descriptor = function
  | Source (descriptor, _) -> descriptor
  | Stream (descriptor, _) -> descriptor

let body_error message = Awskit.Error.Producer.body message
let int64_equal left right = Int64.compare left right = 0

let run_response_cleanup cleanup f =
  if cleanup.started then Ok ()
  else (
    cleanup.started <- true;
    f ())

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
    split_header_values (Http.Header.get_multi headers "transfer-encoding")
  in
  let has_transfer_encoding = not (List.is_empty transfer_encoding_values) in
  let transfer_encoding_chunked =
    has_chunked_transfer_encoding transfer_encoding_values
  in
  match
    content_length_from_headers (Http.Header.get_multi headers "content-length")
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
       (Http.Header.get_multi headers "content-length"))
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
    (Fmt.str "%s timed out after %.3fs" phase_name (Ptime.Span.to_float_s span))

let with_timeout_result (Time_clock clock) timeout_policy phase f =
  match Awskit.Timeout.span timeout_policy phase with
  | None -> f ()
  | Some span -> (
      try Eio.Time.with_timeout_exn clock (Ptime.Span.to_float_s span) f
      with Eio.Time.Timeout -> Error (timeout_error phase span))

let observe_result_phase observability definition ~method_ f =
  Observer.with_operation observability ~operation:definition
    ~start:(fun () -> Http_observation.phase_start ~method_)
    f

let drain_limit_error max_response_drain_bytes =
  Awskit.Error.Producer.body
    ~limit:(Int64.of_int max_response_drain_bytes)
    "response body exceeded max_response_drain_bytes"

let writer_for descriptor stream streaming streaming_active =
  {
    stream;
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
          Error (body_error "request body ended before declared content_length")
      )

let push_request_body (writer : request_body_writer) chunk =
  let length = Int64.of_int (String.length chunk) in
  if !(writer.streaming_active) then Observer.add writer.streaming length;
  match Eio.Stream.add writer.stream (Chunk chunk) with
  | () ->
      (writer.produced_bytes := Int64.(!(writer.produced_bytes) + length));
      Ok ()
  | exception exn ->
      if !(writer.streaming_active) then
        Observer.add writer.streaming Int64.(neg length);
      raise exn

let write_request_body_string writer string =
  match writer.write_error with
  | Some error -> Error error
  | None -> (
      match check_write_length writer (String.length string) with
      | Error error ->
          writer.write_error <- Some error;
          Error error
      | Ok () -> push_request_body writer string)

let write_request_body_subbytes writer bytes ~off ~len =
  if invalid_write_bounds bytes ~off ~len then
    Error (body_error "invalid write bounds")
  else
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match check_write_length writer len with
        | Error error ->
            writer.write_error <- Some error;
            Error error
        | Ok () ->
            push_request_body writer
              (Bytes.sub bytes ~pos:off ~len |> Bytes.to_string))

module Request_body = struct
  type 'a io = 'a
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
    write_request_body_subbytes writer bytes ~off:0 ~len:(Bytes.length bytes)
end

let body_to_cohttp ?(on_error = fun _ -> ()) ?(on_escaped_exn = fun _ -> ())
    ~(conn : conn) ~sw ~stats ~method_ = function
  | Source (_, body) ->
      {
        body = Some (Cohttp_eio.Body.of_string body);
        finished = Eio.Promise.create_resolved (Ok ());
      }
  | Stream (descriptor, write) ->
      let stream = Eio.Stream.create 16 in
      let request_streaming = Option.value_exn stats.request_streaming in
      let writer =
        writer_for descriptor stream request_streaming stats.streaming_active
      in
      let request_body_finished, wake_request_body_finished =
        Eio.Promise.create ()
      in
      let finish result =
        ignore
          (Eio.Promise.try_resolve wake_request_body_finished result : bool)
      in
      Eio.Fiber.fork ~sw (fun () ->
          match
            observe_result_phase conn.observability
              (fun () ->
                Http_observation.request_body_production ~bytes:(fun () ->
                    !(writer.produced_bytes)))
              ~method_
              (fun () ->
                with_timeout_result conn.time_clock conn.timeout_policy
                  `Request_body (fun () -> write writer))
          with
          | Ok () ->
              let result = check_finished_length writer in
              Eio.Stream.add stream End;
              finish result
          | Error error ->
              on_error error;
              finish (Error error);
              Eio.Stream.add stream (Failed (Awskit.Error.to_string_hum error))
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception exn -> (
              match Awskit.Body.Request.escaped_exn exn with
              | Some escaped ->
                  on_escaped_exn escaped;
                  finish
                    (Error (body_error "request body writer escaped exception"));
                  Eio.Stream.add stream End
              | None ->
                  let error = body_error (Exn.to_string exn) in
                  on_error error;
                  finish (Error error);
                  Eio.Stream.add stream
                    (Failed (Awskit.Error.to_string_hum error))));
      {
        body =
          Some
            (stream_source stream stats.connector_request_bytes
               request_streaming stats.streaming_active);
        finished = request_body_finished;
      }

let do_with_response_raw (conn : conn) (request : Awskit.Request.t) request_body
    ~stats ~f =
  let (Net net) = conn.net in
  let headers = Http.Header.of_list request.headers in
  let uri = make_uri request in
  match (request.target.scheme, conn.https) with
  | `Https, None -> Error missing_https_connector_error
  | `Http, _ | `Https, Some _ ->
      let client = Cohttp_eio.Client.make ~https:conn.https net in
      let meth = to_cohttp_meth request.method_ in
      (* A response to HEAD, or a 1xx/204/304 status, has no message body even
         if Content-Length is present; reading it would block until timeout. *)
      let is_head = match request.method_ with `HEAD -> true | _ -> false in
      let response_is_bodiless status =
        is_head
        || status = 204
        || status = 304
        || (status >= 100 && status < 200)
      in
      (* [Client.call] hands ownership of the raw response body to this
         attempt before request production and framing validation finish. Keep
         a connector-level cleanup path for failures before [Response_body]
         can take ownership. *)
      let cleanup_raw_response_body ~status body ~cleanup ~stats ~drain_body =
        let was_draining = !(stats.draining) in
        let drained_before = !(stats.connector_drained_bytes) in
        let drain () =
          if (not drain_body) || response_is_bodiless status then Ok ()
          else
            let buffer = Cstruct.create 0x8000 in
            let rec loop remaining =
              let len =
                if remaining <= 0 then 1
                else min (Cstruct.length buffer) remaining
              in
              let read_buffer = Cstruct.sub buffer 0 len in
              let read =
                try Eio.Flow.single_read body read_buffer with
                | End_of_file -> 0
                | exn -> raise exn
              in
              if read = 0 then Ok ()
              else (
                (stats.connector_drained_bytes :=
                   Int64.(!(stats.connector_drained_bytes) + of_int read));
                if read > remaining then
                  Error (drain_limit_error conn.max_response_drain_bytes)
                else loop (remaining - read))
            in
            stats.draining := true;
            Exn.protect
              ~f:(fun () ->
                observe_result_phase conn.observability
                  (fun () ->
                    Http_observation.response_body_drain ~bytes:(fun () ->
                        Int64.max 0L
                          Int64.(
                            !(stats.connector_drained_bytes) - drained_before)))
                  ~method_:request.method_
                  (fun () ->
                    with_timeout_result conn.time_clock conn.timeout_policy
                      `Drain (fun () -> loop conn.max_response_drain_bytes)))
              ~finally:(fun () -> stats.draining := was_draining)
        in
        Eio.Cancel.protect (fun () ->
            match
              run_response_cleanup cleanup (fun () ->
                  drain () |> Result.map_error ~f:(fun _ -> ()))
            with
            | Ok () | Error _ -> ()
            | exception _ -> ())
      in
      let make_response_body ~status response body ~cleanup =
        let bodiless = response_is_bodiless status in
        let headers = Http.Response.headers response in
        let framing =
          if bodiless then
            Result.map (validate_bodiless_response_headers headers)
              ~f:(fun () -> Response_unknown)
          else response_body_framing headers
        in
        match framing with
        | Error _ as error -> error
        | Ok framing ->
            Ok
              {
                body;
                method_ = request.method_;
                time_clock = conn.time_clock;
                timeout_policy = conn.timeout_policy;
                max_response_drain_bytes = conn.max_response_drain_bytes;
                framing = (if bodiless then Response_unknown else framing);
                bodiless;
                stats;
                observability = conn.observability;
                cleanup;
              }
      in
      let successful_status status = status >= 200 && status < 300 in
      let early_result = ref None in
      let request_body_stream_error = ref None in
      let request_body_escaped_exn = ref None in
      let exception Early_response in
      let exception Callback_raised of exn in
      let call_f response response_body =
        match f response response_body with
        | result -> result
        | exception exn -> raise (Callback_raised exn)
      in
      let call_response ~status response body ~cleanup =
        match make_response_body ~status response body ~cleanup with
        | Error _ as error ->
            cleanup_raw_response_body ~status body ~cleanup ~stats
              ~drain_body:false;
            error
        | Ok response_body -> call_f (to_aws_response response) response_body
      in
      let handle_response ~bridge ~request_body_escaped_exn response body =
        let status = Http.Response.status response |> Http.Status.to_int in
        let cleanup = { started = false } in
        let cleanup_on_error result =
          match result with
          | Ok _ as result -> result
          | Error _ as result ->
              cleanup_raw_response_body ~status body ~cleanup ~stats
                ~drain_body:true;
              result
        in
        try
          let request_body_result =
            if successful_status status then Eio.Promise.await bridge.finished
            else
              match Eio.Promise.peek bridge.finished with
              | Some result -> result
              | None ->
                  let result = call_response ~status response body ~cleanup in
                  early_result := Some (cleanup_on_error result);
                  raise Early_response
          in
          match !request_body_escaped_exn with
          | Some escaped -> raise escaped
          | None -> (
              match request_body_result with
              | Error _ as error -> cleanup_on_error error
              | Ok () ->
                  call_response ~status response body ~cleanup
                  |> cleanup_on_error)
        with exn ->
          cleanup_raw_response_body ~status body ~cleanup ~stats
            ~drain_body:true;
          raise exn
      in
      let run_call ~call_sw =
        let bridge =
          body_to_cohttp ~conn ~sw:call_sw ~stats ~method_:request.method_
            ~on_error:(fun error -> request_body_stream_error := Some error)
            ~on_escaped_exn:(fun exn -> request_body_escaped_exn := Some exn)
            request_body
        in
        try
          with_timeout_result conn.time_clock conn.timeout_policy `Attempt
            (fun () ->
              match
                observe_result_phase conn.observability
                  Http_observation.response_headers_wait
                  ~method_:request.method_ (fun () ->
                    with_timeout_result conn.time_clock conn.timeout_policy
                      `Connect (fun () ->
                        let response, body =
                          Cohttp_eio.Client.call client ~sw:call_sw ~headers
                            ?body:bridge.body meth uri
                        in
                        Ok (response, body)))
              with
              | Error _ as error -> error
              | Ok (response, body) ->
                  handle_response ~bridge ~request_body_escaped_exn response
                    body)
        with
        | Early_response as exn -> raise exn
        | Callback_raised exn -> raise exn
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> (
            match !request_body_escaped_exn with
            | Some escaped -> raise escaped
            | None -> (
                match Eio.Promise.peek bridge.finished with
                | Some (Error error) -> Error error
                | _ ->
                    let message = Exn.to_string exn in
                    let error =
                      Awskit.Error.Producer.transport ~retryable:true message
                    in
                    Error error))
      in
      let run_attempt () =
        try
          Eio.Switch.run ~name:"awskit http attempt" (fun call_sw ->
              run_call ~call_sw)
        with
        | Early_response -> (
            match !early_result with
            | Some result -> result
            | None -> Error (body_error "missing early response result"))
        | Callback_raised exn -> raise exn
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> (
            match !request_body_escaped_exn with
            | Some escaped -> raise escaped
            | None -> (
                match !request_body_stream_error with
                | Some error -> Error error
                | None -> raise exn))
      in
      with_timeout_result conn.time_clock conn.timeout_policy `Operation
        run_attempt

let do_with_response (conn : conn) (request : Awskit.Request.t) request_body ~f
    =
  let descriptor = request_body_descriptor request_body in
  let replayability =
    if descriptor.replayable then Http_observation.Replayable
    else Non_replayable
  in
  Observer.with_instrument conn.observability
    Http_observation.attempts_in_flight
    ~labels:(fun () -> Http_observation.request_state ~method_:request.method_)
    1L
    (fun () ->
      let request_streaming =
        match request_body with
        | Source _ -> None
        | Stream _ ->
            Some
              (Observer.acquire conn.observability
                 Http_observation.streaming_bytes_in_flight
                 ~labels:(fun () ->
                   Http_observation.streaming_state Http_observation.Request)
                 0L)
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
              (match request_body with Source _ -> None | Stream _ -> Some 0L);
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
      Exn.protect
        ~f:(fun () ->
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
        ~finally:(fun () ->
          stats.streaming_active := false;
          Option.iter request_streaming ~f:Observer.release;
          Observer.release response_streaming))

type connection = conn
type 'a t = 'a

let invalid_read_bounds bytes ~off ~len =
  off < 0 || len < 0 || len > Bytes.length bytes - off

let inactive_reader_error =
  Awskit.Error.Producer.body "response body reader used outside its scope"

let close_response_body_reader reader = reader.active <- false

let initial_response_body_remaining = function
  | Response_content_length content_length -> Some content_length
  | Response_unknown | Response_chunked -> None

let record_response_body_read reader length =
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
      let remaining = Int64.(remaining - length64) in
      reader.remaining <- Some remaining

let response_body_eof reader =
  match reader.remaining with
  | Some remaining when Int64.compare remaining 0L > 0 ->
      Error (body_error "response body ended before declared Content-Length")
  | None | Some _ ->
      reader.eof <- true;
      Ok 0

let rec read_from_current reader bytes ~off ~len =
  if len = 0 then Ok 0
  else if reader.offset < String.length reader.chunk then begin
    let available = String.length reader.chunk - reader.offset in
    let copied = min available len in
    Bytes.From_string.blit ~src:reader.chunk ~src_pos:reader.offset ~dst:bytes
      ~dst_pos:off ~len:copied;
    reader.offset <- reader.offset + copied;
    record_response_body_read reader copied;
    if !(reader.stats.streaming_active) then
      Observer.add reader.stats.response_streaming Int64.(neg (of_int copied));
    Ok copied
  end
  else if reader.eof then Ok 0
  else
    let buffer = Cstruct.create 0x8000 in
    let read = Eio.Flow.single_read reader.body buffer in
    reader.chunk <- Cstruct.to_string ~len:read buffer;
    if !(reader.stats.streaming_active) then
      Observer.add reader.stats.response_streaming (Int64.of_int read);
    reader.offset <- 0;
    read_from_current reader bytes ~off ~len

let read_response_body reader bytes ~off ~len =
  if not reader.active then Error inactive_reader_error
  else if invalid_read_bounds bytes ~off ~len then
    Error (Awskit.Error.Producer.body "invalid read bounds")
  else if reader.bodiless then Ok 0
  else
    match
      with_timeout_result reader.time_clock reader.timeout_policy `Response_body
        (fun () ->
          try read_from_current reader bytes ~off ~len with
          | End_of_file -> response_body_eof reader
          | Eio.Cancel.Cancelled _ as exn ->
              close_response_body_reader reader;
              raise exn
          | exn ->
              close_response_body_reader reader;
              Error (Awskit.Error.Producer.body (Exn.to_string exn)))
    with
    | Error _ as error ->
        close_response_body_reader reader;
        error
    | Ok _ as ok -> ok

let next_response_body ?(chunk_size = 8192) reader =
  if chunk_size <= 0 then
    Error (Awskit.Error.Producer.body "chunk_size must be positive")
  else
    let buffer = Bytes.create chunk_size in
    match read_response_body reader buffer ~off:0 ~len:chunk_size with
    | Error _ as error -> error
    | Ok 0 -> Ok None
    | Ok n -> Ok (Some (Bytes.sub buffer ~pos:0 ~len:n))

let discard_reader reader ~remaining ~max_response_drain_bytes =
  let buffer = Bytes.create 8192 in
  let rec loop remaining =
    let len =
      if remaining <= 0 then 1 else min (Bytes.length buffer) remaining
    in
    match read_response_body reader buffer ~off:0 ~len with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok n ->
        if n > remaining then Error (drain_limit_error max_response_drain_bytes)
        else loop (remaining - n)
  in
  loop remaining

let discard_response_body_reader (reader : response_body_reader)
    (body : response_body) =
  run_response_cleanup body.cleanup (fun () ->
      let was_draining = !(reader.stats.draining) in
      let drained_before = !(reader.stats.connector_drained_bytes) in
      reader.stats.draining := true;
      Exn.protect
        ~f:(fun () ->
          observe_result_phase body.observability
            (fun () ->
              Http_observation.response_body_drain ~bytes:(fun () ->
                  Int64.max 0L
                    Int64.(
                      !(reader.stats.connector_drained_bytes) - drained_before)))
            ~method_:body.method_
            (fun () ->
              with_timeout_result body.time_clock body.timeout_policy `Drain
                (fun () ->
                  discard_reader reader ~remaining:body.max_response_drain_bytes
                    ~max_response_drain_bytes:body.max_response_drain_bytes)))
        ~finally:(fun () -> reader.stats.draining := was_draining))

let discard_response_body_after_exception reader body = function
  | exn ->
      Eio.Cancel.protect (fun () ->
          match discard_response_body_reader reader body with
          | Ok () | Error _ -> ()
          | exception _ -> ());
      close_response_body_reader reader;
      raise exn

let with_response_body (body : response_body) ~consume =
  let reader =
    {
      body = body.body;
      time_clock = body.time_clock;
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
  match
    observe_result_phase body.observability
      (fun () ->
        Http_observation.response_body_consumption ~bytes:(fun () ->
            Int64.max 0L
              Int64.(
                !(body.stats.connector_response_bytes)
                - connector_response_bytes_before)))
      ~method_:body.method_
      (fun () -> consume reader)
  with
  | exception exn -> discard_response_body_after_exception reader body exn
  | Ok _ as result -> (
      match discard_response_body_reader reader body with
      | Ok () ->
          close_response_body_reader reader;
          result
      | Error _ as error ->
          close_response_body_reader reader;
          error)
  | Error _ as error -> (
      match discard_response_body_reader reader body with
      | Ok () | Error _ ->
          close_response_body_reader reader;
          error)

let discard_response_body (body : response_body) =
  discard_response_body_reader
    {
      body = body.body;
      time_clock = body.time_clock;
      timeout_policy = body.timeout_policy;
      bodiless = body.bodiless;
      active = true;
      chunk = "";
      offset = 0;
      eof = false;
      remaining = initial_response_body_remaining body.framing;
      stats = body.stats;
    }
    body

module Response_body = struct
  type 'a io = 'a
  type t = response_body
  type reader = response_body_reader

  let read = read_response_body
  let next = next_response_body
  let with_reader = with_response_body
  let discard = discard_response_body

  let consumed_bytes (body : response_body) =
    Int64.max 0L !(body.stats.connector_response_bytes)
end

module IO = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
end

module Transport = struct
  type 'a io = 'a
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
  type 'a io = 'a
  type connection = conn

  let sleep (c : conn) span =
    let (Time_clock clock) = c.time_clock in
    Eio.Time.sleep clock (Ptime.Span.to_float_s span)
end

module Random = struct
  type connection = conn

  let float c ~upper_bound = c.random_float ~upper_bound
end

module Credentials = struct
  type 'a io = 'a
  type connection = conn

  let resolve (c : conn) =
    Observer.with_operation c.observability
      ~operation:(fun () ->
        Awskit.Observability.For_service.Credential_resolution.operation)
      ~start:(fun () -> ())
      (fun () -> Ok c.credentials)
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
  type 'a io = 'a
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
