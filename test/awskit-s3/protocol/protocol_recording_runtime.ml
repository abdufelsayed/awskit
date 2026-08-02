open Awskit_s3

module Runtime = struct
  type timeline_kind = Start | Finish | Event | Sleep

  type timeline_entry = {
    kind : timeline_kind;
    name : string;
    parent : string option;
    attempt : int option;
    outcome : Awskit.Observability.Outcome.t option;
  }

  type frame = { name : string; attempt : int option }

  type response =
    | Response of {
        status : int;
        headers : (string * string) list;
        body : string;
        read_error_after : int option;
      }
    | Failure of Awskit.Error.t

  type call = { request : Awskit.Request.t; body : string }

  type request_body = {
    body : (string, Awskit.Error.t) result;
    descriptor : Awskit.Body.Request.descriptor;
  }

  module O = Awskit.Observability
  module P = O.For_projection

  type capture = {
    mutable observations : P.Operation.Completion.t list;
    mutable events : P.Event.t list;
    mutable instruments : P.Metric.Observation.t list;
    mutable stack : frame list;
    mutable timeline : timeline_entry list;
    mutable tick : int64;
  }

  let attempt_diagnostic diagnostics =
    List.find_map
      (fun diagnostic ->
        if String.equal "attempt" (O.Diagnostic.Public.name diagnostic) then
          match O.Diagnostic.Public.value diagnostic with
          | Int value -> Some value
          | String _ | Bool _ | Int64 _ | Float _ -> None
        else None)
      diagnostics

  let current_frame capture = List.nth_opt capture.stack 0

  let record_timeline capture ~kind ~name ~parent ~attempt ?outcome () =
    capture.timeline <-
      { kind; name; parent; attempt; outcome } :: capture.timeline

  module Capture_trace = struct
    type t = capture

    type activation = {
      capture : capture;
      frame : frame;
      parent : frame option;
    }

    let name _ = "protocol-recording"
    let needs_clock _ = true
    let enabled _ _ = true

    let start capture started =
      let parent = current_frame capture in
      let name = started |> P.Operation.Start.info |> P.Operation.Info.name in
      let attempt =
        match attempt_diagnostic (P.Operation.Start.diagnostics started) with
        | Some _ as attempt -> attempt
        | None -> Option.bind parent (fun frame -> frame.attempt)
      in
      let frame = { name; attempt } in
      record_timeline capture ~kind:Start ~name
        ~parent:(Option.map (fun frame -> frame.name) parent)
        ~attempt ();
      { capture; frame; parent }

    let correlation _ = []

    let within activation callback =
      let parent_stack = activation.capture.stack in
      activation.capture.stack <- activation.frame :: parent_stack;
      match callback () with
      | value ->
          activation.capture.stack <- parent_stack;
          value
      | exception exn ->
          activation.capture.stack <- parent_stack;
          raise exn

    let finish activation completion =
      let name =
        completion |> P.Operation.Completion.info |> P.Operation.Info.name
      in
      activation.capture.observations <-
        completion :: activation.capture.observations;
      record_timeline activation.capture ~kind:Finish ~name
        ~parent:(Option.map (fun frame -> frame.name) activation.parent)
        ~attempt:activation.frame.attempt
        ~outcome:(P.Operation.Completion.outcome completion)
        ()

    let event_enabled _ _ = true

    let event capture event =
      let frame = current_frame capture in
      let attempt =
        match attempt_diagnostic (P.Event.diagnostics event) with
        | Some _ as attempt -> attempt
        | None -> Option.bind frame (fun frame -> frame.attempt)
      in
      let name = event |> P.Event.info |> P.Event.Info.name in
      record_timeline capture ~kind:Event ~name
        ~parent:(Option.map (fun frame -> frame.name) frame)
        ~attempt ();
      capture.events <- event :: capture.events
  end

  module Context = struct
    type +'a io = 'a
    type 'a key = { mutable value : 'a option }

    let create () = { value = None }
    let get key = key.value

    let with_binding key value callback =
      let previous = key.value in
      key.value <- Some value;
      match callback () with
      | result ->
          key.value <- previous;
          result
      | exception exn ->
          key.value <- previous;
          raise exn

    let bind value callback = callback value
    let return value = value
    let fail exn = raise exn
    let capture callback = try Ok (callback ()) with exn -> Error exn

    let finalize callback hook =
      try
        let value = callback () in
        (try hook (Ok value) with _ -> ());
        value
      with exn ->
        (try hook (Error exn) with _ -> ());
        raise exn

    let raised_outcome _ = O.Outcome.Exception
  end

  module Runtime_trace_sink = struct
    type +'a io = 'a Context.io
    type t = Capture_trace.t
    type activation = Capture_trace.activation

    let name = Capture_trace.name
    let needs_clock = Capture_trace.needs_clock
    let enabled = Capture_trace.enabled
    let start = Capture_trace.start
    let correlation = Capture_trace.correlation
    let within = Capture_trace.within
    let finish = Capture_trace.finish
    let event_enabled = Capture_trace.event_enabled
    let event = Capture_trace.event
  end

  module Observer = O.For_runtime.Make (Context) (Runtime_trace_sink)

  type connection = {
    region : Region.t;
    credentials : Credentials.t;
    credential_error : Awskit.Error.t option;
    endpoint_config : Awskit_s3.endpoint_config;
    retry_policy : Awskit.Retry.t;
    mutable calls : call list;
    mutable responses : response list;
    mutable sleeps : Ptime.Span.t list;
    capture : capture;
    observer : Observer.t;
  }

  type 'a t = 'a

  type response_body = {
    method_ : Awskit.Request.Method.t;
    body : string;
    read_error_after : int option;
    consumed_bytes : int64 ref;
    draining : bool ref;
    connector_total_bytes : int64 ref;
    connection : connection;
  }

  type request_body_writer = Buffer.t

  type response_body_reader = {
    body : string;
    read_error_after : int option;
    mutable active : bool;
    mutable offset : int;
    consumed_bytes : int64 ref;
    draining : bool ref;
    connector_total_bytes : int64 ref;
  }

  let connect ?(endpoint_config = Awskit_s3.default_endpoint_config)
      ?(region = Region.of_string_exn "us-east-1")
      ?(credentials = Protocol_support.credentials) ?(credential_error = None)
      ?(retry_policy = Awskit.Retry.default) responses =
    let capture =
      {
        observations = [];
        events = [];
        instruments = [];
        stack = [];
        timeline = [];
        tick = 0L;
      }
    in
    let metric_sink =
      O.Metric_sink.create ~name:"protocol-recording.metrics" ~needs_clock:false
        ~enabled:(fun family ->
          match P.Metric.Family.aggregation family with
          | Gauge -> true
          | Counter | Histogram -> false)
        ~observe:(fun observation ->
          capture.instruments <- observation :: capture.instruments)
    in
    let clock () =
      let value = capture.tick in
      capture.tick <- Int64.succ value;
      value
    in
    let observer =
      Observer.create ~logs:false ~clock ~metric_sinks:[ metric_sink ]
        ~trace_sinks:[ capture ] ()
    in
    {
      region;
      credentials;
      credential_error;
      endpoint_config;
      retry_policy;
      calls = [];
      responses;
      sleeps = [];
      capture;
      observer;
    }

  let last_call conn =
    match conn.calls with
    | call :: _ -> call
    | [] -> Alcotest.fail "expected recorded request"

  let descriptor ?(replayable = true) body =
    Awskit.Body.Request.descriptor_exn
      ~content_length:(Int64.of_int (String.length body))
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
      ~replayable ()

  let request_body ?replayable body =
    { body = Ok body; descriptor = descriptor ?replayable body }

  let empty_request_body = request_body ""
  let string_request_body body = request_body body
  let bytes_request_body body = request_body (Bytes.to_string body)

  let stream_request_body descriptor ~write =
    let buffer = Buffer.create 128 in
    let body =
      match write buffer with
      | Ok () -> (
          let body = Buffer.contents buffer in
          let length = Int64.of_int (String.length body) in
          match descriptor.Awskit.Body.Request.content_length with
          | Some declared when not (Int64.equal declared length) ->
              Error (Awskit.Error.Producer.body "request body length mismatch")
          | _ -> Ok body)
      | Error _ as error -> error
    in
    { body; descriptor }

  let request_body_descriptor body = body.descriptor

  let write_request_body_string writer body =
    Buffer.add_string writer body;
    Ok ()

  let with_operation conn ~operation:definition ~start callback =
    Observer.with_operation conn.observer ~operation:definition ~start callback

  let with_phase conn definition ~method_ callback =
    with_operation conn ~operation:definition
      ~start:(fun () -> O.For_runtime.Http.phase_start ~method_)
      callback

  let emit_event conn definition ~data =
    Observer.emit_event conn.observer definition ~data

  let acquire conn instrument ~labels value =
    Observer.acquire conn.observer instrument ~labels value

  let with_instrument conn instrument ~labels value callback =
    let lease = acquire conn instrument ~labels value in
    match callback () with
    | result ->
        Observer.release lease;
        result
    | exception exn ->
        Observer.release lease;
        raise exn

  let read_response_body reader bytes ~off ~len =
    if not reader.active then
      Error
        (Awskit.Error.Producer.body
           "response body reader used outside its scope")
    else if len = 0 then Ok 0
    else if
      match reader.read_error_after with
      | Some limit -> reader.offset >= limit
      | None -> false
    then Error (Awskit.Error.Producer.body "simulated download read failure")
    else
      let remaining = String.length reader.body - reader.offset in
      if remaining <= 0 then Ok 0
      else
        let copied = min len remaining in
        String.blit reader.body reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        reader.connector_total_bytes :=
          Int64.add !(reader.connector_total_bytes) (Int64.of_int copied);
        if not !(reader.draining) then
          reader.consumed_bytes :=
            Int64.add !(reader.consumed_bytes) (Int64.of_int copied);
        Ok copied

  let rec drain_reader reader =
    let bytes = Bytes.create 8 in
    match read_response_body reader bytes ~off:0 ~len:(Bytes.length bytes) with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> drain_reader reader

  let drain (body : response_body) reader =
    let was_draining = !(body.draining) in
    let before = !(body.connector_total_bytes) in
    body.draining := true;
    match
      with_phase body.connection
        (fun () ->
          O.For_runtime.Http.response_body_drain ~bytes:(fun () ->
              Int64.sub !(body.connector_total_bytes) before))
        ~method_:body.method_
        (fun () -> drain_reader reader)
    with
    | result ->
        body.draining := was_draining;
        result
    | exception exn ->
        body.draining := was_draining;
        raise exn

  let with_response_body (body : response_body) ~consume =
    let reader =
      {
        body = body.body;
        read_error_after = body.read_error_after;
        active = true;
        offset = 0;
        consumed_bytes = body.consumed_bytes;
        draining = body.draining;
        connector_total_bytes = body.connector_total_bytes;
      }
    in
    let before = !(body.consumed_bytes) in
    match
      with_phase body.connection
        (fun () ->
          O.For_runtime.Http.response_body_consumption ~bytes:(fun () ->
              Int64.sub !(body.consumed_bytes) before))
        ~method_:body.method_
        (fun () -> consume reader)
    with
    | Ok _ as result -> (
        match drain body reader with
        | Ok () ->
            reader.active <- false;
            result
        | Error _ as error ->
            reader.active <- false;
            error)
    | Error _ as error -> (
        match drain body reader with
        | Ok () | Error _ ->
            reader.active <- false;
            error)
    | exception exn ->
        ignore (drain body reader : (unit, Awskit.Error.t) result);
        reader.active <- false;
        raise exn

  let discard_response_body (body : response_body) =
    let reader =
      {
        body = body.body;
        read_error_after = body.read_error_after;
        active = true;
        offset = 0;
        consumed_bytes = body.consumed_bytes;
        draining = body.draining;
        connector_total_bytes = body.connector_total_bytes;
      }
    in
    let result = drain body reader in
    reader.active <- false;
    result

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

    let write_bytes writer bytes =
      write_request_body_string writer (Bytes.to_string bytes)

    let write_subbytes writer bytes ~off ~len =
      write_request_body_string writer (Bytes.sub_string bytes off len)
  end

  module Response_body = struct
    type 'a io = 'a
    type t = response_body
    type reader = response_body_reader

    let read = read_response_body

    let next ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Error (Awskit.Error.Producer.body "chunk_size must be positive")
      else
        let bytes = Bytes.create chunk_size in
        match read_response_body reader bytes ~off:0 ~len:chunk_size with
        | Error _ as error -> error
        | Ok 0 -> Ok None
        | Ok n -> Ok (Some (Bytes.sub bytes 0 n))

    let with_reader = with_response_body
    let discard = discard_response_body
    let consumed_bytes (body : response_body) = !(body.consumed_bytes)
  end

  let do_with_response conn request ~body ~connector_total_bytes ~f =
    conn.calls <- { request; body } :: conn.calls;
    let response =
      with_phase conn O.For_runtime.Http.response_headers_wait
        ~method_:request.Awskit.Request.method_ (fun () ->
          match conn.responses with
          | [] ->
              Error
                (Awskit.Error.Producer.transport ~retryable:false
                   "no canned response")
          | Failure error :: rest ->
              conn.responses <- rest;
              Error error
          | Response response :: rest ->
              conn.responses <- rest;
              Ok (Response response))
    in
    match response with
    | Error _ as error -> error
    | Ok (Response response) ->
        f
          (Awskit.Response.create_exn ~status:response.status
             ~headers:response.headers ())
          {
            method_ = request.method_;
            body = response.body;
            read_error_after = response.read_error_after;
            consumed_bytes = ref 0L;
            draining = ref false;
            connector_total_bytes;
            connection = conn;
          }
    | Ok (Failure error) -> Error error

  module IO = struct
    type 'a t = 'a

    let return value = value
    let bind value f = f value
  end

  module Transport = struct
    type 'a io = 'a
    type nonrec connection = connection
    type nonrec request_body = request_body
    type nonrec response_body = response_body

    let with_response conn (request : Awskit.Request.t) ~(body : request_body)
        ~consume =
      let response_seen = ref None in
      let connector_request_bytes = ref 0L in
      let connector_response_bytes = ref 0L in
      let connector_total_bytes = ref 0L in
      let connector_drained_bytes = ref 0L in
      let replayability =
        if body.descriptor.replayable then O.For_runtime.Http.Replayable
        else O.For_runtime.Http.Non_replayable
      in
      let response () =
        Option.map
          (fun response ->
            O.For_runtime.Http.response
              ~status:(Awskit.Response.status response)
              ?request_id:(Awskit.Response.request_id response)
              ?host_id:(Awskit.Response.host_id response)
              ())
          !response_seen
      in
      let stats () =
        O.For_runtime.Http.request_stats
          ~connector_request_bytes:(Some !connector_request_bytes)
          ~connector_response_bytes:!connector_response_bytes
          ~connector_drained_bytes:!connector_drained_bytes
      in
      with_instrument conn O.For_runtime.Http.attempts_in_flight
        ~labels:(fun () ->
          O.For_runtime.Http.request_state ~method_:request.method_)
        1L
        (fun () ->
          with_operation conn
            ~operation:(fun () -> O.For_runtime.Http.request ~response ~stats)
            ~start:(fun () ->
              O.For_runtime.Http.request_start ~method_:request.method_
                ~replayability)
            (fun () ->
              let produced =
                with_phase conn
                  (fun () ->
                    O.For_runtime.Http.request_body_production ~bytes:(fun () ->
                        !connector_request_bytes))
                  ~method_:request.method_
                  (fun () ->
                    match body.body with
                    | Error _ as error -> error
                    | Ok body ->
                        connector_request_bytes :=
                          Int64.of_int (String.length body);
                        Ok body)
              in
              match produced with
              | Error _ as error -> error
              | Ok body ->
                  do_with_response conn request ~body ~connector_total_bytes
                    ~f:(fun response response_body ->
                      response_seen := Some response;
                      let finish_stats () =
                        connector_response_bytes :=
                          !(response_body.consumed_bytes);
                        connector_drained_bytes :=
                          Int64.sub
                            !(response_body.connector_total_bytes)
                            !(response_body.consumed_bytes)
                      in
                      match consume response response_body with
                      | result ->
                          finish_stats ();
                          result
                      | exception exn ->
                          finish_stats ();
                          raise exn)))
  end

  module Clock = struct
    type nonrec connection = connection

    let now _ = Protocol_support.test_time
  end

  module Sleeper = struct
    type 'a io = 'a
    type nonrec connection = connection

    let sleep conn span =
      conn.sleeps <- span :: conn.sleeps;
      let parent = current_frame conn.capture in
      record_timeline conn.capture ~kind:Sleep ~name:"retry.backoff"
        ~parent:(Option.map (fun frame -> frame.name) parent)
        ~attempt:(Option.bind parent (fun frame -> frame.attempt))
        ()
  end

  module Random = struct
    type nonrec connection = connection

    let float _ ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a
    type nonrec connection = connection

    let resolve conn =
      with_operation conn
        ~operation:(fun () -> O.For_service.Credential_resolution.operation)
        ~start:(fun () -> ())
        (fun () ->
          match conn.credential_error with
          | Some error -> Error error
          | None -> Ok conn.credentials)
  end

  module Endpoint = struct
    type nonrec connection = connection

    let region conn = conn.region
    let endpoint _ = None
  end

  module Retry = struct
    type nonrec connection = connection

    let policy conn = conn.retry_policy
  end

  module Timeout = struct
    type nonrec connection = connection

    let policy _ = Awskit.Timeout.default
  end

  module S3_endpoint = struct
    type nonrec connection = connection

    let s3_endpoint_config conn = conn.endpoint_config
  end
end

module Observation_port = struct
  type 'a io = 'a
  type connection = Runtime.connection
  type lease = Runtime.Observer.lease

  let with_operation = Runtime.with_operation
  let emit_event = Runtime.emit_event
  let acquire = Runtime.acquire
  let add = Runtime.Observer.add
  let release = Runtime.Observer.release
  let with_instrument = Runtime.with_instrument
end

module S3 = Awskit_s3.Observability.Make (Runtime) (Observation_port)

type response = Runtime.response
type call = Runtime.call
type connection = Runtime.connection
type timeline_kind = Runtime.timeline_kind = Start | Finish | Event | Sleep

type timeline_entry = Runtime.timeline_entry = {
  kind : timeline_kind;
  name : string;
  parent : string option;
  attempt : int option;
  outcome : Awskit.Observability.Outcome.t option;
}

let connect = Runtime.connect
let last_call = Runtime.last_call
let observations conn = List.rev conn.Runtime.capture.observations
let events conn = List.rev conn.Runtime.capture.events
let instruments conn = List.rev conn.Runtime.capture.instruments
let calls conn = List.rev conn.Runtime.calls
let sleeps conn = List.rev conn.Runtime.sleeps
let timeline conn = List.rev conn.Runtime.capture.timeline

let response ?(headers = []) ?read_error_after status body =
  Runtime.Response { status; headers; body; read_error_after }

let failure error = Runtime.Failure error
