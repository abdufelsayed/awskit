open Base
module Model = Runtime_http_model

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let workload_timeout = Ptime.Span.of_float_s 0.25 |> Option.value_exn

let timeout_policy =
  Awskit.Timeout.create_exn ~operation:workload_timeout
    ~response_body:workload_timeout ~drain:workload_timeout ()

let timeout_error message =
  Awskit.Error.Producer.timeout ~operation:"runtime http workload" message

let listener_bind_denied_by_sandbox = function
  (* opam-repository macOS CI can deny local TCP listeners under sandbox.sh. *)
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let ensure_loopback_listener_available env =
  try
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    ignore
      (Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
         (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)))
  with exn when listener_bind_denied_by_sandbox exn ->
    Loopback_policy.handle_bind_denied ()

let rec read_headers input =
  match Eio.Buf_read.line input with "" -> () | _ -> read_headers input

let rec read_request_headers input acc =
  match Eio.Buf_read.line input with
  | "" -> List.rev acc
  | line -> (
      match String.lsplit2 line ~on:':' with
      | Some (name, value) ->
          read_request_headers input
            ((String.strip name, String.strip value) :: acc)
      | None -> read_request_headers input acc)

let header_value name headers =
  List.find_map headers ~f:(fun (candidate, value) ->
      if String.Caseless.equal name candidate then Some value else None)

let read_request_body input headers =
  match header_value "content-length" headers with
  | None -> ""
  | Some value -> Eio.Buf_read.take (Int.of_string value) input

let write_empty_response output =
  Eio.Buf_write.string output
    "HTTP/1.1 200 test\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
  Eio.Buf_write.flush output

let write_response output scenario =
  Eio.Buf_write.string output
    (Printf.sprintf "HTTP/1.1 %d test\r\n" scenario.Model.status);
  Eio.Buf_write.string output (Model.response_header_block scenario);
  Eio.Buf_write.string output "\r\n";
  Eio.Buf_write.string output (Model.body_for_framing scenario.framing);
  Eio.Buf_write.flush output

let with_loopback_server env scenario test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn ->
      Loopback_policy.handle_bind_denied ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let hold_open, resolve_hold_open = Eio.Promise.create () in
  let release_server () =
    ignore (Eio.Promise.try_resolve resolve_hold_open () : bool)
  in
  let observed_request_method = ref None in
  let check_observed_request_method observed_method =
    Alcotest.(check string)
      "request method"
      (Model.method_to_string scenario.Model.method_)
      observed_method
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let request_line = Eio.Buf_read.line input in
          let observed_method =
            match String.split request_line ~on:' ' with
            | method_ :: _ -> method_
            | [] -> Alcotest.fail "missing HTTP request line"
          in
          observed_request_method := Some observed_method;
          read_headers input;
          Eio.Buf_write.with_flow flow (fun output ->
              write_response output scenario;
              match scenario.connection with
              | Close -> ()
              | Keep_alive -> Eio.Promise.await hold_open));
      `Stop_daemon);
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  match test endpoint with
  | result ->
      release_server ();
      (match (result, !observed_request_method) with
      | Ok _, Some observed_method | Error _, Some observed_method ->
          check_observed_request_method observed_method
      | Ok _, None ->
          Alcotest.failf "%s succeeded without observed request method"
            scenario.Model.name
      | Error _, None -> ());
      result
  | exception exn ->
      release_server ();
      (match !observed_request_method with
      | Some observed_method -> check_observed_request_method observed_method
      | None -> ());
      raise exn

let request_for_endpoint scenario endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:scenario.Model.method_ ~target ()

let request_conn ?observability ?(timeout_policy = timeout_policy)
    ?max_response_drain_bytes env sw =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
    ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~timeout_policy
    ?max_response_drain_bytes ?observability ()
  |> conn_or_fail

let static_body_payload = String.make (4 * 1024) 'x'

(* Cohttp derives Content-Length from the body's native static representation.
   Capturing the request on a loopback peer checks that observation does not
   change that representation or the bytes pulled from it. *)
let run_static_body_request env ~observability ~payload ~make_body =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn ->
      Loopback_policy.handle_bind_denied ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let captured = ref None in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          try
            let _request_line = Eio.Buf_read.line input in
            let headers = read_request_headers input [] in
            let body = read_request_body input headers in
            captured := Some (headers, body);
            Eio.Buf_write.with_flow flow write_empty_response
          with _ -> ());
      `Stop_daemon);
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"127.0.0.1" ~port
      ~path:"/" ()
  in
  let result =
    Eio.Switch.run @@ fun call_sw ->
    let conn = request_conn ~observability env call_sw in
    let request = Awskit.Request.create_exn ~method_:`POST ~target () in
    Awskit_eio.Runtime.Transport.with_response conn request
      ~body:(make_body payload) ~consume:(fun _response body ->
        Awskit_eio.Runtime.Response_body.discard body)
  in
  match (result, !captured) with
  | result, Some (headers, body) -> (result, headers, body)
  | _, None -> Alcotest.fail "static body server did not capture request"

let run_early_failure_request ?observability env ~request_body ~response_headers
    ~response_body =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn ->
      Loopback_policy.handle_bind_denied ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let client_done, resolve_client_done = Eio.Promise.create () in
  let peer_closed, resolve_peer_closed = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          (try
             ignore (Eio.Buf_read.line input : string);
             ignore (read_request_headers input []);
             Eio.Buf_write.with_flow flow (fun output ->
                 Eio.Buf_write.string output "HTTP/1.1 200 test\r\n";
                 List.iter response_headers ~f:(fun (name, value) ->
                     Eio.Buf_write.string output
                       (Fmt.str "%s: %s\r\n" name value));
                 Eio.Buf_write.string output "\r\n";
                 Eio.Buf_write.string output response_body;
                 Eio.Buf_write.flush output);
             (* The nested attempt switch owns the socket and must close it
                after returning the producer/framing error. *)
             ignore (Eio.Buf_read.take_all input : string)
           with _ -> ());
          ignore (Eio.Promise.try_resolve resolve_peer_closed () : bool));
      `Stop_daemon);
  let client_result =
    ref (None : ((unit, Awskit.Error.t) Result.t, exn) Result.t option)
  in
  Eio.Fiber.fork ~sw (fun () ->
      let result =
        try
          Ok
            ( Eio.Switch.run @@ fun call_sw ->
              let conn = request_conn ?observability env call_sw in
              let target =
                Awskit.Request.Target.create_exn ~scheme:`Http ~host:"127.0.0.1"
                  ~port ~path:"/" ()
              in
              let request =
                Awskit.Request.create_exn ~method_:`POST ~target ()
              in
              Awskit_eio.Runtime.Transport.with_response conn request
                ~body:request_body ~consume:(fun _response body ->
                  Awskit_eio.Runtime.Response_body.discard body) )
        with exn -> Error exn
      in
      client_result := Some result;
      ignore (Eio.Promise.try_resolve resolve_client_done () : bool));
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2.0 (fun () ->
      Eio.Promise.await client_done);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2.0 (fun () ->
      Eio.Promise.await peer_closed);
  match !client_result with
  | Some (Ok result) -> (result, true)
  | Some (Error exn) -> raise exn
  | None -> Alcotest.fail "early-failure client did not finish"

let rec read_all reader buffer =
  let chunk = Bytes.create 3 in
  match
    Awskit_eio.Runtime.Response_body.read reader chunk ~off:0
      ~len:(Bytes.length chunk)
  with
  | Error _ as error -> error
  | Ok 0 -> Ok (Buffer.contents buffer)
  | Ok n ->
      Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
      read_all reader buffer

let read_body_to_string body =
  Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      read_all reader (Buffer.create 16))

let read_body_once n body =
  let n = Int.max 0 n in
  Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      let bytes = Bytes.create n in
      match
        Awskit_eio.Runtime.Response_body.read reader bytes ~off:0 ~len:n
      with
      | Error _ as error -> error
      | Ok read -> Ok (Stdlib.Bytes.sub_string bytes 0 read))

let rec read_until_error reader =
  let bytes = Bytes.create 3 in
  match
    Awskit_eio.Runtime.Response_body.read reader bytes ~off:0
      ~len:(Bytes.length bytes)
  with
  | Error error -> Ok error
  | Ok 0 ->
      Error (Awskit.Error.Producer.body "expected response body read error")
  | Ok _ -> read_until_error reader

let assert_reader_invalidated reader =
  let bytes = Bytes.create 1 in
  match Awskit_eio.Runtime.Response_body.read reader bytes ~off:0 ~len:1 with
  | Error error ->
      Alcotest.(check bool)
        "reader invalidated" true
        (String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:"outside its scope");
      Ok ()
  | Ok _ ->
      Error
        (Awskit.Error.Producer.body
           "response body reader remained usable after read error")

let is_content_length_underflow_error error =
  String.is_substring
    (Awskit.Error.to_string_hum error)
    ~substring:"ended before declared Content-Length"

let consume_body scenario body =
  match scenario.Model.consume with
  | Model.Read_all -> read_body_to_string body
  | Model.Read_once n -> read_body_once n body
  | Model.Drop_without_read -> Ok ""
  | Model.Raise_in_consume -> raise Stdlib.Exit

let run_with_guard env scenario =
  let clock = Eio.Stdenv.clock env in
  with_loopback_server env scenario (fun endpoint ->
      try
        Eio.Time.with_timeout_exn clock 0.75 (fun () ->
            Eio.Switch.run @@ fun sw ->
            let conn = request_conn env sw in
            let request = request_for_endpoint scenario endpoint in
            Awskit_eio.Runtime.Transport.with_response conn request
              ~body:Awskit_eio.Runtime.Request_body.empty
              ~consume:(fun response response_body ->
                Alcotest.(check int)
                  "response status" scenario.status
                  (Awskit.Response.status response);
                consume_body scenario response_body))
      with Eio.Time.Timeout ->
        Error (timeout_error "runtime HTTP scenario timed out"))

let observe_run env scenario =
  try
    match run_with_guard env scenario with
    | Ok body -> Model.Observed_body body
    | Error error -> Model.Observed_error (Awskit.Error.to_string_hum error)
  with Stdlib.Exit -> Model.Observed_exception

let reader_invalidation_scenario =
  Model.scenario ~name:"eio-reader-invalidated-after-read-error" ~method_:`GET
    ~status:200
    ~framing:(Content_length { declared = 6; actual = "hello" })
    ~connection:Close ()

let run_reader_invalidation_check env =
  let clock = Eio.Stdenv.clock env in
  with_loopback_server env reader_invalidation_scenario (fun endpoint ->
      try
        Eio.Time.with_timeout_exn clock 0.75 (fun () ->
            Eio.Switch.run @@ fun sw ->
            let conn = request_conn env sw in
            let request =
              request_for_endpoint reader_invalidation_scenario endpoint
            in
            match
              Awskit_eio.Runtime.Transport.with_response conn request
                ~body:Awskit_eio.Runtime.Request_body.empty
                ~consume:(fun _response response_body ->
                  Awskit_eio.Runtime.Response_body.with_reader response_body
                    ~consume:(fun reader ->
                      match read_until_error reader with
                      | Error _ as error -> error
                      | Ok first_error -> (
                          match assert_reader_invalidated reader with
                          | Error _ as error -> error
                          | Ok () -> Error first_error)))
            with
            | Error error when is_content_length_underflow_error error -> Ok ()
            | Error _ as error -> error
            | Ok () ->
                Error
                  (Awskit.Error.Producer.body
                     "expected response body read error"))
      with Eio.Time.Timeout ->
        Error (timeout_error "reader invalidation scenario timed out"))

let test_reader_invalidated_after_read_error env () =
  match run_reader_invalidation_check env with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let no_request_scenario =
  Model.scenario ~name:"eio-loopback-no-request" ~method_:`GET ~status:200
    ~framing:Empty ~connection:Close ()

let test_loopback_server_returns_without_request env () =
  let clock = Eio.Stdenv.clock env in
  match
    Eio.Time.with_timeout_exn clock 0.25 (fun () ->
        ignore
          (with_loopback_server env no_request_scenario (fun _endpoint ->
               Error (timeout_error "synthetic no-request path"))
            : (string, Awskit.Error.t) Result.t))
  with
  | () -> ()
  | exception Eio.Time.Timeout ->
      Alcotest.fail "loopback server waited for accept after client returned"

let measurement name completion =
  completion
  |> Awskit.Observability.For_projection.Operation.Completion.measurements
  |> List.find_map ~f:(fun value ->
      if
        String.equal name
          (Awskit.Observability.For_projection.Measurement.name value)
      then
        match Awskit.Observability.For_projection.Measurement.value value with
        | Int64 value -> Some value
        | Int value -> Some (Int64.of_int value)
        | Float _ -> None
      else None)

let http_attempt_completion completions =
  match
    List.filter completions ~f:(fun completion ->
        String.equal "awskit.http.attempt"
          (completion
          |> Awskit.Observability.For_projection.Operation.Completion.info
          |> Awskit.Observability.For_projection.Operation.Info.name))
  with
  | [ completion ] -> completion
  | attempts ->
      Alcotest.failf "expected one HTTP attempt completion, got %d"
        (List.length attempts)

let assert_attempt_bytes completions ~response ~drained =
  let completion = http_attempt_completion completions in
  Alcotest.(check (option int64))
    "connector response bytes" (Some response)
    (measurement "http.connector_response_bytes" completion);
  Alcotest.(check (option int64))
    "connector drained bytes" (Some drained)
    (measurement "http.connector_drained_bytes" completion)

let assert_completion_count completions name expected =
  Alcotest.(check int)
    (name ^ " completion count")
    expected
    (List.count completions ~f:(fun completion ->
         String.equal name
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let streaming_gauge_values observability direction =
  Awskit_eio.Observability.instrument_snapshot observability
  |> List.filter_map ~f:(fun observation ->
      let module Metric = Awskit.Observability.For_projection.Metric in
      match
        ( List.map
            (Metric.Observation.labels observation)
            ~f:Metric.Label.encoded,
          Metric.Observation.value observation )
      with
      | [ observed_direction ], Int64 value
        when String.equal direction observed_direction ->
          Some value
      | _ -> None)

let health_count observer ~projection_name ~phase =
  Awskit_eio.Observability.health observer
  |> Awskit.Observability.Health.failures
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure
        |> Awskit.Observability.Health.projection
        |> Awskit.Observability.Health.Projection.name)
      && Poly.equal phase (Awskit.Observability.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      Int64.(total + Awskit.Observability.Health.count failure))

let assert_streaming_gauge_released observability direction =
  let values = streaming_gauge_values observability direction in
  Alcotest.(check bool)
    (direction ^ " streaming gauge has a snapshot")
    true
    (not (List.is_empty values));
  Alcotest.(check int64)
    (direction ^ " streaming gauge released at zero")
    0L (List.last_exn values)

let observed_http_sink ~name completions ?(finish = fun _ -> ()) () =
  Awskit_eio.Observability.Trace_sink.create ~name ~needs_clock:false
    ~enabled:(fun info ->
      String.is_prefix
        (Awskit.Observability.For_projection.Operation.Info.name info)
        ~prefix:"awskit.http.")
    ~start:(fun _ ->
      {
        Awskit_eio.Observability.Trace_sink.within =
          (fun callback -> callback ());
        correlation = [];
        finish =
          (fun completion ->
            completions := completion :: !completions;
            finish completion);
      })
    ~event_enabled:(fun _ -> false)
    ~event:(fun _ -> ())

let observed_http_observer ~name completions ?finish () =
  let trace_sink = observed_http_sink ~name completions ?finish () in
  let metric_sink =
    Awskit.Observability.Metric_sink.create ~name:(name ^ "-gauges")
      ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (Awskit.Observability.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> ())
  in
  Awskit_eio.Observability.create ~logs:false ~metric_sinks:[ metric_sink ]
    ~trace_sinks:[ trace_sink ] ()

let streaming_request_body () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
      ~replayable:true ()
  in
  Awskit_eio.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
      Awskit_eio.Runtime.Request_body.write_string writer "abc")

let run_observed_request env scenario ~observability
    ?(body = Awskit_eio.Runtime.Request_body.empty) ?max_response_drain_bytes
    ~consume () =
  with_loopback_server env scenario (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn ~observability ?max_response_drain_bytes env sw in
      let request = request_for_endpoint scenario endpoint in
      Awskit_eio.Runtime.Transport.with_response conn request ~body ~consume)

let test_static_request_body_preserves_native_semantics env () =
  let disabled_payload = "native-static-body" in
  let disabled_result, disabled_headers, disabled_body =
    run_static_body_request env ~observability:Awskit_eio.Observability.none
      ~payload:disabled_payload
      ~make_body:Awskit_eio.Runtime.Request_body.of_string
  in
  (match disabled_result with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  Alcotest.(check (option string))
    "disabled native body length header"
    (Some (Int.to_string (String.length disabled_payload)))
    (header_value "content-length" disabled_headers);
  Alcotest.(check string)
    "disabled native body bytes" disabled_payload disabled_body;
  let completions = ref [] in
  let trace_sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"static-body-capture"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.http.attempt"
          (Awskit.Observability.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let sink_calls = ref 0 in
  let metric_sink =
    Awskit.Observability.Metric_sink.create ~name:"static-body-gauge"
      ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (Awskit.Observability.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> Int.incr sink_calls)
  in
  let observability =
    Awskit_eio.Observability.create ~logs:false ~metric_sinks:[ metric_sink ]
      ~trace_sinks:[ trace_sink ] ()
  in
  let active_result, active_headers, active_body =
    run_static_body_request env ~observability ~payload:static_body_payload
      ~make_body:(fun payload ->
        Awskit_eio.Runtime.Request_body.of_bytes (Bytes.of_string payload))
  in
  (match active_result with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  Alcotest.(check (option string))
    "active native body length header"
    (Some (Int.to_string (String.length static_body_payload)))
    (header_value "content-length" active_headers);
  Alcotest.(check string)
    "active native body bytes" static_body_payload active_body;
  let request_values = streaming_gauge_values observability "request" in
  Alcotest.(check bool)
    "static request has no streaming gauge" true
    (List.is_empty request_values);
  Alcotest.(check int) "no per-chunk metric sink callbacks" 0 !sink_calls;
  let completion =
    match !completions with
    | [ completion ] -> completion
    | completions ->
        Alcotest.failf "expected one static HTTP attempt completion, got %d"
          (List.length completions)
  in
  Alcotest.(check (option int64))
    "static connector request bytes are unmeasured" None
    (measurement "http.connector_request_bytes" completion)

exception Response_consumer_failure

let response_timeout_scenario =
  Model.scenario ~name:"eio-response-body-timeout" ~method_:`POST ~status:200
    ~framing:(Content_length { declared = 5; actual = "he" })
    ~connection:Keep_alive ()

let test_response_timeout_releases_eio_gauges env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-response-timeout" completions ()
  in
  let result =
    run_observed_request env response_timeout_scenario ~observability
      ~body:(streaming_request_body ())
      ~consume:(fun _ body -> read_body_to_string body)
      ()
  in
  (match result with
  | Error error when Awskit.Error.is_timeout error -> ()
  | Error error ->
      Alcotest.failf "expected timeout, got %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "partial response unexpectedly succeeded");
  assert_streaming_gauge_released observability "request";
  assert_streaming_gauge_released observability "response";
  assert_attempt_bytes !completions ~response:2L ~drained:0L;
  assert_completion_count !completions "awskit.http.response_body.drain" 1;
  Alcotest.(check int)
    "timeout attempt completed once" 1
    (List.count !completions ~f:(fun completion ->
         String.equal "awskit.http.attempt"
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let test_response_cancellation_releases_eio_gauges env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-response-cancel" completions ()
  in
  let cancelled =
    try
      match
        with_loopback_server env response_timeout_scenario (fun endpoint ->
            Eio.Cancel.sub (fun context ->
                Eio.Switch.run @@ fun sw ->
                Eio.Fiber.fork ~sw (fun () ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
                    Eio.Cancel.cancel context (Failure "test cancellation"));
                let conn = request_conn ~observability env sw in
                let request =
                  request_for_endpoint response_timeout_scenario endpoint
                in
                Awskit_eio.Runtime.Transport.with_response conn request
                  ~body:(streaming_request_body ()) ~consume:(fun _ body ->
                    read_body_to_string body))
            |> Result.map ~f:(fun _ -> ()))
      with
      | Ok () | Error _ -> false
    with Eio.Cancel.Cancelled _ -> true
  in
  Alcotest.(check bool) "Eio cancellation remains native" true cancelled;
  assert_streaming_gauge_released observability "request";
  assert_streaming_gauge_released observability "response";
  assert_attempt_bytes !completions ~response:2L ~drained:0L;
  assert_completion_count !completions "awskit.http.response_body.drain" 1;
  Alcotest.(check int)
    "cancelled attempt completed once" 1
    (List.count !completions ~f:(fun completion ->
         String.equal "awskit.http.attempt"
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let consumer_failure_scenario =
  Model.scenario ~name:"eio-response-consumer-failure" ~method_:`POST
    ~status:200
    ~framing:(Content_length { declared = 5; actual = "hello" })
    ~connection:Keep_alive ()

let test_response_consumer_failure_releases_eio_gauges env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-consumer-failure" completions ()
  in
  let raised =
    try
      ignore
        (run_observed_request env consumer_failure_scenario ~observability
           ~body:(streaming_request_body ())
           ~consume:(fun _ body ->
             Awskit_eio.Runtime.Response_body.with_reader body
               ~consume:(fun _ -> raise Response_consumer_failure))
           ()
          : (unit, Awskit.Error.t) Result.t);
      false
    with Response_consumer_failure -> true
  in
  Alcotest.(check bool) "consumer exception is preserved" true raised;
  assert_streaming_gauge_released observability "request";
  assert_streaming_gauge_released observability "response";
  assert_attempt_bytes !completions ~response:0L ~drained:5L;
  assert_completion_count !completions "awskit.http.response_body.drain" 1;
  Alcotest.(check int)
    "consumer-failure attempt completed once" 1
    (List.count !completions ~f:(fun completion ->
         String.equal "awskit.http.attempt"
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let test_response_drain_failure_releases_eio_gauges env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-drain-failure" completions ()
  in
  let result =
    run_observed_request env consumer_failure_scenario ~observability
      ~body:(streaming_request_body ())
      ~max_response_drain_bytes:3
      ~consume:(fun _ body -> Awskit_eio.Runtime.Response_body.discard body)
      ()
  in
  (match result with
  | Error error ->
      Alcotest.(check bool)
        "drain limit remains the SDK error" true
        (String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:"response body exceeded max_response_drain_bytes")
  | Ok () -> Alcotest.fail "drain limit unexpectedly succeeded");
  assert_streaming_gauge_released observability "request";
  assert_streaming_gauge_released observability "response";
  (* Match the Lwt raw-drain contract: once the configured cap is consumed,
     probe one more byte so overflow is observable. The connector count
     includes every byte pulled; the cap bounds successful cleanup, not the
     physical reads used to detect overflow. *)
  assert_attempt_bytes !completions ~response:0L ~drained:4L;
  assert_completion_count !completions "awskit.http.response_body.drain" 1;
  Alcotest.(check int)
    "drain-failure attempt completed once" 1
    (List.count !completions ~f:(fun completion ->
         String.equal "awskit.http.attempt"
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let test_throwing_http_sink_preserves_result_and_releases_gauges env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-throwing-http-sink" completions
      ~finish:(fun _ -> raise (Failure "sink failure"))
      ()
  in
  let scenario =
    Model.scenario ~name:"eio-throwing-http-sink-request" ~method_:`POST
      ~status:200 ~framing:Empty ~connection:Close ()
  in
  let result =
    run_observed_request env scenario ~observability
      ~body:(streaming_request_body ())
      ~consume:(fun _ body -> Awskit_eio.Runtime.Response_body.discard body)
      ()
  in
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "sink changed SDK result: %a" Awskit.Error.pp error);
  assert_streaming_gauge_released observability "request";
  assert_streaming_gauge_released observability "response";
  Alcotest.(check bool)
    "throwing sink is contained" true
    Int64.(
      health_count observability ~projection_name:"eio-throwing-http-sink"
        ~phase:Awskit.Observability.Health.Finish
      > 0L)

let test_producer_failure_closes_unconsumed_response env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-producer-failure" completions ()
  in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
      ~replayable:true ()
  in
  let result, peer_closed =
    run_early_failure_request ~observability env
      ~request_body:
        (Awskit_eio.Runtime.Request_body.of_stream descriptor ~write:(fun _ ->
             raise (Failure "producer failure")))
      ~response_headers:[ ("Content-Length", "5"); ("Connection", "close") ]
      ~response_body:"hello"
  in
  Alcotest.(check bool)
    "producer failure closes the attempt socket" true peer_closed;
  (match result with
  | Error error ->
      Alcotest.(check bool)
        "producer error remains primary" true
        (String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:"producer failure")
  | Ok () -> Alcotest.fail "producer failure unexpectedly succeeded");
  assert_attempt_bytes !completions ~response:0L ~drained:5L

let test_malformed_framing_closes_unconsumed_response env () =
  let completions = ref [] in
  let observability =
    observed_http_observer ~name:"eio-framing-failure" completions ()
  in
  let result, peer_closed =
    run_early_failure_request ~observability env
      ~request_body:Awskit_eio.Runtime.Request_body.empty
      ~response_headers:[ ("Content-Length", "nope"); ("Connection", "close") ]
      ~response_body:"hello"
  in
  Alcotest.(check bool)
    "malformed framing closes the attempt socket" true peer_closed;
  (match result with
  | Error error ->
      Alcotest.(check bool)
        "framing error remains primary" true
        (String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:"invalid response Content-Length header")
  | Ok () -> Alcotest.fail "malformed framing unexpectedly succeeded");
  Alcotest.(check int)
    "malformed framing has no drain phase" 0
    (List.count !completions ~f:(fun completion ->
         String.equal "awskit.http.response_body.drain"
           (completion
           |> Awskit.Observability.For_projection.Operation.Completion.info
           |> Awskit.Observability.For_projection.Operation.Info.name)))

let test_attempt_body_and_drain_bytes env () =
  let completions = ref [] in
  let observed_operations =
    [
      "awskit.http.attempt";
      "awskit.http.request_body.production";
      "awskit.http.response_headers.wait";
      "awskit.http.response_body.consumption";
      "awskit.http.response_body.drain";
    ]
  in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"attempt-capture"
      ~needs_clock:true
      ~enabled:(fun info ->
        List.mem observed_operations
          (Awskit.Observability.For_projection.Operation.Info.name info)
          ~equal:String.equal)
      ~start:(fun _start ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let metric_sink =
    Awskit.Observability.Metric_sink.create ~name:"streaming-capture"
      ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (Awskit.Observability.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> Alcotest.fail "instrument sink ran on the body path")
  in
  let ticks = ref 0L in
  let observability =
    Awskit_eio.Observability.create
      ~clock:(fun () ->
        let value = !ticks in
        ticks := Int64.succ value;
        value)
      ~metric_sinks:[ metric_sink ] ~trace_sinks:[ sink ] ()
  in
  let scenario =
    Model.scenario ~name:"eio-observed-attempt-bytes" ~method_:`POST ~status:429
      ~framing:(Content_length { declared = 6; actual = "abcdef" })
      ~connection:Close ~consume:(Read_once 2) ()
  in
  let request_live_snapshot = ref None in
  let result =
    with_loopback_server env scenario (fun endpoint ->
        Eio.Switch.run @@ fun sw ->
        let conn = request_conn ~observability env sw in
        let request = request_for_endpoint scenario endpoint in
        let descriptor =
          Awskit.Body.Request.descriptor_exn ~content_length:3L
            ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
            ~replayable:true ()
        in
        Awskit_eio.Runtime.Transport.with_response conn request
          ~body:
            (Awskit_eio.Runtime.Request_body.of_stream descriptor
               ~write:(fun writer ->
                 let result =
                   Awskit_eio.Runtime.Request_body.write_string writer "abc"
                 in
                 request_live_snapshot :=
                   Some
                     (Awskit_eio.Observability.instrument_snapshot observability);
                 result))
          ~consume:(fun _ body -> read_body_once 2 body))
  in
  (match result with
  | Ok "ab" -> ()
  | Ok value -> Alcotest.failf "unexpected response %S" value
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  let completion_for name =
    List.find_exn !completions ~f:(fun completion ->
        String.equal name
          (completion
          |> Awskit.Observability.For_projection.Operation.Completion.info
          |> Awskit.Observability.For_projection.Operation.Info.name))
  in
  List.iter observed_operations ~f:(fun name ->
      Alcotest.(check int)
        (name ^ " completed once") 1
        (List.count !completions ~f:(fun completion ->
             String.equal name
               (completion
               |> Awskit.Observability.For_projection.Operation.Completion.info
               |> Awskit.Observability.For_projection.Operation.Info.name))));
  List.iter (List.tl_exn observed_operations) ~f:(fun name ->
      Alcotest.(check string)
        (name ^ " outcome") "ok"
        (completion_for name
        |> Awskit.Observability.For_projection.Operation.Completion.outcome
        |> Awskit.Observability.Outcome.to_string));
  let completion = completion_for "awskit.http.attempt" in
  Alcotest.(check string)
    "physical attempt outcome" "throttled"
    (completion
    |> Awskit.Observability.For_projection.Operation.Completion.outcome
    |> Awskit.Observability.Outcome.to_string);
  Alcotest.(check (option int64))
    "connector request bytes" (Some 3L)
    (measurement "http.connector_request_bytes" completion);
  Alcotest.(check (option int64))
    "connector response bytes" (Some 2L)
    (measurement "http.connector_response_bytes" completion);
  Alcotest.(check (option int64))
    "connector drained bytes" (Some 4L)
    (measurement "http.connector_drained_bytes" completion);
  let live_request_values =
    Option.value_exn !request_live_snapshot
    |> List.filter_map ~f:(fun observation ->
        let module Metric = Awskit.Observability.For_projection.Metric in
        match
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
        with
        | [ direction ], Int64 value when String.equal "request" direction ->
            Some value
        | _ -> None)
  in
  Alcotest.(check bool)
    "request streaming gauge is live in producer" true
    (List.exists live_request_values ~f:(fun value -> Int64.(value > 0L)));
  let streaming_values direction =
    Awskit_eio.Observability.instrument_snapshot observability
    |> List.filter_map ~f:(fun observation ->
        let module Metric = Awskit.Observability.For_projection.Metric in
        match
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
        with
        | [ observed_direction ], Int64 value
          when String.equal direction observed_direction ->
            Some value
        | _ -> None)
  in
  List.iter [ "request"; "response" ] ~f:(fun direction ->
      let values = streaming_values direction in
      Alcotest.(check bool)
        (direction ^ " streaming gauge is present")
        true
        (not (List.is_empty values));
      Alcotest.(check bool)
        (direction ^ " streaming gauge stayed non-negative")
        true
        (List.for_all values ~f:Int64.is_non_negative);
      Alcotest.(check int64)
        (direction ^ " streaming gauge returned to zero")
        0L (List.last_exn values))

let suite env =
  let module Target = struct
    let name = "awskit-eio"
    let ensure_loopback_available () = ensure_loopback_listener_available env
    let run_scenario scenario = observe_run env scenario
  end in
  let module Workload = Runtime_http_workload.Make (Target) in
  List.map Workload.suite ~f:(fun (name, cases) ->
      if String.equal name "workload:awskit-eio:runtime-http" then
        ( name,
          cases
          @ [
              Alcotest.test_case "read error invalidates reader" `Quick
                (test_reader_invalidated_after_read_error env);
              Alcotest.test_case "loopback server returns without request"
                `Quick
                (test_loopback_server_returns_without_request env);
              Alcotest.test_case
                "static request body preserves native semantics" `Quick
                (test_static_request_body_preserves_native_semantics env);
              Alcotest.test_case "response timeout releases Eio gauges" `Quick
                (test_response_timeout_releases_eio_gauges env);
              Alcotest.test_case "response cancellation releases Eio gauges"
                `Quick
                (test_response_cancellation_releases_eio_gauges env);
              Alcotest.test_case "response consumer failure releases Eio gauges"
                `Quick
                (test_response_consumer_failure_releases_eio_gauges env);
              Alcotest.test_case "response drain failure releases Eio gauges"
                `Quick
                (test_response_drain_failure_releases_eio_gauges env);
              Alcotest.test_case "throwing HTTP sink preserves result" `Quick
                (test_throwing_http_sink_preserves_result_and_releases_gauges
                   env);
              Alcotest.test_case "producer failure closes unconsumed response"
                `Quick
                (test_producer_failure_closes_unconsumed_response env);
              Alcotest.test_case "malformed framing closes unconsumed response"
                `Quick
                (test_malformed_framing_closes_unconsumed_response env);
              Alcotest.test_case "attempt body and drain byte accounting" `Quick
                (test_attempt_body_and_drain_bytes env);
            ] )
      else (name, cases))

let () = Eio_main.run @@ fun env -> Alcotest.run "awskit-eio" (suite env)
