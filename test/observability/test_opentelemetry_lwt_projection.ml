open Base
module O = Awskit.Observability
module Bridge = Awskit_observability_opentelemetry_lwt
module Fixture = Observability_lwt_fixture
module Otel = Opentelemetry

type capture = {
  mutable spans : Otel.Span.t list;
  mutable metrics : Otel.Metrics.t list;
}

let exporter ?(fail = false) capture : Otel.Exporter.t =
  {
    Otel.Exporter.export =
      (fun signal ->
        if fail then failwith "export failed";
        match signal with
        | Otel.Any_signal_l.Spans values ->
            capture.spans <- List.rev_append values capture.spans
        | Metrics values ->
            capture.metrics <- List.rev_append values capture.metrics
        | Logs _ -> ());
    active = (fun () -> Otel.Aswitch.dummy);
    shutdown = (fun () -> ());
    self_metrics = (fun () -> []);
  }

let bridge ?fail capture =
  let exporter = exporter ?fail capture in
  Bridge.create
    ~tracer:(Otel.Exporter.get_tracer exporter)
    ~meter:(Otel.Exporter.get_meter exporter)
    ()

let metric_attribute_names (metric : Otel.Metrics.t) =
  match metric.data with
  | Some (Histogram histogram) ->
      List.concat_map histogram.data_points ~f:(fun point ->
          List.map point.attributes ~f:(fun attribute -> attribute.key))
  | Some (Gauge gauge) ->
      List.concat_map gauge.data_points ~f:(fun point ->
          List.map point.attributes ~f:(fun attribute -> attribute.key))
  | Some (Sum sum) ->
      List.concat_map sum.data_points ~f:(fun point ->
          List.map point.attributes ~f:(fun attribute -> attribute.key))
  | Some (Exponential_histogram _ | Summary _) | None -> []

let span capture name =
  List.find_exn capture.spans ~f:(fun (span : Otel.Span.t) ->
      String.equal span.name name)

let spans capture name =
  List.filter capture.spans ~f:(fun (span : Otel.Span.t) ->
      String.equal span.name name)

let attribute span name =
  List.Assoc.find (Otel.Span.attrs span) name ~equal:String.equal

let check_string_attribute label span name expected =
  match attribute span name with
  | Some (`String actual) -> Alcotest.(check string) label expected actual
  | Some (`Int _ | `Bool _ | `Float _ | `None) | None ->
      Alcotest.failf "%s was not a string attribute" name

let check_int_attribute label span name expected =
  match attribute span name with
  | Some (`Int actual) -> Alcotest.(check int) label expected actual
  | Some (`String _ | `Bool _ | `Float _ | `None) | None ->
      Alcotest.failf "%s was not an integer attribute" name

let check_kind label span expected =
  Alcotest.(check bool)
    label true
    (Option.value_map (Otel.Span.kind span) ~default:false
       ~f:(Poly.equal expected))

let check_unset_status label span =
  Alcotest.(check bool) label true (Option.is_none (Otel.Span.status span))

let check_error_status label span =
  Alcotest.(check bool)
    label true
    (Option.value_map (Otel.Span.status span) ~default:false ~f:(fun status ->
         Poly.equal status.code Otel.Span_status.Status_code_error))

let check_child label ~(parent : Otel.Span.t) (child : Otel.Span.t) =
  Alcotest.(check bool)
    label true
    (Bytes.equal child.parent_span_id
       (parent |> Otel.Span.id |> Otel.Span_id.to_bytes))

let canonical_projection () =
  Bridge.setup_ambient_context ();
  let capture = { spans = []; metrics = [] } in
  let bridge = bridge capture in
  let now = ref 0L in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () ->
        let value = !now in
        (now := Int64.(value + 25L));
        value)
      ~metric_sinks:[ Bridge.metric_sink bridge ]
      ~trace_sinks:[ Bridge.trace_sink bridge ]
      ()
  in
  Fixture.set_response ~status:200
    ~headers:[ ("x-amz-request-id", "R2") ]
    "payload";
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK result" true (Result.is_ok result);
  let operation_span = span capture "S3.GetObject" in
  let attempt_span = span capture "awskit.s3.attempt" in
  let credentials_span = span capture "awskit.credentials.resolve" in
  let signing_span = span capture "awskit.s3.signing" in
  let http_span = span capture "GET" in
  let headers_span = span capture "awskit.http.response_headers.wait" in
  let consumption_span = span capture "awskit.http.response_body.consumption" in
  Alcotest.(check string) "AWS SDK span name" "S3.GetObject" operation_span.name;
  check_kind "AWS SDK span kind" operation_span Otel.Span.Span_kind_client;
  check_kind "HTTP span kind" http_span Otel.Span.Span_kind_client;
  List.iter
    [
      attempt_span;
      credentials_span;
      signing_span;
      headers_span;
      consumption_span;
    ] ~f:(fun span ->
      check_kind (span.name ^ " kind") span Otel.Span.Span_kind_internal);
  check_child "attempt belongs to SDK operation" ~parent:operation_span
    attempt_span;
  List.iter [ credentials_span; signing_span; http_span ] ~f:(fun child ->
      check_child
        (child.name ^ " belongs to attempt")
        ~parent:attempt_span child);
  List.iter [ headers_span; consumption_span ] ~f:(fun child ->
      check_child
        (child.name ^ " belongs to HTTP request")
        ~parent:http_span child);
  check_string_attribute "AWS RPC system" operation_span "rpc.system.name"
    "aws-api";
  check_string_attribute "AWS RPC method" operation_span "rpc.method"
    "S3.GetObject";
  check_string_attribute "HTTP method" http_span "http.request.method" "GET";
  check_int_attribute "HTTP response status" http_span
    "http.response.status_code" 200;
  check_string_attribute "AWS request ID" http_span "aws.request_id" "R2";
  check_string_attribute "HTTP span correlation" http_span "span.id"
    (http_span |> Otel.Span.id |> Otel.Span_id.to_hex);
  check_unset_status "successful AWS span status" operation_span;
  check_unset_status "successful HTTP span status" http_span;
  List.iter [ "url.full"; "server.address"; "server.port" ] ~f:(fun name ->
      Alcotest.(check bool)
        (name ^ " is not inferred")
        true
        (Option.is_none (attribute http_span name)));
  let span_attribute_names =
    operation_span |> Otel.Span.attrs |> List.map ~f:fst
  in
  Alcotest.(check bool)
    "trace correlation projected" true
    (List.mem span_attribute_names "trace.id" ~equal:String.equal);
  let metric_names =
    List.map capture.metrics ~f:(fun (metric : Otel.Metrics.t) -> metric.name)
    |> Set.of_list (module String)
  in
  List.iter
    [
      "awskit.s3.operations";
      "awskit.s3.attempts";
      "awskit.s3.logical_response_bytes";
    ] ~f:(fun name ->
      Alcotest.(check bool) (name ^ " emitted") true (Set.mem metric_names name));
  let metric_attributes =
    List.concat_map capture.metrics ~f:metric_attribute_names
  in
  List.iter [ "aws.request_id"; "trace.id"; "span.id" ] ~f:(fun name ->
      Alcotest.(check bool)
        (name ^ " excluded from metrics")
        false
        (List.mem metric_attributes name ~equal:String.equal))

let retry_semantics () =
  Bridge.setup_ambient_context ();
  let capture = { spans = []; metrics = [] } in
  let bridge = bridge capture in
  let now = ref 0L in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () ->
        let value = !now in
        (now := Int64.(value + 25L));
        value)
      ~trace_sinks:[ Bridge.trace_sink bridge ]
      ()
  in
  let retry_policy =
    Awskit.Retry.create_exn ~max_attempts:2 ~base_delay:Ptime.Span.zero
      ~jitter:0. ()
  in
  Fixture.set_responses
    [
      ( 503,
        [ ("x-amz-request-id", "R1") ],
        "<Error><Code>SlowDown</Code><Message>retry</Message></Error>" );
      (206, [ ("x-amz-request-id", "R2") ], "payload");
    ];
  let result =
    Fixture.connection ~retry_policy ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "retrying SDK result" true (Result.is_ok result);
  let logical = span capture "S3.GetObject" in
  let attempts = spans capture "awskit.s3.attempt" in
  let http_attempts =
    spans capture "GET"
    |> List.sort ~compare:(fun left right ->
        let attempt span =
          match attribute span "attempt" with
          | Some (`Int value) -> value
          | Some (`String _ | `Bool _ | `Float _ | `None) | None ->
              Alcotest.fail "HTTP span did not inherit its attempt number"
        in
        Int.compare (attempt left) (attempt right))
  in
  Alcotest.(check int) "two S3 attempt spans" 2 (List.length attempts);
  Alcotest.(check int) "two HTTP attempt spans" 2 (List.length http_attempts);
  let first_http = List.nth_exn http_attempts 0 in
  let second_http = List.nth_exn http_attempts 1 in
  check_int_attribute "first attempt number" first_http "attempt" 1;
  check_int_attribute "first response status" first_http
    "http.response.status_code" 503;
  check_string_attribute "first response ID" first_http "aws.request_id" "R1";
  check_string_attribute "first error type" first_http "error.type" "503";
  check_error_status "failed physical attempt status" first_http;
  Alcotest.(check bool)
    "initial request has no resend count" true
    (Option.is_none (attribute first_http "http.request.resend_count"));
  check_int_attribute "second attempt number" second_http "attempt" 2;
  check_int_attribute "second response status" second_http
    "http.response.status_code" 206;
  check_string_attribute "second response ID" second_http "aws.request_id" "R2";
  check_int_attribute "second request resend count" second_http
    "http.request.resend_count" 1;
  Alcotest.(check bool)
    "successful retry has no error type" true
    (Option.is_none (attribute second_http "error.type"));
  check_unset_status "successful retry status" second_http;
  Alcotest.(check bool)
    "handled retry does not fail logical span" true
    (Option.is_none (attribute logical "error.type"));
  check_unset_status "handled retry logical status" logical;
  let first_attempt =
    List.find_exn attempts ~f:(fun attempt ->
        match attribute attempt "attempt" with
        | Some (`Int 1) -> true
        | Some (`Int _ | `String _ | `Bool _ | `Float _ | `None) | None -> false)
  in
  Alcotest.(check bool)
    "retry event belongs to failed attempt" true
    (List.exists (Otel.Span.events first_attempt)
       ~f:(fun (event : Otel.Event.t) ->
         String.equal event.name "awskit.s3.retry.scheduled"))

let cancellation_is_not_an_error () =
  Bridge.setup_ambient_context ();
  let capture = { spans = []; metrics = [] } in
  let bridge = bridge capture in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Bridge.trace_sink bridge ]
      ()
  in
  let pending, _resolver = Lwt.task () in
  Fixture.set_pending pending;
  let request =
    Fixture.connection ~observability:observer () |> Fixture.get_value
  in
  Lwt.cancel request;
  (match Lwt_main.run request with
  | exception Lwt.Canceled -> ()
  | Ok _ | Error _ -> Alcotest.fail "cancelled request returned a result"
  | exception exn ->
      Alcotest.failf "cancelled request raised %s" (Exn.to_string exn));
  let http_span = span capture "GET" in
  check_unset_status "cancelled HTTP span status" http_span;
  Alcotest.(check bool)
    "cancelled HTTP span has no error type" true
    (Option.is_none (attribute http_span "error.type"))

let terminal_service_error_is_recorded () =
  Bridge.setup_ambient_context ();
  let capture = { spans = []; metrics = [] } in
  let bridge = bridge capture in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Bridge.trace_sink bridge ]
      ()
  in
  Fixture.set_response ~status:404
    "<Error><Code>NoSuchKey</Code><Message>missing</Message></Error>";
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK result is an error" true (Result.is_error result);
  let operation_span = span capture "S3.GetObject" in
  let http_span = span capture "GET" in
  check_string_attribute "SDK error type" operation_span "error.type"
    "not_found";
  check_error_status "terminal SDK error status" operation_span;
  check_string_attribute "HTTP error type" http_span "error.type" "404";
  check_error_status "terminal HTTP error status" http_span

let failure_count snapshot ~projection_name ~phase =
  O.Health.failures snapshot
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let exporter_failure_is_observational () =
  Bridge.setup_ambient_context ();
  let bridge = bridge ~fail:true { spans = []; metrics = [] } in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Bridge.trace_sink bridge ]
      ()
  in
  Fixture.set_response ~status:200 "payload";
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK result preserved" true (Result.is_ok result);
  let health = Awskit_lwt.Observability.health observer in
  Alcotest.(check bool)
    "export failure attributed to trace finish" true
    Int64.(
      failure_count health ~projection_name:"opentelemetry.trace" ~phase:Finish
      > 0L)

let () =
  Alcotest.run "awskit-observability-opentelemetry-lwt"
    [
      ( "behavior:opentelemetry-projection",
        [
          Alcotest.test_case "safe traces and exact metrics" `Quick
            canonical_projection;
          Alcotest.test_case "retry topology and semantics" `Quick
            retry_semantics;
          Alcotest.test_case "caller cancellation is not an error" `Quick
            cancellation_is_not_an_error;
          Alcotest.test_case "terminal service error is recorded" `Quick
            terminal_service_error_is_recorded;
          Alcotest.test_case "exporter failure containment" `Quick
            exporter_failure_is_observational;
        ] );
    ]
