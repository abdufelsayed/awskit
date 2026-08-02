open Base
module O = Awskit.Observability
module Fixture = Observability_lwt_fixture
module F = O.For_service

module No_trace = struct
  type +'a io = 'a
  type t = unit
  type activation = unit

  let name () = "none"
  let needs_clock () = false
  let enabled () _ = false
  let start () _ = ()
  let correlation () = []
  let within () callback = callback ()
  let finish () _ = ()
  let event_enabled () _ = false
  let event () _ = ()
end

module Sync_context = struct
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

module Sync_trace_sink = struct
  include No_trace

  type +'a io = 'a Sync_context.io
end

module Sync_engine = O.For_runtime.Make (Sync_context) (Sync_trace_sink)

module Sync_runtime = struct
  type 'a io = 'a
  type connection = Sync_engine.t
  type lease = Sync_engine.lease

  let with_operation = Sync_engine.with_operation
  let emit_event = Sync_engine.emit_event
  let acquire = Sync_engine.acquire
  let add = Sync_engine.add
  let release = Sync_engine.release
  let with_instrument = Sync_engine.with_instrument
end

module Transfer_observation =
  Awskit_s3.Observability.For_transfer.Make (Sync_runtime)

let with_reporter callback =
  let observations = ref [] in
  let previous = Metrics.reporter () in
  Metrics.set_reporter
    (Metrics.cache_reporter
       ~cb:(fun source tags data ->
         observations := (source, tags, data) :: !observations)
       ());
  Awskit_observability_metrics.enable ();
  Exn.protect
    ~f:(fun () -> callback observations)
    ~finally:(fun () ->
      Awskit_observability_metrics.disable ();
      Metrics.set_reporter previous)

let source_names observations =
  List.map !observations ~f:(fun (source, _, _) -> Metrics.Src.name source)

let source observations name =
  observations
  |> List.find_exn ~f:(fun (candidate, _, _) ->
      String.equal name (Metrics.Src.name candidate))
  |> fun (source, _, _) -> source

let count_source observations name =
  List.count !observations ~f:(fun (source, _, _) ->
      String.equal name (Metrics.Src.name source))

let exact_family_labels_and_semantics () =
  with_reporter @@ fun observations ->
  let families = Hashtbl.create (module String) in
  let inventory_sink =
    O.Metric_sink.create ~name:"family-inventory" ~needs_clock:false
      ~enabled:(fun family ->
        Hashtbl.set families
          ~key:(O.For_projection.Metric.Family.name family)
          ~data:family;
        false)
      ~observe:(fun _ -> ())
  in
  Fixture.set_response ~status:200 "payload";
  let ticks = ref 0L in
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () ->
        let value = !ticks in
        ticks := Int64.succ value;
        value)
      ~metric_sinks:[ inventory_sink; Awskit_observability_metrics.sink ]
      ()
  in
  let connection = Fixture.connection ~observability:observer () in
  let result = connection |> Fixture.get_value |> Lwt_main.run in
  Alcotest.(check (result string string))
    "SDK result" (Ok "payload")
    (Result.map_error result ~f:Awskit.Error.to_string_hum);
  let presigned = connection |> Fixture.presign_get |> Lwt_main.run in
  Alcotest.(check bool)
    "presigned artifact succeeds" true (Result.is_ok presigned);
  let names = source_names observations |> Set.of_list (module String) in
  List.iter
    [
      "awskit.s3.operations";
      "awskit.s3.attempts";
      "awskit.s3.logical_response_bytes";
      "awskit.s3.artifacts";
      "awskit.s3.artifact.signings";
      "awskit.http.attempts";
      "awskit.http.connector_response_bytes";
    ] ~f:(fun name ->
      Alcotest.(check bool) (name ^ " projected") true (Set.mem names name));
  Alcotest.(check (list string))
    "logical operation labels"
    [ "aws.operation"; "outcome" ]
    (Metrics.Src.tags (source !observations "awskit.s3.operations"));
  Alcotest.(check (list string))
    "attempt labels"
    [ "aws.operation"; "outcome"; "request.replayability" ]
    (Metrics.Src.tags (source !observations "awskit.s3.attempts"));
  Alcotest.(check (list string))
    "logical byte labels" [ "aws.operation" ]
    (Metrics.Src.tags (source !observations "awskit.s3.logical_response_bytes"));
  Alcotest.(check int)
    "one logical completion" 1
    (count_source observations "awskit.s3.operations");
  Alcotest.(check int)
    "one physical S3 attempt" 1
    (count_source observations "awskit.s3.attempts");
  List.iter !observations ~f:(fun (_source, tags, _data) ->
      List.iter tags ~f:(fun tag ->
          match Metrics.value tag with
          | Metrics.V (Metrics.String, value) ->
              Alcotest.(check bool)
                "no absence sentinel" false
                (String.equal value "none")
          | V _ -> ()));
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
  let retried =
    Fixture.connection ~retry_policy ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "retrying request succeeds" true (Result.is_ok retried);
  Alcotest.(check int)
    "two logical operations after retry workload" 2
    (count_source observations "awskit.s3.operations");
  Alcotest.(check int)
    "three S3 attempts across two logical operations" 3
    (count_source observations "awskit.s3.attempts");
  let transfer_observer =
    Sync_engine.create ~logs:false
      ~clock:(fun () -> 0L)
      ~metric_sinks:[ inventory_sink; Awskit_observability_metrics.sink ]
      ()
  in
  let request_production =
    Sync_runtime.with_operation transfer_observer
      ~operation:(fun () ->
        O.For_runtime.Http.request_body_production ~bytes:(fun () -> 4L))
      ~start:(fun () -> O.For_runtime.Http.phase_start ~method_:`PUT)
      (fun () -> Ok ())
  in
  Alcotest.(check bool)
    "request production phase succeeds" true
    (Result.is_ok request_production);
  let transfer =
    Transfer_observation.with_upload transfer_observer
      ~summarize:(fun () ->
        Awskit_s3.Observability.For_transfer.{ logical_bytes = 4L; parts = 1 })
      (fun () -> Ok ())
  in
  Alcotest.(check bool) "transfer wrapper succeeds" true (Result.is_ok transfer);
  let module Family = O.For_projection.Metric.Family in
  let expected =
    let counter = Family.Counter in
    let histogram = Family.Histogram in
    let gauge = Family.Gauge in
    let int64 = Family.Int64 in
    let float = Family.Float in
    [
      ("awskit.s3.operations", counter, int64, [ "aws.operation"; "outcome" ]);
      ( "awskit.s3.operation.duration",
        histogram,
        int64,
        [ "aws.operation"; "outcome" ] );
      ("awskit.s3.logical_request_bytes", histogram, int64, [ "aws.operation" ]);
      ("awskit.s3.logical_response_bytes", histogram, int64, [ "aws.operation" ]);
      ("awskit.s3.operations_in_flight", gauge, int64, [ "aws.operation" ]);
      ( "awskit.s3.attempts",
        counter,
        int64,
        [ "aws.operation"; "outcome"; "request.replayability" ] );
      ( "awskit.s3.attempt.duration",
        histogram,
        int64,
        [ "aws.operation"; "outcome"; "request.replayability" ] );
      ( "awskit.s3.attempt_failures",
        counter,
        int64,
        [ "aws.operation"; "retry.class"; "request.replayability" ] );
      ("awskit.s3.attempts_in_flight", gauge, int64, [ "aws.operation" ]);
      ( "awskit.s3.retry.decisions",
        counter,
        int64,
        [
          "aws.operation";
          "retry.decision";
          "retry.class";
          "request.replayability";
        ] );
      ( "awskit.s3.retry.delay",
        histogram,
        float,
        [ "aws.operation"; "retry.class" ] );
      ( "awskit.s3.retry.remaining_budget",
        histogram,
        int64,
        [ "aws.operation"; "retry.decision" ] );
      ( "awskit.s3.signing.duration",
        histogram,
        int64,
        [ "aws.operation"; "outcome" ] );
      ( "awskit.s3.artifacts",
        counter,
        int64,
        [ "aws.artifact_operation"; "outcome" ] );
      ( "awskit.s3.artifact.duration",
        histogram,
        int64,
        [ "aws.artifact_operation"; "outcome" ] );
      ( "awskit.s3.artifacts_in_flight",
        gauge,
        int64,
        [ "aws.artifact_operation" ] );
      ( "awskit.s3.artifact.signings",
        counter,
        int64,
        [ "aws.artifact_operation"; "outcome" ] );
      ( "awskit.s3.artifact.signing.duration",
        histogram,
        int64,
        [ "aws.artifact_operation"; "outcome" ] );
      ( "awskit.http.attempts",
        counter,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.attempt.duration",
        histogram,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.responses",
        counter,
        int64,
        [ "http.request.method"; "http.response.status_class" ] );
      ( "awskit.http.connector_request_bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ( "awskit.http.connector_response_bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ( "awskit.http.connector_drained_bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ("awskit.http.attempts_in_flight", gauge, int64, [ "http.request.method" ]);
      ("awskit.http.streaming_bytes_in_flight", gauge, int64, [ "direction" ]);
      ( "awskit.http.request_body.production.duration",
        histogram,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.request_body.production.bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ( "awskit.http.response_headers.wait.duration",
        histogram,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.response_body.consumption.duration",
        histogram,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.response_body.consumption.bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ( "awskit.http.response_body.drain.duration",
        histogram,
        int64,
        [ "http.request.method"; "outcome" ] );
      ( "awskit.http.response_body.drain.bytes",
        histogram,
        int64,
        [ "http.request.method" ] );
      ("awskit.credentials.resolutions", counter, int64, [ "outcome" ]);
      ("awskit.credentials.resolution.duration", histogram, int64, [ "outcome" ]);
      ("awskit.credentials.resolved", counter, int64, [ "credentials.source" ]);
      ( "awskit.s3.transfers",
        counter,
        int64,
        [ "transfer.direction"; "outcome" ] );
      ( "awskit.s3.transfer.duration",
        histogram,
        int64,
        [ "transfer.direction"; "outcome" ] );
      ( "awskit.s3.transfer.logical_bytes",
        histogram,
        int64,
        [ "transfer.direction" ] );
      ("awskit.s3.transfer.parts", histogram, int64, [ "transfer.direction" ]);
      ("awskit.s3.transfers_in_flight", gauge, int64, [ "transfer.direction" ]);
    ]
  in
  Alcotest.(check int)
    "complete initial family inventory" (List.length expected)
    (Hashtbl.length families);
  let registered_sources =
    Awskit_observability_metrics.sources ()
    |> List.map ~f:(fun source -> (Metrics.Src.name source, source))
    |> Map.of_alist_exn (module String)
  in
  Alcotest.(check int)
    "one Metrics source per family" (List.length expected)
    (Map.length registered_sources);
  List.iter expected ~f:(fun (name, aggregation, number, labels) ->
      let family = Hashtbl.find_exn families name in
      Alcotest.(check bool)
        (name ^ " aggregation") true
        (Poly.equal aggregation (Family.aggregation family));
      Alcotest.(check bool)
        (name ^ " number type") true
        (Poly.equal number (Family.number family));
      Alcotest.(check (list string))
        (name ^ " exact labels") labels
        (Family.labels family |> List.map ~f:O.For_projection.Metric.Label.name);
      List.iter (Family.labels family) ~f:(fun label ->
          Alcotest.(check bool)
            (name ^ " finite nonempty label domain")
            true
            (not
               (List.is_empty
                  (O.For_projection.Metric.Label.allowed_values label))));
      let source = Map.find_exn registered_sources name in
      Alcotest.(check (list string))
        (name ^ " Metrics.Tags")
        (List.sort labels ~compare:String.compare)
        (Metrics.Src.tags source))

let failure_count snapshot ~projection_name ~phase =
  O.Health.failures snapshot
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let conflicting_family_is_rejected_without_affecting_sdk_result () =
  let labels = F.Metric.Labels.empty () in
  let family doc =
    F.Metric.Family.counter ~name:"awskit.test.conflicting_family" ~doc ~labels
      ~value:F.Metric.Number.Int64 ()
  in
  let operation family =
    F.Operation.define ~name:"awskit.test.conflicting-family"
      ~doc:"Exercise conflicting metric-family registration"
      ~source:(Logs.Src.create "awskit.test.conflicting-family")
      ~span_kind:O.Span_kind.Internal
      ~start:(fun () -> F.Fields.empty)
      ~classify:(fun terminal -> (F.Terminal.default_outcome terminal, ()))
      ~finish:(fun () -> F.Fields.empty)
      ~log:F.Log.silent_operation
      ~metrics:
        [ F.Metric.Projection.sample family ~get:(fun _ -> Some ((), 1L)) ]
      ()
  in
  Awskit_observability_metrics.enable ();
  Exn.protect
    ~f:(fun () ->
      let observer =
        Sync_engine.create ~logs:false
          ~clock:(fun () -> 0L)
          ~metric_sinks:[ Awskit_observability_metrics.sink ]
          ()
      in
      let run definition =
        Sync_runtime.with_operation observer
          ~operation:(fun () -> definition)
          ~start:(fun () -> ())
          (fun () -> "sdk-result")
      in
      Alcotest.(check string)
        "first operation result" "sdk-result"
        (run (operation (family "First descriptor")));
      Alcotest.(check string)
        "conflicting operation result preserved" "sdk-result"
        (run (operation (family "Conflicting descriptor")));
      Alcotest.(check bool)
        "descriptor conflict attributed to Metrics enablement" true
        Int64.(
          failure_count
            (Sync_engine.health observer)
            ~projection_name:"metrics" ~phase:Enablement
          > 0L))
    ~finally:Awskit_observability_metrics.disable

let instrument_snapshot_requires_explicit_poll () =
  with_reporter @@ fun observations ->
  let family =
    F.Metric.Family.gauge ~name:"awskit.test.polled_gauge"
      ~doc:"Explicitly polled gauge" ~labels:(F.Metric.Labels.empty ())
      ~value:F.Metric.Number.Int64 ()
  in
  let instrument = F.Instrument.define ~family in
  let observer =
    Sync_engine.create ~logs:false
      ~clock:(fun () -> 0L)
      ~metric_sinks:[ Awskit_observability_metrics.sink ]
      ()
  in
  let lease =
    Sync_engine.acquire observer instrument ~labels:(fun () -> ()) 3L
  in
  Alcotest.(check int)
    "acquire does not call reporter" 0
    (count_source observations "awskit.test.polled_gauge");
  let snapshot () = Sync_engine.snapshot observer in
  let snapshot_value () =
    match snapshot () with
    | [ observation ] -> (
        match O.For_projection.Metric.Observation.value observation with
        | Int64 value -> value
        | Int _ | Float _ -> Alcotest.fail "gauge changed numeric kind")
    | _ -> Alcotest.fail "expected one gauge snapshot"
  in
  Alcotest.(check int64) "snapshot sees initial state" 3L (snapshot_value ());
  Awskit_observability_metrics.poll_instruments (snapshot ());
  Alcotest.(check int)
    "first poll reports once" 1
    (count_source observations "awskit.test.polled_gauge");
  Sync_engine.add lease 2L;
  Alcotest.(check int64) "snapshot sees added state" 5L (snapshot_value ());
  Alcotest.(check int)
    "add remains off reporter path" 1
    (count_source observations "awskit.test.polled_gauge");
  Awskit_observability_metrics.poll_instruments (snapshot ());
  Sync_engine.release lease;
  Alcotest.(check int64) "release returns gauge to zero" 0L (snapshot_value ());
  Awskit_observability_metrics.poll_instruments (snapshot ());
  Alcotest.(check int)
    "each explicit poll reports once" 3
    (count_source observations "awskit.test.polled_gauge")

let reporter_failure_is_contained () =
  let previous = Metrics.reporter () in
  Awskit_observability_metrics.enable ();
  Exn.protect
    ~f:(fun () ->
      Metrics.set_reporter
        (Metrics.cache_reporter
           ~cb:(fun _ _ _ -> failwith "metrics reporter failed")
           ());
      Fixture.set_response ~status:200 "payload";
      let observer =
        Awskit_lwt.Observability.create
          ~clock:(fun () -> 0L)
          ~metric_sinks:[ Awskit_observability_metrics.sink ]
          ()
      in
      let result =
        Fixture.connection ~observability:observer ()
        |> Fixture.get_value
        |> Lwt_main.run
      in
      Alcotest.(check bool) "SDK result preserved" true (Result.is_ok result);
      let snapshot = Awskit_lwt.Observability.health observer in
      Alcotest.(check bool)
        "finish failure attributed to Metrics projection" true
        Int64.(
          failure_count snapshot ~projection_name:"metrics" ~phase:Finish > 0L);
      Alcotest.(check bool)
        "instrument state never invoked the reporter" true
        (Int64.equal
           (failure_count snapshot ~projection_name:"metrics" ~phase:Instrument)
           0L);
      let poll_failed =
        try
          observer
          |> Awskit_lwt.Observability.instrument_snapshot
          |> Awskit_observability_metrics.poll_instruments;
          false
        with _ -> true
      in
      Alcotest.(check bool)
        "application-owned poll receives reporter failure" true poll_failed;
      Alcotest.(check int64)
        "poll failure is outside observer health" 0L
        (failure_count
           (Awskit_lwt.Observability.health observer)
           ~projection_name:"metrics" ~phase:Instrument))
    ~finally:(fun () ->
      Awskit_observability_metrics.disable ();
      Metrics.set_reporter previous)

let () =
  Alcotest.run "awskit-observability-metrics"
    [
      ( "behavior:metrics-projection",
        [
          Alcotest.test_case "exact families and count semantics" `Quick
            exact_family_labels_and_semantics;
          Alcotest.test_case "reporter failure containment" `Quick
            reporter_failure_is_contained;
          Alcotest.test_case "instrument polling" `Quick
            instrument_snapshot_requires_explicit_poll;
          Alcotest.test_case "conflicting family containment" `Quick
            conflicting_family_is_rejected_without_affecting_sdk_result;
        ] );
    ]
