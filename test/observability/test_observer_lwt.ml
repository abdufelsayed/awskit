open Base
module O = Awskit.Observability
module F = O.For_service
module Fixture = Observability_lwt_fixture

let disabled_is_lazy () =
  Fixture.reset_calls ();
  Fixture.set_response ~status:200 "payload";
  let result =
    Fixture.connection ~observability:Awskit_lwt.Observability.none ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool)
    "hard-off preserves SDK result" true (Result.is_ok result);
  Alcotest.(check int) "hard-off performs transport" 1 (Fixture.call_count ());
  Alcotest.(check int)
    "hard-off health is empty" 0
    (Awskit_lwt.Observability.health Awskit_lwt.Observability.none
    |> O.Health.failures
    |> List.length)

let operation_name completion =
  completion
  |> O.For_projection.Operation.Completion.info
  |> O.For_projection.Operation.Info.name

let health_count observer ~projection_name ~phase =
  Awskit_lwt.Observability.health observer
  |> O.Health.failures
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let hostile_wrapper_preserves_invocation_and_result () =
  let completions = ref [] in
  let wrapper_calls = ref 0 in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"hostile-wrapper"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        let within : type a. (unit -> a Lwt.t) -> a Lwt.t =
         fun callback ->
          Int.incr wrapper_calls;
          let first = callback () in
          ignore (callback () : a Lwt.t);
          Lwt.bind first (fun _ -> Lwt.fail (Failure "wrapper substituted"))
        in
        {
          Awskit_lwt.Observability.Trace_sink.within;
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_lwt.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.reset_calls ();
  Fixture.set_response ~status:200 "payload";
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check (result string string))
    "real SDK result wins" (Ok "payload")
    (Result.map_error result ~f:Awskit.Error.to_string_hum);
  Alcotest.(check int) "wrapper activated once" 1 !wrapper_calls;
  Alcotest.(check int) "transport invoked once" 1 (Fixture.call_count ());
  Alcotest.(check int) "completion exactly once" 1 (List.length !completions);
  Alcotest.(check string)
    "logical completion" "awskit.s3.operation"
    (operation_name (List.hd_exn !completions));
  Alcotest.(check bool)
    "context violation attributed" true
    Int64.(
      health_count observer ~projection_name:"hostile-wrapper" ~phase:Context
      > 0L)

let synchronous_wrapper_raise_falls_back () =
  let callback_calls = ref 0 in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"synchronous-raise"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun _ -> failwith "wrapper raised before callback");
          correlation = [];
          finish = (fun _ -> ());
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_lwt.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.reset_calls ();
  Fixture.set_response ~status:200 "payload";
  let result =
    Fixture.connection ~observability:observer () |> fun connection ->
    Lwt.bind (Fixture.get_value connection) (fun result ->
        Int.incr callback_calls;
        Lwt.return result)
    |> Lwt_main.run
  in
  Alcotest.(check bool)
    "SDK still returns its result" true (Result.is_ok result);
  Alcotest.(check int) "transport invoked once" 1 (Fixture.call_count ());
  Alcotest.(check int) "result continuation invoked once" 1 !callback_calls;
  Alcotest.(check bool)
    "synchronous wrapper raise attributed" true
    Int64.(
      health_count observer ~projection_name:"synchronous-raise" ~phase:Context
      > 0L)

let wrapper_that_never_invokes_but_returns_substitute_falls_back () =
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"no-invocation"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun _ ->
              (* A parametric wrapper cannot manufacture the callback's result
                 safely. Use an unsafe value here solely to model a hostile
                 adapter that returns a substitute without invoking it. *)
              Lwt.return (Stdlib.Obj.magic "substitute"));
          correlation = [];
          finish = (fun _ -> ());
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_lwt.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.reset_calls ();
  Fixture.set_response ~status:200 "payload";
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK still runs" true (Result.is_ok result);
  Alcotest.(check int) "transport invoked once" 1 (Fixture.call_count ());
  Alcotest.(check bool)
    "missing invocation attributed" true
    Int64.(
      health_count observer ~projection_name:"no-invocation" ~phase:Context > 0L)

let cancellation_is_native () =
  let completions = ref [] in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"cancellation"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let metric_sink =
    O.Metric_sink.create ~name:"cancellation-metrics" ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (O.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> Alcotest.fail "instrument sink ran on cancellation")
  in
  let observer =
    Awskit_lwt.Observability.create ~metric_sinks:[ metric_sink ]
      ~trace_sinks:[ sink ] ()
  in
  let pending, _ = Lwt.task () in
  Fixture.set_pending pending;
  let operation =
    Fixture.connection ~observability:observer () |> fun connection ->
    Fixture.put_string connection "payload"
  in
  Lwt.cancel operation;
  Alcotest.check_raises "Lwt cancellation preserved" Lwt.Canceled (fun () ->
      ignore (Lwt_main.run operation : _));
  Alcotest.(check int) "logical completion once" 1 (List.length !completions);
  Alcotest.(check string)
    "cancelled outcome" "cancelled"
    (List.hd_exn !completions
    |> O.For_projection.Operation.Completion.outcome
    |> O.Outcome.to_string);
  let request_streaming_values =
    Awskit_lwt.Observability.instrument_snapshot observer
    |> List.filter_map ~f:(fun observation ->
        let module Metric = O.For_projection.Metric in
        match
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
        with
        | [ "request" ], Int64 value -> Some value
        | _ -> None)
  in
  Alcotest.(check bool)
    "request streaming gauge remains inspectable" true
    (not (List.is_empty request_streaming_values));
  Alcotest.(check int64)
    "request streaming gauge released after cancellation" 0L
    (List.last_exn request_streaming_values)

let concurrent_context_isolation () =
  let completions = ref [] in
  let next_trace = ref 0 in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"context-isolation"
      ~needs_clock:false
      ~enabled:(fun _ -> true)
      ~start:(fun started ->
        let name =
          started
          |> O.For_projection.Operation.Start.info
          |> O.For_projection.Operation.Info.name
        in
        let finish completion =
          if String.equal "awskit.http.attempt" (operation_name completion) then
            completions := completion :: !completions
        in
        if String.equal name "awskit.s3.operation" then (
          Int.incr next_trace;
          let trace_id =
            O.Correlation.trace_id (Fmt.str "%032x" !next_trace)
            |> Result.ok_or_failwith
          in
          {
            Awskit_lwt.Observability.Trace_sink.within =
              (fun callback -> Lwt.bind (Lwt.pause ()) callback);
            correlation = [ trace_id ];
            finish;
          })
        else
          {
            Awskit_lwt.Observability.Trace_sink.within =
              (fun callback -> callback ());
            correlation = [];
            finish;
          })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_lwt.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.reset_calls ();
  Fixture.set_response ~status:200 "payload";
  let run () =
    Fixture.connection ~observability:observer () |> Fixture.get_value
  in
  let left, right = Lwt_main.run (Lwt.both (run ()) (run ())) in
  Alcotest.(check bool) "first SDK result" true (Result.is_ok left);
  Alcotest.(check bool) "second SDK result" true (Result.is_ok right);
  Alcotest.(check int) "two transport calls" 2 (Fixture.call_count ());
  let trace_ids completion =
    completion
    |> O.For_projection.Operation.Completion.diagnostics
    |> List.filter_map ~f:(fun diagnostic ->
        if String.equal "trace.id" (O.Diagnostic.Public.name diagnostic) then
          match O.Diagnostic.Public.value diagnostic with
          | String value -> Some value
          | Bool _ | Int _ | Int64 _ | Float _ -> None
        else None)
  in
  let ids = List.map !completions ~f:trace_ids in
  Alcotest.(check int) "two HTTP completions" 2 (List.length ids);
  Alcotest.(check bool)
    "each child inherited exactly one trace" true
    (List.for_all ids ~f:(fun values -> Int.equal 1 (List.length values)));
  Alcotest.(check int)
    "concurrent operations retained distinct traces" 2
    (List.concat ids |> Set.of_list (module String) |> Set.length)

let domain_logging_is_lazy_and_human_readable () =
  let source = Awskit_s3.Observability.Sources.operation in
  let previous_reporter = Logs.reporter () in
  let previous_level = Logs.Src.level source in
  let messages = ref [] in
  let reporter =
    {
      Logs.report =
        (fun _source level ~over k msgf ->
          let capture ?header:_ ?tags format =
            Fmt.kstr
              (fun message ->
                let completion =
                  Option.bind tags ~f:(fun tags ->
                      Logs.Tag.find O.Logs_tags.operation_completion tags)
                in
                messages := (level, message, completion) :: !messages;
                over ();
                k ())
              format
          in
          msgf capture);
    }
  in
  Exn.protect
    ~f:(fun () ->
      Logs.set_reporter reporter;
      Logs.Src.set_level source (Some Logs.Debug);
      let observer = Awskit_lwt.Observability.default () in
      Fixture.set_response ~status:200 "payload";
      let success =
        Fixture.connection ~observability:observer ()
        |> Fixture.get_value
        |> Lwt_main.run
      in
      Alcotest.(check bool) "successful operation" true (Result.is_ok success);
      Alcotest.(check int)
        "routine success log suppressed" 0 (List.length !messages);
      Fixture.set_response ~status:404
        "<Error><Code>NoSuchKey</Code><Message>missing</Message></Error>";
      let missing =
        Fixture.connection ~observability:observer ()
        |> Fixture.get_value
        |> Lwt_main.run
      in
      Alcotest.(check bool)
        "not-found result preserved" true (Result.is_error missing);
      match !messages with
      | [ (level, message, Some completion) ] ->
          Alcotest.(check bool)
            "expected outcome uses Debug" true
            (Poly.equal level Logs.Debug);
          Alcotest.(check string)
            "bounded operation appears in human message"
            "S3 GetObject finished with outcome not_found" message;
          Alcotest.(check string)
            "typed tag carries the same operation" "awskit.s3.operation"
            (operation_name completion)
      | entries ->
          Alcotest.failf "expected one tagged S3 completion log, got %d: %s"
            (List.length entries)
            (entries
            |> List.rev_map ~f:(fun (level, message, completion) ->
                Fmt.str "%a:%s:tag=%b" Logs.pp_level level message
                  (Option.is_some completion))
            |> String.concat ~sep:" | "))
    ~finally:(fun () ->
      Logs.set_reporter previous_reporter;
      Logs.Src.set_level source previous_level)

let () =
  Alcotest.run "awskit-observer-lwt"
    [
      ( "behavior:observer-lwt",
        [
          Alcotest.test_case "disabled fast path is lazy" `Quick
            disabled_is_lazy;
          Alcotest.test_case "hostile wrapper preserves SDK semantics" `Quick
            hostile_wrapper_preserves_invocation_and_result;
          Alcotest.test_case "synchronous wrapper raise falls back" `Quick
            synchronous_wrapper_raise_falls_back;
          Alcotest.test_case "missing callback substitute falls back" `Quick
            wrapper_that_never_invokes_but_returns_substitute_falls_back;
          Alcotest.test_case "native cancellation" `Quick cancellation_is_native;
          Alcotest.test_case "promise context isolation" `Quick
            concurrent_context_isolation;
          Alcotest.test_case "domain-owned completion logging" `Quick
            domain_logging_is_lazy_and_human_readable;
        ] );
    ]
