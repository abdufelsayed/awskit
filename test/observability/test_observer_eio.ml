open Base
module O = Awskit.Observability
module Fixture = Observability_eio_fixture

let operation_name completion =
  completion
  |> O.For_projection.Operation.Completion.info
  |> O.For_projection.Operation.Info.name

let health_count observer ~projection_name ~phase =
  Awskit_eio.Observability.health observer
  |> O.Health.failures
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let hard_off_preserves_sdk env () =
  Fixture.with_connection env ~observability:Awskit_eio.Observability.none
  @@ fun connection ~calls ->
  Alcotest.(check bool)
    "SDK result" true
    (Result.is_ok (Fixture.get_value connection));
  Alcotest.(check int) "transport invoked once" 1 (Atomic.get calls);
  Alcotest.(check int)
    "hard-off health remains empty" 0
    (Awskit_eio.Observability.health Awskit_eio.Observability.none
    |> O.Health.failures
    |> List.length)

let hostile_wrapper_preserves_invocation_and_result env () =
  let completions = ref [] in
  let wrapper_calls = ref 0 in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"hostile-wrapper"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        let within : type a. (unit -> a) -> a =
         fun callback ->
          Int.incr wrapper_calls;
          let first = callback () in
          ignore (callback () : a);
          ignore (first : a);
          failwith "wrapper substituted"
        in
        {
          Awskit_eio.Observability.Trace_sink.within;
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_eio.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.with_connection env ~observability:observer
  @@ fun connection ~calls ->
  let result = Fixture.get_value connection in
  Alcotest.(check bool) "real SDK result wins" true (Result.is_ok result);
  Alcotest.(check int) "wrapper activated once" 1 !wrapper_calls;
  Alcotest.(check int) "transport invoked once" 1 (Atomic.get calls);
  Alcotest.(check int) "completion exactly once" 1 (List.length !completions);
  Alcotest.(check string)
    "logical completion" "awskit.s3.operation"
    (operation_name (List.hd_exn !completions));
  Alcotest.(check bool)
    "context violation attributed" true
    Int64.(
      health_count observer ~projection_name:"hostile-wrapper" ~phase:Context
      > 0L)

let synchronous_wrapper_raise_falls_back env () =
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"synchronous-raise"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun _ -> failwith "wrapper raised before callback");
          correlation = [];
          finish = (fun _ -> ());
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_eio.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.with_connection env ~observability:observer
  @@ fun connection ~calls ->
  Alcotest.(check bool)
    "SDK still returns its result" true
    (Result.is_ok (Fixture.get_value connection));
  Alcotest.(check int) "transport invoked once" 1 (Atomic.get calls);
  Alcotest.(check bool)
    "synchronous wrapper raise attributed" true
    Int64.(
      health_count observer ~projection_name:"synchronous-raise" ~phase:Context
      > 0L)

let missing_invocation_with_substitute_falls_back env () =
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"no-invocation"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun _ ->
              (* A parametric wrapper cannot manufacture the callback's result
                 safely. Use an unsafe value here solely to model a hostile
                 adapter that returns a substitute without invoking it. *)
              Stdlib.Obj.magic "substitute");
          correlation = [];
          finish = (fun _ -> ());
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_eio.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.with_connection env ~observability:observer
  @@ fun connection ~calls ->
  Alcotest.(check bool)
    "SDK still runs" true
    (Result.is_ok (Fixture.get_value connection));
  Alcotest.(check int) "transport invoked once" 1 (Atomic.get calls);
  Alcotest.(check bool)
    "missing invocation attributed" true
    Int64.(
      health_count observer ~projection_name:"no-invocation" ~phase:Context > 0L)

let cancellation_is_native env () =
  let completions = ref [] in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"cancellation"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.operation"
          (O.For_projection.Operation.Info.name info))
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
  let observer = Awskit_eio.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.with_connection env ~response_delay:0.25 ~observability:observer
  @@ fun connection ~calls:_ ->
  let cancelled =
    match
      Eio.Cancel.sub (fun context ->
          Eio.Switch.run @@ fun sw ->
          Eio.Fiber.fork ~sw (fun () ->
              Eio.Fiber.yield ();
              Eio.Cancel.cancel context (Failure "test cancellation"));
          Fixture.get_value connection)
    with
    | exception Eio.Cancel.Cancelled _ -> true
    | Ok _ | Error _ -> false
    | exception _ -> false
  in
  Alcotest.(check bool) "Eio cancellation preserved" true cancelled;
  Alcotest.(check int)
    "logical completion exactly once" 1 (List.length !completions);
  Alcotest.(check string)
    "cancelled outcome" "cancelled"
    (List.hd_exn !completions
    |> O.For_projection.Operation.Completion.outcome
    |> O.Outcome.to_string)

let concurrent_context_isolation env () =
  let http_completions = ref [] in
  let next_trace = Atomic.make 0 in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"context"
      ~needs_clock:false
      ~enabled:(fun _ -> true)
      ~start:(fun started ->
        let name =
          started
          |> O.For_projection.Operation.Start.info
          |> O.For_projection.Operation.Info.name
        in
        let correlation =
          if String.equal name "awskit.s3.operation" then
            let number = Atomic.fetch_and_add next_trace 1 + 1 in
            O.Correlation.trace_id (Fmt.str "%032x" number)
            |> Result.ok
            |> Option.to_list
          else []
        in
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun callback ->
              Eio.Fiber.yield ();
              callback ());
          correlation;
          finish =
            (fun completion ->
              if String.equal "awskit.http.attempt" (operation_name completion)
              then http_completions := completion :: !http_completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer = Awskit_eio.Observability.create ~trace_sinks:[ sink ] () in
  Fixture.with_connection env ~response_delay:0.01 ~observability:observer
  @@ fun first ~calls:_ ->
  Fixture.with_connection env ~response_delay:0.01 ~observability:observer
  @@ fun second ~calls:_ ->
  let results =
    Eio.Switch.run @@ fun sw ->
    List.map [ first; second ] ~f:(fun connection ->
        Eio.Fiber.fork_promise ~sw (fun () -> Fixture.get_value connection))
    |> List.map ~f:Eio.Promise.await_exn
  in
  Alcotest.(check int) "two SDK results" 2 (List.length results);
  Alcotest.(check bool)
    "both SDK calls succeed" true
    (List.for_all results ~f:Result.is_ok);
  let trace_ids =
    List.filter_map !http_completions ~f:(fun completion ->
        completion
        |> O.For_projection.Operation.Completion.diagnostics
        |> List.find_map ~f:(fun diagnostic ->
            if String.equal "trace.id" (O.Diagnostic.Public.name diagnostic)
            then
              match O.Diagnostic.Public.value diagnostic with
              | String value -> Some value
              | Bool _ | Int _ | Int64 _ | Float _ -> None
            else None))
    |> Set.of_list (module String)
  in
  Alcotest.(check int) "two HTTP completions" 2 (List.length !http_completions);
  Alcotest.(check int) "fiber-local logical parents" 2 (Set.length trace_ids)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-observer-eio"
    [
      ( "behavior:observer-eio",
        [
          Alcotest.test_case "hard-off preserves SDK behavior" `Quick
            (hard_off_preserves_sdk env);
          Alcotest.test_case "hostile wrapper preserves SDK semantics" `Quick
            (hostile_wrapper_preserves_invocation_and_result env);
          Alcotest.test_case "synchronous wrapper raise falls back" `Quick
            (synchronous_wrapper_raise_falls_back env);
          Alcotest.test_case "missing callback substitute falls back" `Quick
            (missing_invocation_with_substitute_falls_back env);
          Alcotest.test_case "native cancellation" `Quick
            (cancellation_is_native env);
          Alcotest.test_case "fiber context isolation" `Quick
            (concurrent_context_isolation env);
        ] );
    ]
