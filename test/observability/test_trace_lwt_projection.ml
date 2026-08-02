open Base
module O = Awskit.Observability
module Fixture = Observability_lwt_fixture

type Trace_core.span += Test_span of int

type state = {
  mutable next : int;
  mutable entered : string list;
  mutable exited : int;
  mutable child_parents : int;
  mutable data : (string * Trace_core.user_data) list list;
}

let collector state =
  let callbacks =
    Trace_core.Collector.Callbacks.make
      ~enter_span:(fun
          state
          ~__FUNCTION__:_
          ~__FILE__:_
          ~__LINE__:_
          ~level:_
          ~params:_
          ~data:_
          ~parent
          name
        ->
        state.next <- state.next + 1;
        state.entered <- name :: state.entered;
        (match parent with
        | Trace_core.P_some _ -> state.child_parents <- state.child_parents + 1
        | P_none | P_unknown -> ());
        Test_span state.next)
      ~exit_span:(fun state _ -> state.exited <- state.exited + 1)
      ~add_data_to_span:(fun state _ data -> state.data <- data :: state.data)
      ~message:(fun _ ~level:_ ~params:_ ~data:_ ~span:_ _ -> ())
      ~metric:(fun _ ~level:_ ~params:_ ~data:_ _ _ -> ())
      ()
  in
  Trace_core.Collector.C_some (state, callbacks)

let projection () =
  let state =
    { next = 0; entered = []; exited = 0; child_parents = 0; data = [] }
  in
  Awskit_observability_trace_lwt.install_context ();
  Trace_core.with_setup_collector (collector state) @@ fun () ->
  Fixture.set_response ~status:200
    ~headers:[ ("x-amz-request-id", "R1") ]
    "payload";
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Awskit_observability_trace_lwt.sink ]
      ()
  in
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK result" true (Result.is_ok result);
  let entered = Set.of_list (module String) state.entered in
  List.iter
    [
      "awskit.s3.operation";
      "awskit.s3.attempt";
      "awskit.credentials.resolve";
      "awskit.s3.signing";
      "awskit.http.attempt";
      "awskit.http.response_headers.wait";
      "awskit.http.response_body.consumption";
    ] ~f:(fun name ->
      Alcotest.(check bool) (name ^ " span") true (Set.mem entered name));
  Alcotest.(check int)
    "every span exited once"
    (List.length state.entered)
    state.exited;
  Alcotest.(check bool)
    "nested context reached children" true (state.child_parents > 0);
  Alcotest.(check bool)
    "safe provider request ID projected" true
    (List.exists state.data ~f:(fun fields ->
         match List.Assoc.find fields "aws.request_id" ~equal:String.equal with
         | Some (`String "R1") -> true
         | Some _ | None -> false))

let failure_count snapshot ~projection_name ~phase =
  O.Health.failures snapshot
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let collector_failure_is_contained () =
  let callbacks =
    Trace_core.Collector.Callbacks.make
      ~enter_span:(fun
          _
          ~__FUNCTION__:_
          ~__FILE__:_
          ~__LINE__:_
          ~level:_
          ~params:_
          ~data:_
          ~parent:_
          _
        -> failwith "trace collector failed")
      ~exit_span:(fun _ _ -> ())
      ~add_data_to_span:(fun _ _ _ -> ())
      ~message:(fun _ ~level:_ ~params:_ ~data:_ ~span:_ _ -> ())
      ~metric:(fun _ ~level:_ ~params:_ ~data:_ _ _ -> ())
      ()
  in
  Trace_core.with_setup_collector (Trace_core.Collector.C_some ((), callbacks))
  @@ fun () ->
  Fixture.set_response ~status:200 "payload";
  let observer =
    Awskit_lwt.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Awskit_observability_trace_lwt.sink ]
      ()
  in
  let result =
    Fixture.connection ~observability:observer ()
    |> Fixture.get_value
    |> Lwt_main.run
  in
  Alcotest.(check bool) "SDK result preserved" true (Result.is_ok result);
  let health = Awskit_lwt.Observability.health observer in
  Alcotest.(check bool)
    "start failure attributed to trace projection" true
    Int64.(failure_count health ~projection_name:"trace" ~phase:Start > 0L)

let () =
  Alcotest.run "awskit-observability-trace-lwt"
    [
      ( "behavior:trace-projection",
        [
          Alcotest.test_case "safe nested projection" `Quick projection;
          Alcotest.test_case "collector failure containment" `Quick
            collector_failure_is_contained;
        ] );
    ]
