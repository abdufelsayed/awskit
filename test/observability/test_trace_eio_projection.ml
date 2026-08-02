open Base
module O = Awskit.Observability
module Fixture = Observability_eio_fixture

type Trace_core.span += Test_span of int

type state = {
  mutable next : int;
  mutable entered : string list;
  mutable exited : int;
  mutable child_parents : int;
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
      ~add_data_to_span:(fun _ _ _ -> ())
      ~message:(fun _ ~level:_ ~params:_ ~data:_ ~span:_ _ -> ())
      ~metric:(fun _ ~level:_ ~params:_ ~data:_ _ _ -> ())
      ()
  in
  Trace_core.Collector.C_some (state, callbacks)

let projection env () =
  let state = { next = 0; entered = []; exited = 0; child_parents = 0 } in
  Awskit_observability_trace_eio.install_context ();
  Trace_core.with_setup_collector (collector state) @@ fun () ->
  let observer =
    Awskit_eio.Observability.create
      ~clock:(fun () -> 0L)
      ~trace_sinks:[ Awskit_observability_trace_eio.sink ]
      ()
  in
  Fixture.with_connection env ~observability:observer
  @@ fun connection ~calls:_ ->
  Alcotest.(check bool)
    "SDK result" true
    (Result.is_ok (Fixture.get_value connection));
  let entered = Set.of_list (module String) state.entered in
  List.iter
    [
      "awskit.s3.operation";
      "awskit.s3.attempt";
      "awskit.credentials.resolve";
      "awskit.s3.signing";
      "awskit.http.attempt";
    ] ~f:(fun name ->
      Alcotest.(check bool) (name ^ " span") true (Set.mem entered name));
  Alcotest.(check int)
    "every span exited once"
    (List.length state.entered)
    state.exited;
  Alcotest.(check bool)
    "nested context reached children" true (state.child_parents > 0)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-observability-trace-eio"
    [
      ( "behavior:trace-eio-projection",
        [ Alcotest.test_case "safe nested projection" `Quick (projection env) ]
      );
    ]
