open Base
module O = Awskit.Observability
module F = O.For_service
module P = O.For_projection

module Context = struct
  type +'a io = 'a
  type 'a key = { mutable value : 'a option }

  let finalize_calls = ref 0
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
    Int.incr finalize_calls;
    try
      let value = callback () in
      (try hook (Ok value) with _ -> ());
      value
    with exn ->
      (try hook (Error exn) with _ -> ());
      raise exn

  let raised_outcome _ = O.Outcome.Exception
end

module Trace = struct
  type +'a io = 'a Context.io

  type activation = {
    finish : P.Operation.Completion.t -> unit;
    within : 'a. (unit -> 'a io) -> 'a io;
  }

  type t = {
    name : string;
    enabled : bool;
    needs_clock : bool;
    starts : P.Operation.Start.t list ref;
    completions : P.Operation.Completion.t list ref;
    events : P.Event.t list ref;
  }

  let name t = t.name
  let needs_clock t = t.needs_clock
  let enabled t _ = t.enabled

  let start t started =
    t.starts := started :: !(t.starts);
    {
      finish =
        (fun completion -> t.completions := completion :: !(t.completions));
      within = (fun callback -> callback ());
    }

  let correlation _ = []
  let within activation callback = activation.within callback
  let finish activation completion = activation.finish completion
  let event_enabled t _ = t.enabled
  let event t event = t.events := event :: !(t.events)

  let create ?(enabled = true) ?(needs_clock = false) name =
    {
      name;
      enabled;
      needs_clock;
      starts = ref [];
      completions = ref [];
      events = ref [];
    }
end

module Observer = O.For_runtime.Make (Context) (Trace)

let source = Logs.Src.create "awskit.test.observation-engine"

type operation_result = Success | Throttled | Failure

let classify terminal =
  match F.Terminal.result terminal with
  | Ok Success -> (O.Outcome.Ok, ())
  | Ok Throttled -> (O.Outcome.Throttled, ())
  | Ok Failure -> (O.Outcome.Error, ())
  | Error _ -> (F.Terminal.default_outcome terminal, ())

let operation ?(log = F.Log.silent_operation) ?(metrics = []) name =
  F.Operation.define ~name ~doc:"Semantic observer contract operation" ~source
    ~span_kind:O.Span_kind.Internal
    ~start:(fun () -> F.Fields.empty)
    ~classify
    ~finish:(fun () -> F.Fields.empty)
    ~log ~metrics ()

let run observer definition result =
  Observer.with_operation observer
    ~operation:(fun () -> definition)
    ~start:(fun () -> ())
    (fun () -> result)

let health_count observer ~projection_name ~phase =
  Observer.health observer
  |> O.Health.failures
  |> List.filter ~f:(fun failure ->
      String.equal projection_name
        (failure |> O.Health.projection |> O.Health.Projection.name)
      && Poly.equal phase (O.Health.phase failure))
  |> List.fold ~init:0L ~f:(fun total failure ->
      let count = O.Health.count failure in
      Int64.(total + count))

let test_hard_off_is_lazy () =
  let definition_requested = ref false in
  let started = ref false in
  let classified = ref false in
  let definition =
    F.Operation.define ~name:"awskit.test.disabled" ~doc:"hard-off" ~source
      ~span_kind:O.Span_kind.Internal
      ~start:(fun () ->
        started := true;
        F.Fields.empty)
      ~classify:(fun terminal ->
        classified := true;
        (F.Terminal.default_outcome terminal, ()))
      ~finish:(fun () -> F.Fields.empty)
      ~log:F.Log.silent_operation ~metrics:[] ()
  in
  ignore
    (Observer.with_operation Observer.none
       ~operation:(fun () ->
         definition_requested := true;
         definition)
       ~start:(fun () -> ())
       (fun () -> Success));
  Alcotest.(check bool)
    "definition thunk remains lazy" false !definition_requested;
  Alcotest.(check bool) "start thunk remains lazy" false !started;
  Alcotest.(check bool) "classifier remains lazy" false !classified;
  Alcotest.(check int)
    "hard-off health is empty" 0
    (Observer.health Observer.none |> O.Health.failures |> List.length);
  let family =
    F.Metric.Family.gauge ~name:"awskit.test.disabled.gauge" ~doc:"hard-off"
      ~labels:(F.Metric.Labels.empty ()) ~value:F.Metric.Number.Int64 ()
  in
  let instrument = F.Instrument.define ~family in
  let labels_called = ref false in
  let finalize_before = !Context.finalize_calls in
  ignore
    (Observer.with_instrument Observer.none instrument
       ~labels:(fun () ->
         labels_called := true;
         ())
       1L
       (fun () -> ())
      : unit);
  Alcotest.(check bool) "hard-off labels remain lazy" false !labels_called;
  Alcotest.(check int)
    "hard-off skips finalizer" finalize_before !Context.finalize_calls

let test_operation_completion_and_clock () =
  let trace = Trace.create ~needs_clock:true "trace" in
  let ticks = ref 0L in
  let clock () =
    let value = !ticks in
    ticks := Int64.succ value;
    value
  in
  let observer = Observer.create ~logs:false ~clock ~trace_sinks:[ trace ] () in
  let value = run observer (operation "awskit.test.operation") Success in
  Alcotest.(check bool) "result preserved" true (Poly.equal Success value);
  Alcotest.(check int) "one start" 1 (List.length !(trace.starts));
  Alcotest.(check int) "one completion" 1 (List.length !(trace.completions));
  Alcotest.(check int64) "one monotonic interval" 2L !ticks

let test_context_isolation_and_finalization () =
  let trace = Trace.create "trace" in
  let observer = Observer.create ~logs:false ~trace_sinks:[ trace ] () in
  let definition = operation "awskit.test.nested" in
  let result =
    Observer.with_operation observer
      ~operation:(fun () -> definition)
      ~start:(fun () -> ())
      (fun () ->
        Observer.with_operation observer
          ~operation:(fun () -> definition)
          ~start:(fun () -> ())
          (fun () -> Throttled))
  in
  Alcotest.(check bool)
    "nested result preserved" true
    (Poly.equal Throttled result);
  Alcotest.(check int)
    "nested operations complete exactly once" 2
    (List.length !(trace.completions));
  let raises =
    try
      ignore
        (Observer.with_operation observer
           ~operation:(fun () -> definition)
           ~start:(fun () -> ())
           (fun () -> raise (Failure "callback")));
      false
    with
    | Failure message -> String.equal message "callback"
    | _ -> false
  in
  Alcotest.(check bool) "callback exception preserved" true raises;
  Alcotest.(check int)
    "exception completion exactly once" 3
    (List.length !(trace.completions))

let test_events_and_pull_snapshot () =
  let trace = Trace.create "trace" in
  let metric_sink =
    O.Metric_sink.create ~name:"gauge" ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.test.gauge" (P.Metric.Family.name family))
      ~observe:(fun _ -> ())
  in
  let observer =
    Observer.create ~logs:false ~trace_sinks:[ trace ]
      ~metric_sinks:[ metric_sink ] ()
  in
  let event =
    F.Event.define ~name:"awskit.test.event" ~doc:"event" ~source
      ~fields:(fun () -> F.Fields.empty)
      ~log:F.Log.silent_event ~metrics:[] ()
  in
  Observer.emit_event observer event ~data:(fun () -> ());
  Alcotest.(check int) "event delivered" 1 (List.length !(trace.events));
  let family =
    F.Metric.Family.gauge ~name:"awskit.test.gauge" ~doc:"gauge"
      ~labels:(F.Metric.Labels.empty ()) ~value:F.Metric.Number.Int64 ()
  in
  let instrument = F.Instrument.define ~family in
  let lease = Observer.acquire observer instrument ~labels:(fun () -> ()) 4L in
  Alcotest.(check int)
    "one gauge snapshot" 1
    (Observer.snapshot observer |> List.length);
  Observer.add lease (-1L);
  Observer.release lease;
  let value =
    match Observer.snapshot observer with
    | [ observation ] -> (
        match P.Metric.Observation.value observation with
        | Int64 value -> value
        | Int _ | Float _ -> Alcotest.fail "gauge number changed")
    | _ -> Alcotest.fail "expected one gauge"
  in
  Alcotest.(check int64) "released gauge returns to zero" 0L value

let test_projection_failure_is_local () =
  let metric_sink =
    O.Metric_sink.create ~name:"failing-metric" ~needs_clock:false
      ~enabled:(fun _ -> true)
      ~observe:(fun _ -> failwith "sink")
  in
  let observer = Observer.create ~logs:false ~metric_sinks:[ metric_sink ] () in
  let family =
    F.Metric.Family.counter ~name:"awskit.test.counter" ~doc:"counter"
      ~labels:(F.Metric.Labels.empty ()) ~value:F.Metric.Number.Int64 ()
  in
  let projection =
    F.Metric.Projection.sample family
      ~get:(fun (_ : (unit, unit) F.operation_completion) -> Some ((), 1L))
  in
  let definition = operation ~metrics:[ projection ] "awskit.test.metric" in
  ignore (run observer definition Success);
  Alcotest.(check bool)
    "finish failure is contained" true
    Int64.(
      health_count observer ~projection_name:"failing-metric" ~phase:Finish > 0L)

let test_definition_failure_is_local () =
  let observer = Observer.create ~logs:false () in
  let callback_called = ref false in
  let result =
    Observer.with_operation observer
      ~operation:(fun () -> failwith "definition")
      ~start:(fun () -> Alcotest.fail "start builder must remain lazy")
      (fun () ->
        callback_called := true;
        Success)
  in
  Alcotest.(check bool)
    "callback result preserved" true
    (Poly.equal Success result);
  Alcotest.(check bool) "callback invoked" true !callback_called;
  Alcotest.(check bool)
    "definition failure recorded at enablement" true
    Int64.(
      health_count observer ~projection_name:"engine" ~phase:Enablement > 0L)

let () =
  Alcotest.run "awskit-observation-engine"
    [
      ( "behavior:semantic-observer",
        [
          Alcotest.test_case "hard-off is lazy" `Quick test_hard_off_is_lazy;
          Alcotest.test_case "operation completion and clock" `Quick
            test_operation_completion_and_clock;
          Alcotest.test_case "context and finalization" `Quick
            test_context_isolation_and_finalization;
          Alcotest.test_case "events and snapshot" `Quick
            test_events_and_pull_snapshot;
          Alcotest.test_case "projection failure local" `Quick
            test_projection_failure_is_local;
          Alcotest.test_case "definition failure local" `Quick
            test_definition_failure_is_local;
        ] );
    ]
