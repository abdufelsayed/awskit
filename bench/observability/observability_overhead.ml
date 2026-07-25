open Base
module O = Awskit.Observability
module F = O.For_service

let iterations =
  match Sys.getenv "AWSKIT_OBSERVABILITY_BENCH_ITERATIONS" with
  | Some value -> Int.of_string value
  | None -> 100_000

let source =
  Logs.Src.create "awskit.benchmark.observability"
    ~doc:"Awskit observability overhead benchmark"

let outcome_dimension =
  F.Dimension.Enum.define ~name:"outcome" ~equal:Poly.equal
    ~values:
      [
        (O.Outcome.Ok, "ok");
        (Not_found, "not_found");
        (Conflict, "conflict");
        (Throttled, "throttled");
        (Error, "error");
        (Exception, "exception");
        (Cancelled, "cancelled");
        (Timeout, "timeout");
      ]

type outcome_labels = { outcome : O.Outcome.t }

let outcome_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add outcome_dimension
       ~get:(fun (labels : outcome_labels) -> labels.outcome)

let operations =
  F.Metric.Family.counter ~name:"awskit.benchmark.operations"
    ~doc:"Completed benchmark operations" ~labels:outcome_labels
    ~value:F.Metric.Number.Int64 ()

let durations =
  F.Metric.Family.histogram ~name:"awskit.benchmark.operation.duration"
    ~doc:"Benchmark operation duration" ~unit_:"ns" ~labels:outcome_labels
    ~value:F.Metric.Number.Int64 ()

let completion_metrics =
  [
    F.Metric.Projection.sample operations
      ~get:(fun (completion : (unit, unit) F.Operation.Completion.t) ->
        Some ({ outcome = F.Operation.Completion.outcome completion }, 1L));
    F.Metric.Projection.sample ~needs_duration:true durations
      ~get:(fun (completion : (unit, unit) F.Operation.Completion.t) ->
        Option.map (F.Operation.Completion.duration_ns completion)
          ~f:(fun duration ->
            ({ outcome = F.Operation.Completion.outcome completion }, duration)));
  ]

let classify terminal =
  match F.Terminal.result terminal with
  | Ok (Ok ()) -> (O.Outcome.Ok, ())
  | Ok (Error error) -> (O.Outcome.of_error error, ())
  | Error _ -> (F.Terminal.default_outcome terminal, ())

let completion_log =
  F.Log.operation ~levels:[ Logs.Debug; Logs.Error ]
    ~decide:(fun (completion : (unit, unit) F.Operation.Completion.t) ->
      let outcome = F.Operation.Completion.outcome completion in
      let level =
        if Poly.equal outcome O.Outcome.Ok then Logs.Debug else Logs.Error
      in
      F.Log.Emit
        {
          level;
          message =
            lazy
              (Fmt.str "benchmark operation finished with outcome %s"
                 (O.Outcome.to_string outcome));
        })

let define_operation ~name ~log ~metrics =
  F.Operation.define ~name ~doc:"Benchmark observation operation" ~source
    ~span_kind:O.Span_kind.Internal
    ~start:(fun () -> F.Fields.empty)
    ~classify
    ~finish:(fun () -> F.Fields.empty)
    ~log ~metrics ()

let logical_operation =
  define_operation ~name:"awskit.benchmark.operation" ~log:completion_log
    ~metrics:completion_metrics

let attempt_operation =
  define_operation ~name:"awskit.benchmark.attempt" ~log:F.Log.silent_operation
    ~metrics:[]

let credential_operation =
  define_operation ~name:"awskit.benchmark.credentials"
    ~log:F.Log.silent_operation ~metrics:[]

let signing_operation =
  define_operation ~name:"awskit.benchmark.signing" ~log:F.Log.silent_operation
    ~metrics:[]

let http_operation =
  define_operation ~name:"awskit.benchmark.http" ~log:F.Log.silent_operation
    ~metrics:[]

let retry_events =
  let labels = F.Metric.Labels.empty () in
  F.Metric.Family.counter ~name:"awskit.benchmark.retry.events"
    ~doc:"Benchmark retry events" ~labels ~value:F.Metric.Number.Int64 ()

let retry_event =
  F.Event.define ~name:"awskit.benchmark.retry" ~doc:"Benchmark retry decision"
    ~source
    ~fields:(fun attempt ->
      F.Fields.create
        ~measurements:[ F.Measurement.int ~name:"attempt" attempt ]
        ())
    ~log:F.Log.silent_event
    ~metrics:
      [
        F.Metric.Projection.sample retry_events ~get:(fun _attempt ->
            Some ((), 1L));
      ]
    ()

let active_operations =
  let labels = F.Metric.Labels.empty () in
  let family =
    F.Metric.Family.gauge ~name:"awskit.benchmark.operations_in_flight"
      ~doc:"Benchmark operations in flight" ~labels ~value:F.Metric.Number.Int64
      ()
  in
  F.Instrument.define ~family

type direction = Request | Response

let direction_dimension =
  F.Dimension.Enum.define ~name:"direction" ~equal:Poly.equal
    ~values:[ (Request, "request"); (Response, "response") ]

type direction_labels = { direction : direction }

let streaming_bytes =
  let labels =
    F.Metric.Labels.empty ()
    |> F.Metric.Labels.add direction_dimension
         ~get:(fun (labels : direction_labels) -> labels.direction)
  in
  let family =
    F.Metric.Family.gauge ~name:"awskit.benchmark.streaming_bytes_in_flight"
      ~doc:"Benchmark streaming bytes in flight" ~unit_:"By" ~labels
      ~value:F.Metric.Number.Int64 ()
  in
  F.Instrument.define ~family

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

module Trace = struct
  type +'a io = 'a Context.io
  type t = unit
  type activation = unit

  let name () = "benchmark"
  let needs_clock () = true
  let enabled () _ = true
  let start () _ = ()
  let correlation () = []
  let within () callback = callback ()
  let finish () _ = ()
  let event_enabled () _ = true
  let event () _ = ()
end

module Runtime = O.For_runtime.Make (Context) (Trace)

let with_operation observer definition callback =
  Runtime.with_operation observer
    ~operation:(fun () -> definition)
    ~start:(fun () -> ())
    callback

let operation_path observer result () =
  ignore
    (with_operation observer logical_operation (fun () -> result)
      : (unit, Awskit.Error.t) Result.t)

let retry_path observer () =
  ignore
    (with_operation observer logical_operation (fun () ->
         List.iter [ 1; 2; 3 ] ~f:(fun attempt ->
             ignore
               (with_operation observer attempt_operation (fun () ->
                    let child definition =
                      ignore
                        (with_operation observer definition (fun () -> Ok ())
                          : (unit, Awskit.Error.t) Result.t)
                    in
                    child credential_operation;
                    child signing_operation;
                    child http_operation;
                    if attempt < 3 then
                      Runtime.emit_event observer retry_event ~data:(fun () ->
                          attempt);
                    Ok ())
                 : (unit, Awskit.Error.t) Result.t));
         Ok ())
      : (unit, Awskit.Error.t) Result.t)

let instrument_path observer () =
  let lease =
    Runtime.acquire observer active_operations ~labels:(fun () -> ()) 1L
  in
  Runtime.release lease

let streaming_path observer () =
  let lease =
    Runtime.acquire observer streaming_bytes
      ~labels:(fun () -> { direction = Request })
      0L
  in
  Runtime.add lease 65_536L;
  Runtime.add lease (-65_536L);
  Runtime.release lease

let metric_sink =
  O.Metric_sink.create ~name:"benchmark" ~needs_clock:true
    ~enabled:(fun _ -> true)
    ~observe:(fun _ -> ())

let clock () = Mtime_clock.now () |> Mtime.to_uint64_ns

let run name callback =
  Stdlib.Gc.full_major ();
  let before = Stdlib.Gc.quick_stat () in
  let counter = Mtime_clock.counter () in
  for _ = 1 to iterations do
    callback ()
  done;
  let elapsed = Mtime_clock.count counter |> Mtime.Span.to_uint64_ns in
  let after = Stdlib.Gc.quick_stat () in
  let per_operation = Int64.to_float elapsed /. Float.of_int iterations in
  let allocated_words =
    after.minor_words
    -. before.minor_words
    +. (after.major_words -. before.major_words)
    -. (after.promoted_words -. before.promoted_words)
  in
  Fmt.pr "%s %.1f ns/op %.2f words/op (%d iterations)@." name per_operation
    (allocated_words /. Float.of_int iterations)
    iterations

let with_logs callback =
  let previous_level = Logs.Src.level source in
  let previous_reporter = Logs.reporter () in
  Exn.protect
    ~f:(fun () ->
      Logs.Src.set_level source (Some Debug);
      Logs.set_reporter Logs.nop_reporter;
      callback ())
    ~finally:(fun () ->
      Logs.set_reporter previous_reporter;
      Logs.Src.set_level source previous_level)

let () =
  let hard_off = Runtime.none in
  let logs = Runtime.default () in
  let metrics =
    Runtime.create ~logs:false ~clock ~metric_sinks:[ metric_sink ] ()
  in
  let trace = Runtime.create ~logs:false ~clock ~trace_sinks:[ () ] () in
  let combined =
    Runtime.create ~clock ~metric_sinks:[ metric_sink ] ~trace_sinks:[ () ] ()
  in
  let failure =
    Error
      (Awskit.Error.Producer.transport ~retryable:false
         "benchmark transport failure")
  in
  run "hard-off" (operation_path hard_off (Ok ()));
  with_logs (fun () ->
      run "logs-success" (operation_path logs (Ok ()));
      run "logs-failure" (operation_path logs failure));
  run "metrics" (operation_path metrics (Ok ()));
  run "trace" (operation_path trace (Ok ()));
  with_logs (fun () -> run "combined" (operation_path combined (Ok ())));
  with_logs (fun () -> run "retry" (retry_path combined));
  run "instrument" (instrument_path metrics);
  run "streaming-bytes" (streaming_path metrics)
