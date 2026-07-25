open Base
module F = Awskit.Observability.For_service
module Types = Observation_types

type start = {
  operation : Types.Operation.t;
  number : int;
  replayability : Types.replayability;
}

type finish = { retry_class : Awskit.Error.retry_class option }

let start ~operation ~number ~replayable =
  if number <= 0 then invalid_arg "S3 attempt number must be positive";
  {
    operation;
    number;
    replayability = (if replayable then Replayable else Non_replayable);
  }

type attempt_labels = {
  operation : Types.Operation.t;
  outcome : Awskit.Observability.Outcome.t;
  replayability : Types.replayability;
}

type failure_labels = {
  operation : Types.Operation.t;
  retry_class : Awskit.Error.retry_class;
  replayability : Types.replayability;
}

type state_labels = { operation : Types.Operation.t }

let attempt_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : attempt_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.outcome_dimension
       ~get:(fun (labels : attempt_labels) -> labels.outcome)
  |> F.Metric.Labels.add Types.replayability_dimension
       ~get:(fun (labels : attempt_labels) -> labels.replayability)

let failure_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : failure_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.retry_class_dimension
       ~get:(fun (labels : failure_labels) -> labels.retry_class)
  |> F.Metric.Labels.add Types.replayability_dimension
       ~get:(fun (labels : failure_labels) -> labels.replayability)

let state_labels_descriptor =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : state_labels) -> labels.operation)

let attempts =
  F.Metric.Family.counter ~name:"awskit.s3.attempts"
    ~doc:"Completed S3 retry iterations" ~labels:attempt_labels
    ~value:F.Metric.Number.Int64 ()

let duration =
  F.Metric.Family.histogram ~name:"awskit.s3.attempt.duration"
    ~doc:"S3 retry-iteration duration excluding backoff" ~unit_:"ns"
    ~labels:attempt_labels ~value:F.Metric.Number.Int64 ()

let failures =
  F.Metric.Family.counter ~name:"awskit.s3.attempt_failures"
    ~doc:"Failed S3 attempts with a known retry class" ~labels:failure_labels
    ~value:F.Metric.Number.Int64 ()

let attempts_in_flight_family =
  F.Metric.Family.gauge ~name:"awskit.s3.attempts_in_flight"
    ~doc:"S3 retry iterations currently in flight"
    ~labels:state_labels_descriptor ~value:F.Metric.Number.Int64 ()

let attempts_in_flight = F.Instrument.define ~family:attempts_in_flight_family
let state_labels operation : state_labels = { operation }

let start_fields (start : start) =
  F.Fields.create
    ~dimensions:
      [
        F.Dimension.Enum.value Types.replayability_dimension start.replayability;
      ]
    ~diagnostics:(Option.to_list (F.Diagnostic.attempt_number start.number))
    ()

let finish_fields (finish : finish) =
  F.Fields.create
    ~dimensions:
      (Option.to_list
         (Option.map finish.retry_class ~f:(fun retry_class ->
              F.Dimension.Enum.value Types.retry_class_dimension retry_class)))
    ()

let error_finish error = { retry_class = Some (Awskit.Error.retry_class error) }

let classify terminal =
  match F.Terminal.result terminal with
  | Ok (Execution_attempt.Complete (Ok _)) ->
      (Awskit.Observability.Outcome.Ok, { retry_class = None })
  | Ok (Execution_attempt.Complete (Error error))
  | Ok (Execution_attempt.Retry_after { error; _ }) ->
      (Awskit.Observability.Outcome.of_error error, error_finish error)
  | Error _ -> (F.Terminal.default_outcome terminal, { retry_class = None })

let metrics =
  [
    F.Metric.Projection.sample attempts
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Some
          ( {
              operation = start.operation;
              outcome = F.Operation.Completion.outcome completion;
              replayability = start.replayability;
            },
            1L ));
    F.Metric.Projection.sample ~needs_duration:true duration
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Option.map (F.Operation.Completion.duration_ns completion)
          ~f:(fun duration ->
            ( {
                operation = start.operation;
                outcome = F.Operation.Completion.outcome completion;
                replayability = start.replayability;
              },
              duration )));
    F.Metric.Projection.sample failures
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Option.bind (F.Operation.Completion.finish completion) ~f:(fun finish ->
            Option.map finish.retry_class ~f:(fun retry_class ->
                ( {
                    operation = start.operation;
                    retry_class;
                    replayability = start.replayability;
                  },
                  1L ))));
  ]

let operation () =
  F.Operation.define ~name:"awskit.s3.attempt"
    ~doc:"One S3 retry iteration including credentials, signing, and HTTP"
    ~source:Types.Sources.attempt
    ~span_kind:Awskit.Observability.Span_kind.Internal ~start:start_fields
    ~classify ~finish:finish_fields ~log:F.Log.silent_operation ~metrics ()

type retry = {
  operation : Types.Operation.t;
  attempt : int;
  decision : Types.retry_decision;
  retry_class : Awskit.Error.retry_class;
  replayability : Types.replayability;
  delay_seconds : float option;
  remaining_budget : int;
}

let retry ~operation ~attempt ~decision ~retry_class ~replayable ?delay
    ~remaining_budget () =
  if attempt <= 0 then invalid_arg "S3 retry attempt number must be positive";
  let decision =
    match decision with
    | Execution_retry.Scheduled -> Types.Scheduled
    | Non_replayable_request -> Types.Non_replayable_request
    | Attempts_exhausted -> Types.Attempts_exhausted
    | Budget_exhausted -> Types.Budget_exhausted
    | Policy_denied -> Types.Policy_denied
  in
  {
    operation;
    attempt;
    decision;
    retry_class;
    replayability = (if replayable then Replayable else Non_replayable);
    delay_seconds = Option.map delay ~f:Ptime.Span.to_float_s;
    remaining_budget;
  }

type retry_labels = {
  operation : Types.Operation.t;
  decision : Types.retry_decision;
  retry_class : Awskit.Error.retry_class;
  replayability : Types.replayability;
}

type delay_labels = {
  operation : Types.Operation.t;
  retry_class : Awskit.Error.retry_class;
}

type budget_labels = {
  operation : Types.Operation.t;
  decision : Types.retry_decision;
}

let retry_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : retry_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.retry_decision_dimension
       ~get:(fun (labels : retry_labels) -> labels.decision)
  |> F.Metric.Labels.add Types.retry_class_dimension
       ~get:(fun (labels : retry_labels) -> labels.retry_class)
  |> F.Metric.Labels.add Types.replayability_dimension
       ~get:(fun (labels : retry_labels) -> labels.replayability)

let delay_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : delay_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.retry_class_dimension
       ~get:(fun (labels : delay_labels) -> labels.retry_class)

let budget_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : budget_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.retry_decision_dimension
       ~get:(fun (labels : budget_labels) -> labels.decision)

let retry_decisions =
  F.Metric.Family.counter ~name:"awskit.s3.retry.decisions"
    ~doc:"S3 retry scheduling and denial decisions" ~labels:retry_labels
    ~value:F.Metric.Number.Int64 ()

let retry_delay =
  F.Metric.Family.histogram ~name:"awskit.s3.retry.delay"
    ~doc:"Scheduled S3 retry backoff" ~unit_:"s" ~labels:delay_labels
    ~value:F.Metric.Number.Float ()

let remaining_budget =
  F.Metric.Family.histogram ~name:"awskit.s3.retry.remaining_budget"
    ~doc:"Remaining retry budget after an S3 retry decision"
    ~labels:budget_labels ~value:F.Metric.Number.Int64 ()

let retry_fields (retry : retry) =
  F.Fields.create
    ~dimensions:
      [
        F.Dimension.Enum.value Types.retry_decision_dimension retry.decision;
        F.Dimension.Enum.value Types.retry_class_dimension retry.retry_class;
      ]
    ~measurements:
      (F.Measurement.int ~name:"retry.remaining_budget" retry.remaining_budget
      :: Option.to_list
           (Option.map retry.delay_seconds ~f:(fun seconds ->
                F.Measurement.float ~unit_:"s" ~name:"retry.delay" seconds)))
    ~diagnostics:(Option.to_list (F.Diagnostic.attempt_number retry.attempt))
    ()

let retry_metrics =
  [
    F.Metric.Projection.sample retry_decisions ~get:(fun (retry : retry) ->
        Some
          ( {
              operation = retry.operation;
              decision = retry.decision;
              retry_class = retry.retry_class;
              replayability = retry.replayability;
            },
            1L ));
    F.Metric.Projection.sample retry_delay ~get:(fun (retry : retry) ->
        Option.map retry.delay_seconds ~f:(fun delay ->
            ( { operation = retry.operation; retry_class = retry.retry_class },
              delay )));
    F.Metric.Projection.sample remaining_budget ~get:(fun (retry : retry) ->
        Some
          ( { operation = retry.operation; decision = retry.decision },
            Int64.of_int retry.remaining_budget ));
  ]

let event_message label (retry : retry) =
  lazy
    (Fmt.str "S3 %s retry %s after attempt %d"
       (Types.Operation.to_string retry.operation)
       label retry.attempt)

let scheduled_log =
  F.Log.event ~levels:[ Logs.Debug ] ~decide:(fun (retry : retry) ->
      Emit { level = Logs.Debug; message = event_message "scheduled" retry })

let denied_log =
  F.Log.event ~levels:[ Logs.Debug; Logs.Warning ]
    ~decide:(fun (retry : retry) ->
      let level =
        match retry.retry_class with
        | Awskit.Error.Retryable | Awskit.Error.Throttled -> Logs.Warning
        | Awskit.Error.Auth | Awskit.Error.Conflict | Awskit.Error.Not_found
        | Awskit.Error.Fatal | Awskit.Error.Unknown ->
            Logs.Debug
      in
      let reason =
        match retry.decision with
        | Types.Scheduled -> "scheduled"
        | Types.Non_replayable_request -> "denied: request is non-replayable"
        | Types.Attempts_exhausted -> "denied: attempts exhausted"
        | Types.Budget_exhausted -> "denied: retry budget exhausted"
        | Types.Policy_denied -> "denied by policy"
      in
      Emit { level; message = event_message reason retry })

let scheduled_event =
  F.Event.define ~name:"awskit.s3.retry.scheduled"
    ~doc:"An S3 retry and backoff were scheduled" ~source:Types.Sources.retry
    ~fields:retry_fields ~log:scheduled_log ~metrics:retry_metrics ()

let denied_event =
  F.Event.define ~name:"awskit.s3.retry.denied" ~doc:"An S3 retry was denied"
    ~source:Types.Sources.retry ~fields:retry_fields ~log:denied_log
    ~metrics:retry_metrics ()

let event (retry : retry) =
  match retry.decision with
  | Scheduled -> scheduled_event
  | Non_replayable_request | Attempts_exhausted | Budget_exhausted
  | Policy_denied ->
      denied_event

module Make (Runtime : F.Observer) = struct
  let with_attempt connection ~operation:operation_name ~number ~replayable
      callback =
    Runtime.with_instrument connection attempts_in_flight
      ~labels:(fun () -> state_labels operation_name)
      1L
      (fun () ->
        Runtime.with_operation connection ~operation
          ~start:(fun () -> start ~operation:operation_name ~number ~replayable)
          callback)

  let emit_retry connection retry =
    Runtime.emit_event connection (event retry) ~data:(fun () -> retry)
end
