open Base
module F = Awskit.Observability.For_service

type start = { operation : Artifact_operation.t }
type finish = unit

type completion_labels = {
  operation : Artifact_operation.t;
  outcome : Awskit.Observability.Outcome.t;
}

type operation_labels = { operation : Artifact_operation.t }

let operation_dimension =
  F.Dimension.Enum.define ~name:"aws.artifact_operation"
    ~equal:Artifact_operation.equal
    ~values:
      (List.map Artifact_operation.all ~f:(fun operation ->
           (operation, Artifact_operation.to_string operation)))

let outcome_dimension = Observation_dimensions.outcome_dimension

let completion_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add operation_dimension
       ~get:(fun (labels : completion_labels) -> labels.operation)
  |> F.Metric.Labels.add outcome_dimension
       ~get:(fun (labels : completion_labels) -> labels.outcome)

let operation_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add operation_dimension
       ~get:(fun (labels : operation_labels) -> labels.operation)

let artifacts =
  F.Metric.Family.counter ~name:"awskit.s3.artifacts"
    ~doc:"Completed S3 presigned-artifact generations" ~labels:completion_labels
    ~value:F.Metric.Number.Int64 ()

let duration =
  F.Metric.Family.histogram ~name:"awskit.s3.artifact.duration"
    ~doc:"S3 presigned-artifact generation duration" ~unit_:"ns"
    ~labels:completion_labels ~value:F.Metric.Number.Int64 ()

let artifacts_in_flight_family =
  F.Metric.Family.gauge ~name:"awskit.s3.artifacts_in_flight"
    ~doc:"S3 presigned-artifact generations currently in flight"
    ~labels:operation_labels ~value:F.Metric.Number.Int64 ()

let artifacts_in_flight = F.Instrument.define ~family:artifacts_in_flight_family
let state_labels operation : operation_labels = { operation }
let start (operation : Artifact_operation.t) : start = { operation }

let start_fields (start : start) =
  F.Fields.create
    ~dimensions:[ F.Dimension.Enum.value operation_dimension start.operation ]
    ()

let finish_fields (_ : finish) = F.Fields.empty

let classify terminal =
  match F.Terminal.result terminal with
  | Ok (Ok _) -> (Awskit.Observability.Outcome.Ok, ())
  | Ok (Error error) -> (Awskit.Observability.Outcome.of_error error, ())
  | Error _ -> (F.Terminal.default_outcome terminal, ())

let message (completion : (start, finish) F.Operation.Completion.t) =
  let operation =
    completion |> F.Operation.Completion.start |> fun start ->
    Artifact_operation.to_string start.operation
  in
  lazy
    (Fmt.str "S3 presigned artifact %s finished with outcome %s" operation
       (completion
       |> F.Operation.Completion.outcome
       |> Awskit.Observability.Outcome.to_string))

let log =
  F.Log.operation ~levels:[ Logs.Debug; Logs.Error ]
    ~decide:(fun (completion : (start, finish) F.Operation.Completion.t) ->
      match F.Operation.Completion.outcome completion with
      | Awskit.Observability.Outcome.Ok -> F.Log.Skip
      | Cancelled -> Emit { level = Logs.Debug; message = message completion }
      | Not_found | Conflict | Throttled | Error | Exception | Timeout ->
          Emit { level = Logs.Error; message = message completion })

let metrics =
  [
    F.Metric.Projection.sample artifacts
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Some
          ( {
              operation = start.operation;
              outcome = F.Operation.Completion.outcome completion;
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
              },
              duration )));
  ]

let operation () =
  F.Operation.define ~name:"awskit.s3.artifact"
    ~doc:"Generate one S3 presigned artifact"
    ~source:Observation_sources.artifact
    ~span_kind:Awskit.Observability.Span_kind.Internal ~start:start_fields
    ~classify ~finish:finish_fields ~log ~metrics ()

let complete ~operation:operation_name ~outcome () =
  F.Operation.project (operation ()) ~start:(start operation_name)
    ~result:(Error (Failure "simulator completion")) ~default_outcome:outcome
    ~duration_ns:None

module Make (Runtime : F.Observer) = struct
  let with_artifact connection ~operation:operation_name callback =
    Runtime.with_instrument connection artifacts_in_flight
      ~labels:(fun () -> state_labels operation_name)
      1L
      (fun () ->
        Runtime.with_operation connection
          ~operation:(fun () -> operation ())
          ~start:(fun () -> start operation_name)
          callback)
end
