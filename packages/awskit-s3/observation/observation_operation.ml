open Base
module F = Awskit.Observability.For_service
module Types = Observation_types

module Shape = Observation_logical_operation.Make (struct
  type t = Types.Operation.t

  let equal = Types.Operation.equal
  let values = Types.operation_values
  let to_string = Types.Operation.to_string
  let source = Types.Sources.operation
end)

type start = Shape.start = {
  operation : Types.Operation.t;
  region : string option;
  bucket : string option;
}

type finish = Shape.finish = {
  attempts : int option;
  logical_request_bytes : int64 option;
  logical_response_bytes : int64 option;
  retry_class : Awskit.Error.retry_class option;
}

let start = Shape.start

type completion_labels = {
  operation : Types.Operation.t;
  outcome : Awskit.Observability.Outcome.t;
}

type operation_labels = { operation : Types.Operation.t }

let completion_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : completion_labels) -> labels.operation)
  |> F.Metric.Labels.add Types.outcome_dimension
       ~get:(fun (labels : completion_labels) -> labels.outcome)

let operation_labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : operation_labels) -> labels.operation)

let operations =
  F.Metric.Family.counter ~name:"awskit.s3.operations"
    ~doc:"Completed logical S3 operations" ~labels:completion_labels
    ~value:F.Metric.Number.Int64 ()

let duration =
  F.Metric.Family.histogram ~name:"awskit.s3.operation.duration"
    ~doc:"Logical S3 operation duration" ~unit_:"ns" ~labels:completion_labels
    ~value:F.Metric.Number.Int64 ()

let logical_request_bytes =
  F.Metric.Family.histogram ~name:"awskit.s3.logical_request_bytes"
    ~doc:"Caller-visible logical request bytes" ~unit_:"By"
    ~labels:operation_labels ~value:F.Metric.Number.Int64 ()

let logical_response_bytes =
  F.Metric.Family.histogram ~name:"awskit.s3.logical_response_bytes"
    ~doc:"Successful caller-visible logical response bytes" ~unit_:"By"
    ~labels:operation_labels ~value:F.Metric.Number.Int64 ()

let operations_in_flight_family =
  F.Metric.Family.gauge ~name:"awskit.s3.operations_in_flight"
    ~doc:"Logical S3 operations currently in flight" ~labels:operation_labels
    ~value:F.Metric.Number.Int64 ()

let operations_in_flight =
  F.Instrument.define ~family:operations_in_flight_family

let state_labels operation : operation_labels = { operation }

let classify ~attempts ~logical_request_bytes ~logical_response_bytes terminal =
  let common retry_class =
    {
      attempts = Some (attempts ());
      logical_request_bytes = logical_request_bytes ();
      logical_response_bytes = logical_response_bytes ();
      retry_class;
    }
  in
  match F.Terminal.result terminal with
  | Ok (Ok _) -> (Awskit.Observability.Outcome.Ok, common None)
  | Ok (Error error) ->
      ( Awskit.Observability.Outcome.of_error error,
        common (Some (Awskit.Error.retry_class error)) )
  | Error _ -> (F.Terminal.default_outcome terminal, common None)

let metrics =
  [
    F.Metric.Projection.sample operations
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
    F.Metric.Projection.sample logical_request_bytes
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Option.bind (F.Operation.Completion.finish completion) ~f:(fun finish ->
            Option.map finish.logical_request_bytes ~f:(fun bytes ->
                (({ operation = start.operation } : operation_labels), bytes))));
    F.Metric.Projection.sample logical_response_bytes
      ~get:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        let start = F.Operation.Completion.start completion in
        Option.bind (F.Operation.Completion.finish completion) ~f:(fun finish ->
            Option.map finish.logical_response_bytes ~f:(fun bytes ->
                (({ operation = start.operation } : operation_labels), bytes))));
  ]

let operation ~attempts ~logical_request_bytes ~logical_response_bytes =
  Shape.define
    ~classify:
      (classify ~attempts ~logical_request_bytes ~logical_response_bytes)
    ~metrics

module Make (Runtime : F.Observer) = struct
  let with_operation connection ~operation:operation_name ~region ~bucket
      ~attempts ~logical_request_bytes ~logical_response_bytes callback =
    Runtime.with_instrument connection operations_in_flight
      ~labels:(fun () -> state_labels operation_name)
      1L
      (fun () ->
        Runtime.with_operation connection
          ~operation:(fun () ->
            operation ~attempts ~logical_request_bytes ~logical_response_bytes)
          ~start:(fun () -> start ?region ?bucket operation_name)
          callback)
end
