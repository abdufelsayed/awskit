open Base
module F = Awskit.Observability.For_service
module Types = Observation_types

type start = { operation : Types.Operation.t }
type finish = { retry_class : Awskit.Error.retry_class option }

type labels = {
  operation : Types.Operation.t;
  outcome : Awskit.Observability.Outcome.t;
}

let labels =
  F.Metric.Labels.empty ()
  |> F.Metric.Labels.add Types.operation_dimension
       ~get:(fun (labels : labels) -> labels.operation)
  |> F.Metric.Labels.add Types.outcome_dimension ~get:(fun (labels : labels) ->
      labels.outcome)

let duration =
  F.Metric.Family.histogram ~name:"awskit.s3.signing.duration"
    ~doc:"S3 request-signing duration" ~unit_:"ns" ~labels
    ~value:F.Metric.Number.Int64 ()

let start operation = { operation }
let start_fields (_ : start) = F.Fields.empty

let finish_fields (finish : finish) =
  F.Fields.create
    ~dimensions:
      (Option.to_list
         (Option.map finish.retry_class ~f:(fun retry_class ->
              F.Dimension.Enum.value Types.retry_class_dimension retry_class)))
    ()

let classify terminal =
  match F.Terminal.result terminal with
  | Ok (Ok _) -> (Awskit.Observability.Outcome.Ok, { retry_class = None })
  | Ok (Error error) ->
      ( Awskit.Observability.Outcome.of_error error,
        { retry_class = Some (Awskit.Error.retry_class error) } )
  | Error _ -> (F.Terminal.default_outcome terminal, { retry_class = None })

let message (completion : (start, finish) F.Operation.Completion.t) =
  let operation =
    completion |> F.Operation.Completion.start |> fun start ->
    Types.Operation.to_string start.operation
  in
  lazy
    (Fmt.str "S3 %s signing finished with outcome %s" operation
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
  F.Operation.define ~name:"awskit.s3.signing"
    ~doc:"Sign one S3 physical request" ~source:Types.Sources.signing
    ~span_kind:Awskit.Observability.Span_kind.Internal ~start:start_fields
    ~classify ~finish:finish_fields ~log ~metrics ()

module Make (Runtime : F.Observer) = struct
  let with_signing connection ~operation:operation_name callback =
    Runtime.with_operation connection ~operation
      ~start:(fun () -> start operation_name)
      callback
end
