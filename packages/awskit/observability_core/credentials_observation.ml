module Aws_credentials = Credentials
module Aws_error = Error
open Base

let source =
  Logs.Src.create "awskit.credentials" ~doc:"Awskit credential resolution"

type finish = {
  source : Aws_credentials.source option;
  retry_class : Aws_error.retry_class option;
}

type outcome_labels = { outcome : Fields.Outcome.t }
type source_labels = { source : Aws_credentials.source }

let outcome_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add Fields.Dimension.outcome
       ~get:(fun (labels : outcome_labels) -> labels.outcome)

let source_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add Fields.Dimension.credential_source
       ~get:(fun (labels : source_labels) -> labels.source)

let resolutions =
  Metric.Family.counter ~name:"awskit.credentials.resolutions"
    ~doc:"Completed credential resolutions" ~labels:outcome_labels
    ~value:Metric.Number.Int64 ()

let resolution_duration =
  Metric.Family.histogram ~name:"awskit.credentials.resolution.duration"
    ~doc:"Credential resolution duration" ~unit_:"ns" ~labels:outcome_labels
    ~value:Metric.Number.Int64 ()

let resolved =
  Metric.Family.counter ~name:"awskit.credentials.resolved"
    ~doc:"Credentials successfully resolved by source" ~labels:source_labels
    ~value:Metric.Number.Int64 ()

let classify terminal =
  match Definition.Terminal.result terminal with
  | Ok (Ok credentials) ->
      ( Fields.Outcome.Ok,
        { source = Aws_credentials.source credentials; retry_class = None } )
  | Ok (Error error) ->
      ( Fields.Outcome.of_error error,
        { source = None; retry_class = Some (Aws_error.retry_class error) } )
  | Error _ ->
      ( Definition.Terminal.default_outcome terminal,
        { source = None; retry_class = None } )

let finish_fields (finish : finish) =
  let source_dimension =
    Option.to_list
      (Option.map finish.source ~f:(fun source ->
           Fields.Dimension.Enum.value Fields.Dimension.credential_source source))
  in
  let retry_dimension =
    Option.to_list
      (Option.map finish.retry_class ~f:(fun retry_class ->
           Fields.Dimension.Enum.value Fields.Dimension.retry_class retry_class))
  in
  Fields.Fields.create ~dimensions:(source_dimension @ retry_dimension) ()

let message (completion : (unit, finish) Definition.Operation.Completion.t) =
  lazy
    (Fmt.str "credential resolution finished with outcome %s"
       (completion
       |> Definition.Operation.Completion.outcome
       |> Fields.Outcome.to_string))

let log =
  Definition.Log.operation ~levels:[ Logs.Debug; Logs.Error ]
    ~decide:(fun
        (completion : (unit, finish) Definition.Operation.Completion.t) ->
      match Definition.Operation.Completion.outcome completion with
      | Fields.Outcome.Ok -> Skip
      | Cancelled -> Emit { level = Logs.Debug; message = message completion }
      | Not_found | Conflict | Throttled | Error | Exception | Timeout ->
          Emit { level = Logs.Error; message = message completion })

let metrics =
  [
    Metric.Projection.sample resolutions
      ~get:(fun
          (completion : (unit, finish) Definition.Operation.Completion.t) ->
        Some
          ({ outcome = Definition.Operation.Completion.outcome completion }, 1L));
    Metric.Projection.sample ~needs_duration:true resolution_duration
      ~get:(fun
          (completion : (unit, finish) Definition.Operation.Completion.t) ->
        Option.map (Definition.Operation.Completion.duration_ns completion)
          ~f:(fun duration ->
            ( { outcome = Definition.Operation.Completion.outcome completion },
              duration )));
    Metric.Projection.sample resolved
      ~get:(fun
          (completion : (unit, finish) Definition.Operation.Completion.t) ->
        Option.bind (Definition.Operation.Completion.finish completion)
          ~f:(fun (finish : finish) ->
            Option.map finish.source ~f:(fun source ->
                (({ source } : source_labels), 1L))));
  ]

let operation =
  Definition.Operation.define ~name:"awskit.credentials.resolve"
    ~doc:"Resolve credentials for one signed request" ~source
    ~span_kind:Fields.Span_kind.Internal
    ~start:(fun () -> Fields.Fields.empty)
    ~classify ~finish:finish_fields ~log ~metrics ()
