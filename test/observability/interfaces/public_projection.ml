open Base
module O = Awskit.Observability
module P = O.For_projection

let metric_family_name (observation : P.Metric.Observation.t) =
  observation |> P.Metric.Observation.family |> P.Metric.Family.name

let public_diagnostic_names (completion : P.Operation.Completion.t) =
  completion
  |> P.Operation.Completion.diagnostics
  |> List.map ~f:O.Diagnostic.Public.name
