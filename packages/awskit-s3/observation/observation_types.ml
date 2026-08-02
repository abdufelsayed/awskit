open Base
module F = Awskit.Observability.For_service
module Operation = Operation
module Sources = Observation_sources
include Observation_dimensions

let operation_values = Operation.all

let operation_dimension =
  F.Dimension.Enum.define ~name:"aws.operation" ~equal:Operation.equal
    ~values:
      (List.map operation_values ~f:(fun operation ->
           (operation, Operation.to_string operation)))
