module Sources = Observation.Sources

module Transfer_definition = Observation.Transfer.Make_source (struct
  let source = Sources.transfer
end)

module Simulator_logical_operation = Observation.Logical_operation.Make (struct
  type t = Operation.t

  let equal = Operation.equal
  let values = Operation.all
  let to_string = Operation.to_string
  let source = Sources.operation
end)

module For_transfer = struct
  type summary = Transfer_definition.summary = {
    logical_bytes : int64;
    parts : int;
  }

  module Make = Transfer_definition.Make
end

module For_simulator = struct
  let complete = Simulator_logical_operation.complete
  let complete_artifact = Observation.Artifact.complete
  let complete_artifact_signing = Observation.Artifact_signing.complete

  let complete_credential_resolution ~credentials () =
    Awskit.Observability.For_service.Operation.project
      Awskit.Observability.For_service.Credential_resolution.operation ~start:()
      ~result:(Ok (Ok credentials))
      ~default_outcome:Awskit.Observability.Outcome.Ok ~duration_ns:None
end
