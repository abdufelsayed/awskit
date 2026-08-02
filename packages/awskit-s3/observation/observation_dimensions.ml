open Base

let outcome_dimension =
  Awskit.Observability.For_service.Dimension.Enum.define ~name:"outcome"
    ~equal:Poly.equal
    ~values:
      [
        (Awskit.Observability.Outcome.Ok, "ok");
        (Not_found, "not_found");
        (Conflict, "conflict");
        (Throttled, "throttled");
        (Error, "error");
        (Exception, "exception");
        (Cancelled, "cancelled");
        (Timeout, "timeout");
      ]

type replayability = Replayable | Non_replayable

let replayability_dimension =
  Awskit.Observability.For_service.Dimension.Enum.define
    ~name:"request.replayability" ~equal:Poly.equal
    ~values:[ (Replayable, "replayable"); (Non_replayable, "non_replayable") ]

type retry_decision =
  | Scheduled
  | Non_replayable_request
  | Attempts_exhausted
  | Budget_exhausted
  | Policy_denied

let retry_decision_dimension =
  Awskit.Observability.For_service.Dimension.Enum.define ~name:"retry.decision"
    ~equal:Poly.equal
    ~values:
      [
        (Scheduled, "scheduled");
        (Non_replayable_request, "non_replayable");
        (Attempts_exhausted, "attempts_exhausted");
        (Budget_exhausted, "budget_exhausted");
        (Policy_denied, "policy_denied");
      ]

let retry_class_dimension =
  Awskit.Observability.For_service.Dimension.Enum.define ~name:"retry.class"
    ~equal:Poly.equal
    ~values:
      [
        (Awskit.Error.Retryable, "retryable");
        (Throttled, "throttled");
        (Auth, "auth");
        (Conflict, "conflict");
        (Not_found, "not_found");
        (Fatal, "fatal");
        (Unknown, "unknown");
      ]

type transfer_direction = Upload | Download

let transfer_direction_dimension =
  Awskit.Observability.For_service.Dimension.Enum.define
    ~name:"transfer.direction" ~equal:Poly.equal
    ~values:[ (Upload, "upload"); (Download, "download") ]
