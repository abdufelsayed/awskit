type decision_kind =
  | Scheduled
  | Non_replayable_request
  | Attempts_exhausted
  | Budget_exhausted
  | Policy_denied

(** A retry transition calculated by the service-domain policy. [event_error] is
    the original failure used for classification; [error] is the SDK-visible
    error after retry metadata is attached. *)
type decision =
  | Stop of {
      error : Awskit.Error.t;
      event_error : Awskit.Error.t;
      decision : decision_kind;
    }
  | Retry_after of {
      budget_state : Awskit.Retry.budget_state;
      delay : Ptime.Span.t;
      error : Awskit.Error.t;
    }
