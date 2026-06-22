(** Retry policy shared by AWS service packages.

    The policy only decides whether another attempt is allowed and how long to
    wait before it. Service packages still decide whether a request body is
    replayable and whether a response body must be consumed before retrying. *)

type t
type budget
type budget_state

val create :
  ?max_attempts:int ->
  ?base_delay:Ptime.Span.t ->
  ?max_delay:Ptime.Span.t ->
  ?jitter:float ->
  ?budget:budget ->
  unit ->
  (t, Error.t) result
(** Create a retry policy.

    [max_attempts] includes the first request attempt. A value of [1] disables
    retries. [jitter] is a fractional multiplier in the range [[0, 1]]. A value
    of [0] keeps deterministic exponential delays; a value of [1] uses a
    full-jitter delay between zero and the capped exponential delay. *)

val create_exn :
  ?max_attempts:int ->
  ?base_delay:Ptime.Span.t ->
  ?max_delay:Ptime.Span.t ->
  ?jitter:float ->
  ?budget:budget ->
  unit ->
  t
(** Like {!val:create}, but raises [Error.Awskit_error] carrying the structured
    validation error on validation failure. *)

val default : t
(** Conservative default policy for AWS SDK calls. *)

val disabled : t
(** Policy that never retries. *)

val max_attempts : t -> int
(** Maximum attempts, including the first request. *)

val base_delay : t -> Ptime.Span.t
(** Initial exponential-backoff delay. *)

val max_delay : t -> Ptime.Span.t
(** Upper bound for computed backoff delays. *)

val jitter : t -> float
(** Jitter fraction in the range [[0, 1]]. *)

val budget :
  ?capacity:int ->
  ?retry_cost:int ->
  ?timeout_cost:int ->
  ?success_credit:int ->
  unit ->
  (budget, Error.t) result
(** Create a retry budget.

    The budget controls how many retries a runtime may spend under repeated
    transient failures. Capacity and every cost/credit must be strictly
    positive. *)

val budget_exn :
  ?capacity:int ->
  ?retry_cost:int ->
  ?timeout_cost:int ->
  ?success_credit:int ->
  unit ->
  budget
(** Like {!val:budget}, but raises [Error.Awskit_error] on validation failure.
*)

val retry_budget : t -> budget
(** Return the budget attached to a retry policy. *)

val budget_capacity : budget -> int
(** Maximum capacity for a retry budget. *)

val retry_cost : t -> Error.t -> int
(** Return the budget cost for retrying [error]. *)

val success_credit : t -> int
(** Return the amount credited after a successful request. *)

val initial_budget_state : t -> budget_state
(** Create a fresh per-operation retry budget state. *)

val available_capacity : budget_state -> int
(** Return remaining capacity in a per-operation retry budget. *)

val charge_retry : t -> budget_state -> Error.t -> budget_state option
(** Spend retry capacity for [error]. [None] means retry budget is exhausted. *)

val credit_success : t -> budget_state -> budget_state
(** Credit capacity after a successful attempt, capped at the policy budget. *)

val delay :
  random_float:(upper_bound:float -> float) ->
  t ->
  attempt:int ->
  error:Error.t ->
  Ptime.Span.t option
(** [delay t ~attempt ~error] returns the delay before retrying after [attempt]
    failed. [None] means the error should be returned to the caller.

    [random_float] is required so jitter always comes from the active runtime
    capability instead of process-global mutable state. It receives the
    exclusive upper bound for the random delay in seconds and must return a
    value in [[0, upper_bound]]. Values outside the range are clamped. *)
