(** Retry policy shared by AWS service packages.

    The policy only decides whether another attempt is allowed and how long to
    wait before it. Service packages still decide whether a request body is
    replayable and whether a response body must be consumed before retrying. *)

type t

val create :
  ?max_attempts:int ->
  ?base_delay:Ptime.Span.t ->
  ?max_delay:Ptime.Span.t ->
  ?jitter:float ->
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

val delay :
  ?random_float:(unit -> float) ->
  t ->
  attempt:int ->
  error:Error.t ->
  Ptime.Span.t option
(** [delay t ~attempt ~error] returns the delay before retrying after [attempt]
    failed. [None] means the error should be returned to the caller. *)
