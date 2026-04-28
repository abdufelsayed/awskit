(** Retry policy shared by AWS service packages.

    The policy only decides whether another attempt is allowed and how long to
    wait before it. Service packages still decide whether a request body is
    replayable and whether a response body must be consumed before retrying. *)

type t

val create :
  ?max_attempts:int ->
  ?base_delay:Ptime.Span.t ->
  ?max_delay:Ptime.Span.t ->
  unit ->
  (t, Error.t) result
(** Create a retry policy.

    [max_attempts] includes the first request attempt. A value of [1] disables
    retries. *)

val create_exn :
  ?max_attempts:int ->
  ?base_delay:Ptime.Span.t ->
  ?max_delay:Ptime.Span.t ->
  unit ->
  t

val default : t
(** Conservative default policy for AWS SDK calls. *)

val disabled : t
(** Policy that never retries. *)

val max_attempts : t -> int
val base_delay : t -> Ptime.Span.t
val max_delay : t -> Ptime.Span.t

val delay : t -> attempt:int -> error:Error.t -> Ptime.Span.t option
(** [delay t ~attempt ~error] returns the delay before retrying after [attempt]
    failed. [None] means the error should be returned to the caller. *)
