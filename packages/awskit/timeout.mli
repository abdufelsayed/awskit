(** Runtime-neutral timeout policy. *)

type phase =
  [ `Connect | `Attempt | `Operation | `Request_body | `Response_body | `Drain ]
(** Named timeout phases a runtime may enforce. [`Operation] is scoped to one
    runtime transport operation. For service calls that retry, the operation
    timer is applied independently to each attempt and does not include retry
    sleeps. *)

type policy
(** Timeout policy for runtime adapters. [None] means no timeout for that phase.
*)

val create :
  ?connect:Ptime.Span.t ->
  ?attempt:Ptime.Span.t ->
  ?operation:Ptime.Span.t ->
  ?request_body:Ptime.Span.t ->
  ?response_body:Ptime.Span.t ->
  ?drain:Ptime.Span.t ->
  unit ->
  (policy, Error.t) result
(** Build a timeout policy. Present spans must be positive. *)

val create_exn :
  ?connect:Ptime.Span.t ->
  ?attempt:Ptime.Span.t ->
  ?operation:Ptime.Span.t ->
  ?request_body:Ptime.Span.t ->
  ?response_body:Ptime.Span.t ->
  ?drain:Ptime.Span.t ->
  unit ->
  policy
(** Exception-raising variant of {!create}. *)

val default : policy
(** Conservative default policy. *)

val disabled : policy
(** Policy with all timeout phases disabled. *)

val span : policy -> phase -> Ptime.Span.t option
(** Return the configured span for a phase. *)
