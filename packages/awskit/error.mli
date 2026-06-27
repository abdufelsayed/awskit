(** Structured core AWS SDK errors.

    Service packages preserve this value and add classifier helpers over it.
    Errors carry a structured kind plus an outer context stack. Human output is
    optimized for logs and CLI diagnostics; sexp output is optimized for
    structured debugging.

    Application code should treat this module as a consumer API: inspect,
    classify, and print errors returned by Awskit operations.

    The {!module:Producer} submodule is the producer-side API for custom runtime
    authors, awskit service packages, runtime adapters, simulators, and tests
    that need to construct the shared opaque error type. It is not part of the
    application-facing consumer contract. *)

type t
(** Opaque structured error. Use the accessors below for classification and
    diagnostics; use {!module:Producer} only at implementation boundaries. *)

exception Awskit_error of t
(** Raised by explicit [_exn] helpers. Non-exception APIs return {!type:t} in
    their [Error] case. *)

type validation = private { field : string option; message : string }
(** Caller/input validation error. *)

type credentials_error = private { source : string option; message : string }
(** Credential discovery, loading, or validation failure. *)

type signing_error = private { message : string }
(** Request signing failure. *)

type endpoint_error = private { uri : string option; message : string }
(** Endpoint resolution or validation failure. *)

type transport = private {
  message : string;
  retryable : bool;
  cause : string option;
}
(** HTTP/runtime transport failure. [cause] is a compact runtime exception
    string when the adapter caught one. *)

type service = private {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}
(** AWS service error response.

    Service values returned through public accessors and public diagnostics are
    safe for logs: secret-bearing headers are redacted and response bodies are
    replaced with a redaction marker. Use {!module:Unsafe_diagnostics} when an
    implementor or test needs the raw service headers or body. *)

type body = private { message : string; limit : int64 option }
(** Request or response body failure. *)

type decode = private { message : string }
(** Response metadata or payload decoding failure. *)

type timeout = private { operation : string option; message : string }
(** Operation, connection, request, or response timeout. *)

type cancellation = private { reason : string option }
(** Caller or runtime cancellation. *)

type retry_exhausted = private {
  attempts : int;
  max_attempts : int option;
  last_error : t option;
  message : string;
}
(** Retry policy exhaustion. [last_error] preserves the terminal failure when
    one is available. *)

type not_supported = private { feature : string option; message : string }
(** Unsupported operation, feature, or runtime capability. *)

type operation = private {
  service : string option;
  name : string;
  resource : string option;
}
(** High-level SDK operation context. *)

type retry = private {
  attempt : int;
  max_attempts : int option;
  reason : string;
}
(** Retry context attached to the returned final error. *)

(** Additional diagnostic context. Public accessors return redacted context. *)
type context = private
  | Message of string
  | Operation of operation
  | Retry of retry
  | Sexp of Base.Sexp.t

(** Coarse caller-handling class used by retry policy and applications. *)
type retry_class =
  | Retryable
  | Throttled
  | Auth
  | Conflict
  | Not_found
  | Fatal
  | Unknown

type kind = private
  | Validation of validation
  | Credentials of credentials_error
  | Signing of signing_error
  | Endpoint of endpoint_error
  | Transport of transport
  | Timeout of timeout
  | Cancelled of cancellation
  | Service of service
  | Body of body
  | Decode of decode
  | Retry_exhausted of retry_exhausted
  | Not_supported of not_supported
  | Multiple of t list

val kind : t -> kind
(** Return the redacted top-level error kind. *)

val context : t -> context list
(** Return the redacted context stack, newest context first. *)

val retry_class : t -> retry_class
(** Coarse retry/handling classification. [retry_class] aggregates [Multiple]
    errors by caller-handling priority: [Auth] > [Throttled] > [Retryable] >
    [Conflict] > [Not_found] > [Fatal] > [Unknown]. This means a retryable or
    throttled nested error can outrank a fatal nested validation/body/decode
    error. *)

val is_validation : t -> bool
(** [true] when [t] or a nested [Multiple] error contains validation failure. *)

val is_credentials : t -> bool
(** [true] when [t] or a nested [Multiple] error contains a credentials failure.
*)

val is_endpoint : t -> bool
(** [true] when [t] or a nested [Multiple] error contains an endpoint failure.
*)

val is_transport : t -> bool
(** [true] when [t] or a nested [Multiple] error contains a transport failure.
*)

val is_timeout : t -> bool
(** [true] when [t] or a nested [Multiple] error contains a timeout. *)

val is_cancelled : t -> bool
(** [true] when [t] or a nested [Multiple] error contains SDK-level
    cancellation. Runtime-native cancellation such as [Lwt.Canceled] may be
    preserved as an exception instead of converted to [Cancelled]. *)

val validation_field : t -> string option
(** First validation field in [t], if one is available. *)

val is_not_found : t -> bool
(** [true] when {!val:retry_class} classifies [t] as [Not_found]. *)

val service_code : t -> string option
(** First AWS service error code in [t], if one is available. *)

val service_status : t -> int option
(** First AWS service HTTP status in [t], if one is available. *)

val sexp_of_validation : validation -> Base.Sexp.t
val sexp_of_credentials_error : credentials_error -> Base.Sexp.t
val sexp_of_signing_error : signing_error -> Base.Sexp.t
val sexp_of_endpoint_error : endpoint_error -> Base.Sexp.t
val sexp_of_transport : transport -> Base.Sexp.t
val sexp_of_service : service -> Base.Sexp.t
val sexp_of_body : body -> Base.Sexp.t
val sexp_of_decode : decode -> Base.Sexp.t
val sexp_of_timeout : timeout -> Base.Sexp.t
val sexp_of_cancellation : cancellation -> Base.Sexp.t
val sexp_of_retry_exhausted : retry_exhausted -> Base.Sexp.t
val sexp_of_not_supported : not_supported -> Base.Sexp.t
val sexp_of_operation : operation -> Base.Sexp.t
val sexp_of_retry : retry -> Base.Sexp.t
val sexp_of_context : context -> Base.Sexp.t
val sexp_of_retry_class : retry_class -> Base.Sexp.t
val sexp_of_kind : kind -> Base.Sexp.t

val sexp_of_t : t -> Base.Sexp.t
(** Render a redacted S-expression diagnostic. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a redacted human diagnostic. *)

val pp_sexp : Format.formatter -> t -> unit
(** Pretty-print the redacted S-expression diagnostic. *)

val to_string_hum : t -> string
(** Render a redacted human diagnostic string. *)

val to_sexp_string_hum : t -> string
(** Render a redacted S-expression diagnostic string. *)

val equal : t -> t -> bool
(** Structural equality on error values. *)

module Unsafe_diagnostics : sig
  (** Explicit escape hatch for raw diagnostic material.

      Values exposed by this module may contain service response bodies,
      authorization headers, security tokens, signatures, cookies, credentials,
      or other secret-bearing data. Do not use these functions for application
      logs or exception messages. *)

  val service_headers : t -> (string * string) list option
  (** Raw service headers when [t] is a service error. *)

  val service_body : t -> string option
  (** Raw service response body when [t] is a service error and the producer
      supplied it. *)

  val to_sexp_unredacted : t -> Base.Sexp.t
  (** Render an unredacted S-expression diagnostic. *)
end

module Producer : sig
  (** Producer-side error constructors.

      Custom runtimes, Awskit service packages, runtime adapters, simulators,
      and tests use this module to construct the shared opaque error type.
      Application code should not construct {!type:t} values directly; inspect,
      classify, and display returned errors instead. *)

  val validation : ?field:string -> string -> t
  (** Construct a caller/input validation error. *)

  val credentials : ?source:string -> string -> t
  (** Construct a credential discovery, loading, or validation error. *)

  val signing : string -> t
  (** Construct a request signing error. *)

  val endpoint : ?uri:string -> string -> t
  (** Construct an endpoint resolution or validation error. *)

  val transport : ?cause:string -> retryable:bool -> string -> t
  (** Construct an HTTP/runtime transport error. Set [retryable] according to
      whether retrying the same request may succeed. *)

  val timeout : ?operation:string -> string -> t
  (** Construct a timeout error. [operation] should name the timeout phase or
      SDK operation, not include request data. *)

  val cancelled : ?reason:string -> unit -> t
  (** Construct an SDK-level cancellation error. Prefer preserving
      runtime-native cancellation when an adapter can do so. *)

  val service :
    status:int ->
    ?code:string ->
    ?message:string ->
    ?request_id:string ->
    ?host_id:string ->
    headers:(string * string) list ->
    ?body:string ->
    unit ->
    t
  (** Construct an AWS service error response. Public diagnostics redact headers
      and body. *)

  val decode : string -> t
  (** Construct a response metadata or payload decoding error. *)

  val body : ?limit:int64 -> string -> t
  (** Construct a request or response body error. [limit] records the enforced
      byte limit when relevant. *)

  val retry_exhausted :
    attempts:int -> ?max_attempts:int -> ?last_error:t -> string -> t
  (** Construct a retry exhaustion error, preserving the last observed failure
      when available. *)

  val not_supported : ?feature:string -> string -> t
  (** Construct an unsupported operation, feature, or runtime capability error.
  *)

  val multiple : t list -> t
  (** Combine multiple errors. Empty lists become a validation error; singleton
      lists are returned unchanged. *)

  val with_context : string -> t -> t
  (** Push a redacted message context onto an error. *)

  val with_sexp_context : Base.Sexp.t -> t -> t
  (** Push a redacted structured context onto an error. *)

  val with_operation :
    ?service:string -> name:string -> ?resource:string -> unit -> t -> t
  (** Push operation context onto an error. *)

  val with_retry : attempt:int -> ?max_attempts:int -> reason:string -> t -> t
  (** Push retry-attempt context onto an error. *)

  val raise : t -> 'a
  (** Raise [Awskit_error t]. *)

  val get_ok_exn : ('a, t) result -> 'a
  (** Extract [Ok] or raise [Awskit_error] for [Error]. *)
end
