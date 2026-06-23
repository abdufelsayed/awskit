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

exception Awskit_error of t

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

type context = private
  | Message of string
  | Operation of operation
  | Retry of retry
  | Sexp of Base.Sexp.t

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
val context : t -> context list

val retry_class : t -> retry_class
(** Coarse retry/handling classification. [retry_class] aggregates [Multiple]
    errors by caller-handling priority: [Auth] > [Throttled] > [Retryable] >
    [Conflict] > [Not_found] > [Fatal] > [Unknown]. This means a retryable or
    throttled nested error can outrank a fatal nested validation/body/decode
    error. *)

val is_validation : t -> bool
val is_credentials : t -> bool
val is_endpoint : t -> bool
val is_transport : t -> bool
val is_timeout : t -> bool
val is_cancelled : t -> bool
val validation_field : t -> string option
val is_not_found : t -> bool
val service_code : t -> string option
val service_status : t -> int option
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
val pp : Format.formatter -> t -> unit
val pp_sexp : Format.formatter -> t -> unit
val to_string_hum : t -> string
val to_sexp_string_hum : t -> string
val equal : t -> t -> bool

module Unsafe_diagnostics : sig
  (** Explicit escape hatch for raw diagnostic material.

      Values exposed by this module may contain service response bodies,
      authorization headers, security tokens, signatures, cookies, credentials,
      or other secret-bearing data. Do not use these functions for application
      logs or exception messages. *)

  val service_headers : t -> (string * string) list option
  val service_body : t -> string option
  val to_sexp_unredacted : t -> Base.Sexp.t
end

module Producer : sig
  (** Producer-side error constructors.

      Custom runtimes, Awskit service packages, runtime adapters, simulators,
      and tests use this module to construct the shared opaque error type.
      Application code should not construct {!type:t} values directly; inspect,
      classify, and display returned errors instead. *)

  val validation : ?field:string -> string -> t
  val credentials : ?source:string -> string -> t
  val signing : string -> t
  val endpoint : ?uri:string -> string -> t
  val transport : ?cause:string -> retryable:bool -> string -> t
  val timeout : ?operation:string -> string -> t
  val cancelled : ?reason:string -> unit -> t

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

  val decode : string -> t
  val body : ?limit:int64 -> string -> t

  val retry_exhausted :
    attempts:int -> ?max_attempts:int -> ?last_error:t -> string -> t

  val not_supported : ?feature:string -> string -> t
  val multiple : t list -> t
  val with_context : string -> t -> t
  val with_sexp_context : Base.Sexp.t -> t -> t

  val with_operation :
    ?service:string -> name:string -> ?resource:string -> unit -> t -> t

  val with_retry : attempt:int -> ?max_attempts:int -> reason:string -> t -> t
  val raise : t -> 'a
  val get_ok_exn : ('a, t) result -> 'a
end
