(** Structured core AWS SDK errors.

    Service packages should preserve this value and add classifier helpers over
    it instead of widening errors into service-specific polymorphic variants. *)

type validation = { field : string option; message : string }
(** Caller/input validation error. [field] names the invalid field when the
    producer can identify one. *)

type transport = { message : string; retryable : bool; cause : string option }
(** HTTP/runtime transport failure. [retryable] records whether the adapter
    considers another attempt safe from a transport perspective. *)

type service = {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}
(** AWS service error response.

    [code] and [message] come from the service error payload when one could be
    decoded. [request_id] and [host_id] are copied from response metadata to
    make support/debugging easier. [body] is retained only when the service
    package captured a bounded response body. *)

type body = { message : string; limit : int64 option }
(** Request or response body failure. [limit] is set for bounded helpers that
    rejected a body because it exceeded the configured byte limit. *)

type retry_class =
  [ `Retryable
  | `Throttled
  | `Auth
  | `Conflict
  | `Not_found
  | `Fatal
  | `Unknown ]
(** Coarse retry/handling classification derived from a structured error. *)

type t =
  | Validation of validation
  | Signing of string
  | Transport of transport
  | Service of service
  | Decode of string
  | Body of body  (** Shared error value used by core and service packages. *)

val validation : ?field:string -> string -> t
(** Build a validation error. *)

val signing : string -> t
(** Build a SigV4 signing error. *)

val transport : ?cause:string -> retryable:bool -> string -> t
(** Build a transport error. *)

val service : service -> t
(** Wrap an AWS service error response. *)

val decode : string -> t
(** Build an XML/JSON/header decoding error. *)

val body : ?limit:int64 -> string -> t
(** Build a request/response body error. *)

val retry_class : t -> retry_class
(** Classify an error for retry and common caller handling. *)

val is_not_found : t -> bool
(** [true] for generic not-found errors, including HTTP 404 service errors. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for diagnostics. *)

val to_string_hum : t -> string
(** Human-readable error message suitable for logs and exceptions. *)

val equal : t -> t -> bool
(** Structural equality for tests and classifiers. *)
