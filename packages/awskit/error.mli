(** Structured core AWS SDK errors.

    Service packages preserve this value and add classifier helpers over it.
    Errors carry a structured kind plus an outer context stack. Human output is
    optimized for logs and CLI diagnostics; sexp output is optimized for tests
    and structured debugging. *)

type validation = { field : string option; message : string }
(** Caller/input validation error. *)

type transport = { message : string; retryable : bool; cause : string option }
(** HTTP/runtime transport failure. [cause] is a compact runtime exception
    string when the adapter caught one. *)

type service = {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}
(** AWS service error response. *)

type body = { message : string; limit : int64 option }
(** Request or response body failure. *)

type operation = {
  service : string option;
  name : string;
  resource : string option;
}
(** High-level SDK operation context. *)

type retry = { attempt : int; max_attempts : int option; reason : string }
(** Retry context attached to the returned final error. *)

type context =
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

type kind =
  | Validation of validation
  | Signing of string
  | Transport of transport
  | Service of service
  | Decode of string
  | Body of body
  | Multiple of t list

and t

exception Awskit_error of t

val kind : t -> kind
val context : t -> context list
val validation : ?field:string -> string -> t
val signing : string -> t
val transport : ?cause:string -> retryable:bool -> string -> t
val service : service -> t
val decode : string -> t
val body : ?limit:int64 -> string -> t
val multiple : t list -> t
val with_context : string -> t -> t
val with_sexp_context : Base.Sexp.t -> t -> t

val with_operation :
  ?service:string -> name:string -> ?resource:string -> unit -> t -> t

val with_retry : attempt:int -> ?max_attempts:int -> reason:string -> t -> t
val retry_class : t -> retry_class
val is_validation : t -> bool
val validation_field : t -> string option
val is_not_found : t -> bool
val service_code : t -> string option
val service_status : t -> int option
val sexp_of_validation : validation -> Base.Sexp.t
val sexp_of_transport : transport -> Base.Sexp.t
val sexp_of_service : service -> Base.Sexp.t
val sexp_of_body : body -> Base.Sexp.t
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
val raise : t -> 'a
val get_ok_exn : ('a, t) result -> 'a
val equal : t -> t -> bool
