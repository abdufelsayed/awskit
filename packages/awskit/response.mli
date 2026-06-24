(** Body-free HTTP response metadata.

    Runtime adapters keep the response body separate and expose it only through
    scoped readers. This record contains status, headers, and AWS request ids
    that are useful after the body has been consumed. *)

type t = private {
  status : int;
  headers : (string * string) list;
  request_id : string option;
  host_id : string option;
}

val create :
  status:int -> ?headers:(string * string) list -> unit -> (t, Error.t) result
(** Create response metadata from an HTTP status and raw headers. AWS request id
    fields are extracted from standard S3/AWS headers when present. *)

val create_exn : status:int -> ?headers:(string * string) list -> unit -> t
(** Like {!val:create}, but raises [Error.Awskit_error] carrying the structured
    validation error on validation failure. *)

val status : t -> int
(** Return the HTTP status code. *)

val headers : t -> (string * string) list
(** Return response headers in their stored order. *)

val header : t -> string -> string option
(** Look up a header case-insensitively. *)

val required_header : t -> string -> (string, Error.t) result
(** Return a header or a decode error when it is absent. *)

val header_int : t -> string -> (int option, Error.t) result
(** Parse an optional integer header. Invalid integer text is a decode error. *)

val header_int64 : t -> string -> (int64 option, Error.t) result
(** Parse an optional 64-bit integer header. Invalid integer text is a decode
    error. *)

val is_success : t -> bool
(** [true] for HTTP 2xx statuses. *)

val request_id : t -> string option
(** Return the AWS request id, when present. *)

val host_id : t -> string option
(** Return the extended AWS host id, when present. *)
