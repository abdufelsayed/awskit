(** Validated HTTP header field value. *)

type t
(** Opaque header value that is non-empty and contains no control characters. *)

val of_string : field:string -> string -> (t, Awskit.Error.t) result
(** Validate a header value for [field]. *)

val of_string_exn : field:string -> string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the original header value spelling. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the header value. *)

val equal : t -> t -> bool
(** Compare two header values. *)
