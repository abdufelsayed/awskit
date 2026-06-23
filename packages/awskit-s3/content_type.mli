(** Validated [Content-Type] value. *)

type t
(** Opaque content type preserving the caller's original spelling. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Validate a non-empty content type without control characters. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the original content type spelling. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the content type. *)

val equal : t -> t -> bool
(** Compare two content types. *)
