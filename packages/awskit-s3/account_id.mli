(** AWS account id. *)

type t
(** Opaque 12-digit AWS account id. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Validate an account id as exactly 12 ASCII digits. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the account id string. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the account id. *)

val equal : t -> t -> bool
(** Compare two account ids. *)
