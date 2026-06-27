(** S3 bucket name. *)

type t
(** Opaque S3 bucket name. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Validate an S3 bucket name using Awskit's bucket-name rules. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the bucket name string. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the bucket name. *)

val equal : t -> t -> bool
(** Compare two bucket names. *)
