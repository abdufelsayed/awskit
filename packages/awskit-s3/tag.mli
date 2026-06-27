(** S3 tag key/value pair. *)

type t
(** Opaque validated S3 tag. *)

val create : key:string -> value:string -> (t, Awskit.Error.t) result
(** Validate and create one S3 tag. Keys use up to 128 UTF-16 positions and
    values use up to 256 UTF-16 positions. Values may be empty. Keys and values
    allow letters, numbers, spaces, and [+ - = . _ : / @]. Tag keys beginning
    with [aws:] are reserved for AWS and rejected. *)

val create_exn : key:string -> value:string -> t
(** Like {!val:create}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val key : t -> string
(** Return the tag key. *)

val value : t -> string
(** Return the tag value. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the tag. *)

val equal : t -> t -> bool
(** Compare two tags. *)

module Set : sig
  type tag = t

  type t
  (** Validated tag collection preserving insertion order. S3 object tagging
      allows at most ten tags and treats keys case-sensitively. *)

  val empty : t
  (** Empty tag set. *)

  val of_list : tag list -> (t, Awskit.Error.t) result
  (** Validate a tag set from tags in insertion order.

      Duplicate keys are rejected case-sensitively, and S3's ten-tag limit is
      enforced. *)

  val of_list_exn : tag list -> t
  (** Like {!val:of_list}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_list : t -> tag list
  (** Return tags in insertion order. *)
end
