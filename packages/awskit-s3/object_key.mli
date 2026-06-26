(** S3 object key values. *)

type t
(** Opaque non-empty S3 object key. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Validate a non-empty UTF-8 object key up to S3's 1,024-byte object-key
    limit. Relative [..] path segments are accepted only when they never
    outnumber preceding non-relative segments while scanning left to right. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the object key string. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the object key. *)

val equal : t -> t -> bool
(** Compare two object keys. *)

module Prefix : sig
  type t
  (** Non-empty object listing prefix. Use [None] in operation options and page
      records for "no prefix" rather than constructing an empty prefix value. *)

  val of_string : string -> (t, Awskit.Error.t) result
  (** Validate a non-empty listing prefix. *)

  val of_string_exn : string -> t
  (** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_string : t -> string
  (** Return the prefix string. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the prefix. *)

  val equal : t -> t -> bool
  (** Compare two prefixes. *)
end

module Delimiter : sig
  type t
  (** Object listing delimiter. *)

  val of_string : string -> (t, Awskit.Error.t) result
  (** Validate a non-empty listing delimiter. *)

  val of_string_exn : string -> t
  (** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_string : t -> string
  (** Return the delimiter string. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the delimiter. *)

  val equal : t -> t -> bool
  (** Compare two delimiters. *)
end
