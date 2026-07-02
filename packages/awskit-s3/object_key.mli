(** S3 object key values. *)

type t = string
(** S3 object key text.

    Constructors validate eagerly. Operation APIs also validate object keys
    before request construction, so callers may pass plain strings directly to
    object-key arguments when they prefer deferred error handling. *)

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
  type t = string
  (** Object listing prefix text. Use [None] in operation options and page
      records for "no prefix" rather than passing an empty prefix value. *)

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
  type t = string
  (** Object listing delimiter text. *)

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
