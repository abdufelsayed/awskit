(** S3 object key values. *)

type t
(** Opaque non-empty S3 object key. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Validate a non-empty UTF-8 object key up to S3's 1,024-byte object-key
    limit. *)

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
  (** Object listing prefix. Empty prefixes are valid and mean no prefix
      restriction. *)

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Delimiter : sig
  type t
  (** Object listing delimiter. *)

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end
