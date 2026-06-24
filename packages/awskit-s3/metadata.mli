(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)

type t
(** Opaque validated metadata collection.

    Keys are supplied without the [x-amz-meta-] prefix. The S3 request layer
    adds the prefix when emitting headers and strips it when parsing response
    metadata. Duplicate keys are rejected case-insensitively. *)

type entry = { key : string; value : Header_value.t }
(** One validated metadata entry. *)

val empty : t
(** Empty metadata collection. *)

val of_list : (string * string) list -> (t, Awskit.Error.t) result
(** Validate metadata entries and preserve their insertion order. *)

val of_list_exn : (string * string) list -> t
(** Like {!val:of_list}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_list : t -> (string * string) list
(** Return raw unprefixed key/value pairs in insertion order. *)

val entries : t -> entry list
(** Return typed metadata entries in insertion order. *)

val add : key:string -> value:string -> t -> (t, Awskit.Error.t) result
(** Add one metadata entry at the end of the collection, rejecting invalid or
    duplicate keys. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print metadata as raw key/value pairs. *)

val equal : t -> t -> bool
(** Compare two metadata collections in insertion order.

    Equality is order-sensitive. Keys compare by their exact unprefixed string
    spelling and values compare by their exact header value spelling. *)
