(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)

type t = (string * string) list
