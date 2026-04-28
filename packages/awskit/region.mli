(** AWS region names.

    Region names are an open set, so validation rejects only values that cannot
    sensibly be used in request signing or host construction. *)

type t = private string

val of_string : string -> (t, Error.t) result
val of_string_exn : string -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
