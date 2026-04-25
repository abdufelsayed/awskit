(** AWS region names.

    This module intentionally stays pure. It defines validation and the shared
    region type, while OS/environment discovery belongs in [awskit.unix].

    A region is still represented as a string because AWS region names are an
    open set. Validation only rejects values that cannot sensibly be used in
    request signing or host construction. *)

type t = string

val of_string : string -> (t, Error.base) result
val to_string : t -> string
