(** AWS region names.

    Region names are an open set, so validation rejects only values that cannot
    sensibly be used in request signing or host construction. *)

type t = private string
(** Validated region identifier, for example ["us-east-1"].

    The type is private so callers can inspect it as a string through
    {!val:to_string} but must construct it through validation. *)

val of_string : string -> (t, Error.t) result
(** Validate a region string.

    Region identifiers are intentionally permissive to support AWS partitions,
    local simulators, S3-compatible services, and endpoint overrides. Empty
    strings, leading/trailing whitespace, and control/delete characters are
    rejected. Other non-empty strings are preserved exactly for endpoint
    resolution and SigV4 signing scopes. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_string : t -> string
(** Return the region string used in endpoints and SigV4 signing scopes. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the region string. *)

val equal : t -> t -> bool
(** Compare two validated region identifiers. *)
