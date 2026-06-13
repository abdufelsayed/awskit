(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)

type t = (string * string) list
(** User metadata keys and values.

    Keys should be supplied without the [x-amz-meta-] prefix. The S3 request
    layer adds the prefix when emitting headers and strips it when parsing
    response metadata. *)
