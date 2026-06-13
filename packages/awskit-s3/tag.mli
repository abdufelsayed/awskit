(** S3 tag key/value pair. *)

type t = { key : string; value : string }
(** One S3 tag. Keys and values are sent through S3 tagging XML/query encoding
    by operation helpers. *)
