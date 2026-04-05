open Base

type t = (string * string) list [@@deriving show, eq]
(** Custom object metadata.

    S3 allows arbitrary key-value metadata on objects, stored as HTTP headers
    prefixed with [x-amz-meta-]. This type is a simple association list.

    {[
    let meta : Metadata.t = [ ("author", "alice"); ("version", "2") ]
    ]} *)
