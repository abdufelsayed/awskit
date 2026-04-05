open Base

type t = { key : string; value : string } [@@deriving show, eq]
(** S3 object tag.

    Tags are key-value pairs attached to S3 objects. Used for categorization,
    lifecycle rules, and access control.

    {[
    let tag = { Tag.key = "environment"; value = "production" }
    ]} *)
