open Base

(** S3 storage classes.

    Different storage classes have different availability, durability, and cost
    characteristics:

    - [Standard] — General purpose, frequently accessed data
    - [Standard_ia] — Infrequently accessed data
    - [Onezone_ia] — Infrequently accessed, lower availability
    - [Intelligent_tiering] — Automatic optimization
    - [Glacier] — Archive storage, retrieval takes minutes-hours
    - [Glacier_ir] — Glacier with instant retrieval
    - [Deep_archive] — Long-term archive, retrieval takes hours
    - [Express_onezone] — High performance, single AZ *)
type t =
  | Standard  (** General purpose, frequently accessed data *)
  | Standard_ia  (** Infrequently accessed data *)
  | Onezone_ia  (** Infrequently accessed, single AZ *)
  | Intelligent_tiering  (** Automatic tiering optimization *)
  | Glacier  (** Archive storage *)
  | Glacier_ir  (** Glacier with instant retrieval *)
  | Deep_archive  (** Long-term archive *)
  | Express_onezone  (** High performance, single AZ *)
[@@deriving show, eq]

(** Convert to S3 API string. *)
let to_string = function
  | Standard -> "STANDARD"
  | Standard_ia -> "STANDARD_IA"
  | Onezone_ia -> "ONEZONE_IA"
  | Intelligent_tiering -> "INTELLIGENT_TIERING"
  | Glacier -> "GLACIER"
  | Glacier_ir -> "GLACIER_IR"
  | Deep_archive -> "DEEP_ARCHIVE"
  | Express_onezone -> "EXPRESS_ONEZONE"

(** Parse from S3 API string. Returns [None] if unknown. *)
let of_string = function
  | "STANDARD" -> Some Standard
  | "STANDARD_IA" -> Some Standard_ia
  | "ONEZONE_IA" -> Some Onezone_ia
  | "INTELLIGENT_TIERING" -> Some Intelligent_tiering
  | "GLACIER" -> Some Glacier
  | "GLACIER_IR" -> Some Glacier_ir
  | "DEEP_ARCHIVE" -> Some Deep_archive
  | "EXPRESS_ONEZONE" -> Some Express_onezone
  | _ -> None
