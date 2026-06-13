(** S3 object storage classes. *)

type t =
  | Standard
  | Standard_ia
  | Onezone_ia
  | Intelligent_tiering
  | Glacier
  | Glacier_ir
  | Deep_archive

val to_string : t -> string
(** Render the AWS storage-class spelling used in headers and XML. *)

val of_string : string -> t option
(** Parse a known AWS storage-class spelling. Returns [None] for unsupported or
    forward-compatible values. *)
