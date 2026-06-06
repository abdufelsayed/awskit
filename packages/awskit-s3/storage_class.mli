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
val of_string : string -> t option
