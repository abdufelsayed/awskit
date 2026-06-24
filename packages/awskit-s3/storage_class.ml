type t =
  | Standard
  | Reduced_redundancy
  | Standard_ia
  | Onezone_ia
  | Intelligent_tiering
  | Glacier
  | Glacier_ir
  | Deep_archive
  | Outposts
  | Snow
  | Express_onezone
  | Fsx_openzfs
  | Fsx_ontap
  | Unknown of string

let to_string = function
  | Standard -> "STANDARD"
  | Reduced_redundancy -> "REDUCED_REDUNDANCY"
  | Standard_ia -> "STANDARD_IA"
  | Onezone_ia -> "ONEZONE_IA"
  | Intelligent_tiering -> "INTELLIGENT_TIERING"
  | Glacier -> "GLACIER"
  | Glacier_ir -> "GLACIER_IR"
  | Deep_archive -> "DEEP_ARCHIVE"
  | Outposts -> "OUTPOSTS"
  | Snow -> "SNOW"
  | Express_onezone -> "EXPRESS_ONEZONE"
  | Fsx_openzfs -> "FSX_OPENZFS"
  | Fsx_ontap -> "FSX_ONTAP"
  | Unknown value -> value

let of_string = function
  | "STANDARD" -> Standard
  | "REDUCED_REDUNDANCY" -> Reduced_redundancy
  | "STANDARD_IA" -> Standard_ia
  | "ONEZONE_IA" -> Onezone_ia
  | "INTELLIGENT_TIERING" -> Intelligent_tiering
  | "GLACIER" -> Glacier
  | "GLACIER_IR" -> Glacier_ir
  | "DEEP_ARCHIVE" -> Deep_archive
  | "OUTPOSTS" -> Outposts
  | "SNOW" -> Snow
  | "EXPRESS_ONEZONE" -> Express_onezone
  | "FSX_OPENZFS" -> Fsx_openzfs
  | "FSX_ONTAP" -> Fsx_ontap
  | value -> Unknown value
