type t =
  | Standard
  | Standard_ia
  | Onezone_ia
  | Intelligent_tiering
  | Glacier
  | Glacier_ir
  | Deep_archive

let to_string = function
  | Standard -> "STANDARD"
  | Standard_ia -> "STANDARD_IA"
  | Onezone_ia -> "ONEZONE_IA"
  | Intelligent_tiering -> "INTELLIGENT_TIERING"
  | Glacier -> "GLACIER"
  | Glacier_ir -> "GLACIER_IR"
  | Deep_archive -> "DEEP_ARCHIVE"

let of_string = function
  | "STANDARD" -> Some Standard
  | "STANDARD_IA" -> Some Standard_ia
  | "ONEZONE_IA" -> Some Onezone_ia
  | "INTELLIGENT_TIERING" -> Some Intelligent_tiering
  | "GLACIER" -> Some Glacier
  | "GLACIER_IR" -> Some Glacier_ir
  | "DEEP_ARCHIVE" -> Some Deep_archive
  | _ -> None
