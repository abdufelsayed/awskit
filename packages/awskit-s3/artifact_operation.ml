open Base

type t =
  | Presign_get_object
  | Presign_put_object
  | Presign_head_object
  | Presign_delete_object
  | Presign_upload_part

let equal left right =
  match (left, right) with
  | Presign_get_object, Presign_get_object
  | Presign_put_object, Presign_put_object
  | Presign_head_object, Presign_head_object
  | Presign_delete_object, Presign_delete_object
  | Presign_upload_part, Presign_upload_part ->
      true
  | _ -> false

let to_string = function
  | Presign_get_object -> "PresignGetObject"
  | Presign_put_object -> "PresignPutObject"
  | Presign_head_object -> "PresignHeadObject"
  | Presign_delete_object -> "PresignDeleteObject"
  | Presign_upload_part -> "PresignUploadPart"

let all =
  [
    Presign_get_object;
    Presign_put_object;
    Presign_head_object;
    Presign_delete_object;
    Presign_upload_part;
  ]
