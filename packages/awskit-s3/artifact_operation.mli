(** Bounded identity for connection-bound presigned-artifact generation. *)

type t =
  | Presign_get_object
  | Presign_put_object
  | Presign_head_object
  | Presign_delete_object
  | Presign_upload_part

val equal : t -> t -> bool
val to_string : t -> string
val all : t list
