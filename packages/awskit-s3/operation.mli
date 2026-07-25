(** Bounded S3 service-operation identity.

    This is domain vocabulary, independent of the observability package. *)

type t =
  | Abort_multipart_upload
  | Complete_multipart_upload
  | Copy_object
  | Create_bucket
  | Create_multipart_upload
  | Delete_bucket
  | Delete_bucket_cors
  | Delete_bucket_encryption
  | Delete_bucket_ownership_controls
  | Delete_bucket_policy
  | Delete_bucket_tagging
  | Delete_object
  | Delete_object_tagging
  | Delete_objects
  | Delete_public_access_block
  | Get_bucket_cors
  | Get_bucket_encryption
  | Get_bucket_location
  | Get_bucket_ownership_controls
  | Get_bucket_policy
  | Get_bucket_tagging
  | Get_bucket_versioning
  | Get_object
  | Get_object_tagging
  | Get_public_access_block
  | Head_bucket
  | Head_object
  | List_buckets
  | List_object_versions
  | List_objects_v2
  | List_parts
  | Put_bucket_cors
  | Put_bucket_encryption
  | Put_bucket_ownership_controls
  | Put_bucket_policy
  | Put_bucket_tagging
  | Put_bucket_versioning
  | Put_object
  | Put_object_tagging
  | Put_public_access_block
  | Upload_part

val equal : t -> t -> bool
val to_string : t -> string
val all : t list
