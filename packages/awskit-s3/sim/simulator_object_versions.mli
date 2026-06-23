open Awskit_s3

type version_entry =
  | Object_version of Object.Versions.object_version
  | Delete_marker of Object.Versions.delete_marker

type listing_entry =
  | Version_entry of version_entry
  | Common_prefix of Object_key.Prefix.t

val version_entry_key : version_entry -> Object_key.t
val version_entry_id : version_entry -> Object.Version_id.t option
val listing_entry_key_marker : listing_entry -> Object_key.t
val listing_entry_id : listing_entry -> Object.Version_id.t option

val version_entries :
  Simulator_state.bucket_state -> Object.Versions.options -> version_entry list

val listing_entries :
  Simulator_state.bucket_state -> Object.Versions.options -> listing_entry list
