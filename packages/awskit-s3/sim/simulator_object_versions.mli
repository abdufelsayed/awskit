type version_entry =
  | Object_version of Awskit_s3.Object.Versions.object_version
  | Delete_marker of Awskit_s3.Object.Versions.delete_marker

type listing_entry =
  | Version_entry of version_entry
  | Common_prefix of Awskit_s3.Object_key.Prefix.t

val version_entry_key : version_entry -> Awskit_s3.Object_key.t
val version_entry_id : version_entry -> Awskit_s3.Object.Version_id.t option
val listing_entry_key_marker : listing_entry -> Awskit_s3.Object_key.t
val listing_entry_id : listing_entry -> Awskit_s3.Object.Version_id.t option

val version_entries :
  Simulator_state.bucket_state ->
  Awskit_s3.Object.Versions.options ->
  version_entry list

val listing_entries :
  Simulator_state.bucket_state ->
  Awskit_s3.Object.Versions.options ->
  listing_entry list
