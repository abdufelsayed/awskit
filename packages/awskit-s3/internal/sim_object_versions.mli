type version_entry =
  | Object_version of Object.Versions.object_version
  | Delete_marker of Object.Versions.delete_marker

val version_entry_key : version_entry -> string
val version_entry_id : version_entry -> Object.Version_id.t option

val version_entries :
  Sim_state.bucket_state -> Object.Versions.options -> version_entry list
