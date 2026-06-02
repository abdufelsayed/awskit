val bucket_state : Sim_state.store -> string -> Sim_state.bucket_state option

val require_bucket :
  Sim_state.t -> string -> (Sim_state.bucket_state, Awskit.Error.t) result

val require_object :
  Sim_state.t ->
  string ->
  string ->
  (Sim_state.stored_object, Awskit.Error.t) result

val versioning_enabled : Sim_state.bucket_state -> bool
val versioning_suspended : Sim_state.bucket_state -> bool
val versioning_keeps_history : Sim_state.bucket_state -> bool

val store_object :
  Sim_state.t ->
  Sim_state.bucket_state ->
  string ->
  Sim_state.stored_object ->
  Sim_state.stored_object

val find_version :
  Sim_state.bucket_state ->
  string ->
  Object.Version_id.t ->
  Sim_state.stored_version option

val current_or_version :
  Sim_state.bucket_state ->
  string ->
  Object.Version_id.t option ->
  Sim_state.stored_version option

val current_object :
  Sim_state.bucket_state -> string -> Sim_state.stored_object option

val require_object_version :
  Sim_state.t ->
  string ->
  string ->
  Object.Version_id.t option ->
  (Sim_state.stored_object, Awskit.Error.t) result

val delete_version :
  Sim_state.bucket_state ->
  string ->
  Object.Version_id.t ->
  Sim_state.stored_version option

val store_delete_marker :
  Sim_state.t ->
  Sim_state.bucket_state ->
  string ->
  Sim_state.stored_delete_marker

val version_headers : Object.Version_id.t option -> (string * string) list

val copy_source_version_headers :
  Object.Version_id.t option -> (string * string) list

val delete_marker_headers : bool option -> (string * string) list

val delete_marker_error :
  current:bool -> Sim_state.stored_delete_marker -> Awskit.Error.t

val object_size : Sim_state.stored_object -> int64

val ensure_write_preconditions :
  Sim_state.stored_object option ->
  Object.Preconditions.Write.t ->
  (unit, Awskit.Error.t) result

val ensure_read_preconditions :
  Sim_state.stored_object ->
  Object.Preconditions.Read.t ->
  (unit, Awskit.Error.t) result

val ensure_delete_preconditions :
  Sim_state.stored_object ->
  Object.Preconditions.Delete.t ->
  (unit, Awskit.Error.t) result

val delete_preconditions_are_empty : Object.Preconditions.Delete.t -> bool

val ensure_copy_source_preconditions :
  Sim_state.stored_object ->
  Object.Preconditions.Copy_source.t ->
  (unit, Awskit.Error.t) result

val upload_key : Multipart.Upload_id.t -> string

val require_multipart_upload :
  Sim_state.t ->
  bucket:string ->
  key:string ->
  upload_id:Multipart.Upload_id.t ->
  (Sim_state.bucket_state * Sim_state.multipart_upload, Awskit.Error.t) result

val next_upload_id : Sim_state.t -> Multipart.Upload_id.t
