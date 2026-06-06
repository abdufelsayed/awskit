val bucket_state :
  Simulator_state.store -> string -> Simulator_state.bucket_state option

val require_bucket :
  Simulator_state.t ->
  string ->
  (Simulator_state.bucket_state, Awskit.Error.t) result

val require_object :
  Simulator_state.t ->
  string ->
  string ->
  (Simulator_state.stored_object, Awskit.Error.t) result

val versioning_enabled : Simulator_state.bucket_state -> bool
val versioning_suspended : Simulator_state.bucket_state -> bool
val versioning_keeps_history : Simulator_state.bucket_state -> bool

val store_object :
  Simulator_state.t ->
  Simulator_state.bucket_state ->
  string ->
  Simulator_state.stored_object ->
  Simulator_state.stored_object

val find_version :
  Simulator_state.bucket_state ->
  string ->
  Object.Version_id.t ->
  Simulator_state.stored_version option

val current_or_version :
  Simulator_state.bucket_state ->
  string ->
  Object.Version_id.t option ->
  Simulator_state.stored_version option

val current_object :
  Simulator_state.bucket_state -> string -> Simulator_state.stored_object option

val require_object_version :
  Simulator_state.t ->
  string ->
  string ->
  Object.Version_id.t option ->
  (Simulator_state.stored_object, Awskit.Error.t) result

val delete_version :
  Simulator_state.bucket_state ->
  string ->
  Object.Version_id.t ->
  Simulator_state.stored_version option

val store_delete_marker :
  Simulator_state.t ->
  Simulator_state.bucket_state ->
  string ->
  Simulator_state.stored_delete_marker

val version_headers : Object.Version_id.t option -> (string * string) list

val copy_source_version_headers :
  Object.Version_id.t option -> (string * string) list

val delete_marker_headers : bool option -> (string * string) list

val delete_marker_error :
  current:bool -> Simulator_state.stored_delete_marker -> Awskit.Error.t

val object_size : Simulator_state.stored_object -> int64

val ensure_write_preconditions :
  Simulator_state.stored_object option ->
  Object.Preconditions.Write.t ->
  (unit, Awskit.Error.t) result

val ensure_read_preconditions :
  Simulator_state.stored_object ->
  Object.Preconditions.Read.t ->
  (unit, Awskit.Error.t) result

val ensure_delete_preconditions :
  Simulator_state.stored_object ->
  Object.Preconditions.Delete.t ->
  (unit, Awskit.Error.t) result

val delete_preconditions_are_empty : Object.Preconditions.Delete.t -> bool

val ensure_copy_source_preconditions :
  Simulator_state.stored_object ->
  Object.Preconditions.Copy_source.t ->
  (unit, Awskit.Error.t) result

val upload_key : Multipart.Upload_id.t -> string

val require_multipart_upload :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  upload_id:Multipart.Upload_id.t ->
  ( Simulator_state.bucket_state * Simulator_state.multipart_upload,
    Awskit.Error.t )
  result

val next_upload_id : Simulator_state.t -> Multipart.Upload_id.t
