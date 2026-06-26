(** Internal store helpers for simulator state. *)

val bucket_state :
  Simulator_state.store -> string -> Simulator_state.bucket_state option
(** Look up a bucket without recording an operation. *)

val require_bucket :
  Simulator_state.t ->
  string ->
  (Simulator_state.bucket_state, Awskit.Error.t) result
(** Look up a bucket or return an S3 [NoSuchBucket] service error. *)

val require_object :
  Simulator_state.t ->
  string ->
  string ->
  (Simulator_state.stored_object, Awskit.Error.t) result
(** Require the current object or return an S3 not-found service error. *)

val versioning_enabled : Simulator_state.bucket_state -> bool
val versioning_suspended : Simulator_state.bucket_state -> bool
val versioning_keeps_history : Simulator_state.bucket_state -> bool

val set_versioning :
  Simulator_state.bucket_state -> Awskit_s3.Bucket.Versioning.Status.t -> unit

val store_object :
  Simulator_state.t ->
  Simulator_state.bucket_state ->
  string ->
  Simulator_state.stored_object ->
  Simulator_state.stored_object
(** Store an object as the current version, preserving history when bucket
    versioning requires it. *)

val find_version :
  Simulator_state.bucket_state ->
  string ->
  Awskit_s3.Object.Version_id.t ->
  Simulator_state.stored_version option

val current_or_version :
  Simulator_state.bucket_state ->
  string ->
  Awskit_s3.Object.Version_id.t option ->
  Simulator_state.stored_version option

val current_object :
  Simulator_state.bucket_state -> string -> Simulator_state.stored_object option

val require_object_version :
  Simulator_state.t ->
  string ->
  string ->
  Awskit_s3.Object.Version_id.t option ->
  (Simulator_state.stored_object, Awskit.Error.t) result

val delete_version :
  Simulator_state.bucket_state ->
  string ->
  Awskit_s3.Object.Version_id.t ->
  Simulator_state.stored_version option

val store_delete_marker :
  Simulator_state.t ->
  Simulator_state.bucket_state ->
  string ->
  Simulator_state.stored_delete_marker

val version_headers :
  Awskit_s3.Object.Version_id.t option -> (string * string) list

val copy_source_version_headers :
  Awskit_s3.Object.Version_id.t option -> (string * string) list

val delete_marker_headers : bool option -> (string * string) list

val delete_marker_error :
  current:bool -> Simulator_state.stored_delete_marker -> Awskit.Error.t

val object_size : Simulator_state.stored_object -> int64
(** Return the stored object body length in bytes. *)

val ensure_write_preconditions :
  Simulator_state.stored_object option ->
  Awskit_s3.Object.Preconditions.Write.t ->
  (unit, Awskit.Error.t) result

val ensure_read_preconditions :
  Simulator_state.stored_object ->
  Awskit_s3.Object.Preconditions.Read.t ->
  (unit, Awskit.Error.t) result

val ensure_delete_preconditions :
  Simulator_state.stored_object ->
  Awskit_s3.Object.Preconditions.Delete.t ->
  (unit, Awskit.Error.t) result

val delete_preconditions_are_empty :
  Awskit_s3.Object.Preconditions.Delete.t -> bool

val ensure_copy_source_preconditions :
  Simulator_state.stored_object ->
  Awskit_s3.Object.Preconditions.Copy_source.t ->
  (unit, Awskit.Error.t) result

val upload_key : Awskit_s3.Multipart.Upload_id.t -> string
(** Return the simulator table key for a multipart upload id. *)

val require_multipart_upload :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  upload_id:Awskit_s3.Multipart.Upload_id.t ->
  ( Simulator_state.bucket_state * Simulator_state.multipart_upload,
    Awskit.Error.t )
  result

val next_upload_id : Simulator_state.t -> Awskit_s3.Multipart.Upload_id.t
(** Allocate the next deterministic multipart upload id. *)
