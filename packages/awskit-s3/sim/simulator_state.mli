module Clock : sig
  type t

  val create : ?now:Ptime.t -> unit -> t
  val now : t -> Ptime.t
  val advance : t -> Ptime.Span.t -> unit
  val advance_ms : t -> int -> unit
end

type config = { max_list_keys : int }

val default_config : config

type stored_object = {
  mutable body : string;
  mutable etag : Awskit_s3.Object.Etag.t;
  mutable version_id : Awskit_s3.Object.Version_id.t option;
  mutable content_type : Awskit_s3.Content_type.t option;
  mutable metadata : Awskit_s3.Metadata.t;
  mutable storage_class : Awskit_s3.Storage_class.t option;
  mutable tags : Awskit_s3.Tag.Set.t;
  mutable checksum : Awskit_s3.Object.Checksum.response;
  mutable last_modified : Ptime.t;
}

type stored_delete_marker = {
  version_id : Awskit_s3.Object.Version_id.t;
  last_modified : Ptime.t;
}

type stored_version =
  | Stored_object of stored_object
  | Stored_delete_marker of stored_delete_marker

type stored_part = {
  part_number : int;
  body : string;
  etag : Awskit_s3.Object.Etag.t;
  checksum : Awskit_s3.Object.Checksum.response;
  last_modified : Ptime.t;
}

type multipart_upload = {
  upload : Awskit_s3.Multipart.Upload.created Awskit_s3.Multipart.Upload.t;
  content_type : Awskit_s3.Content_type.t option;
  metadata : Awskit_s3.Metadata.t;
  storage_class : Awskit_s3.Storage_class.t option;
  tags : Awskit_s3.Tag.Set.t;
  checksum_algorithm : Awskit_s3.Object.Checksum.Algorithm.t option;
  checksum_type : Awskit_s3.Object.Checksum.Type.t option;
  parts : (int, stored_part) Hashtbl.t;
  created_at : Ptime.t;
}

type bucket_state = {
  created_at : Ptime.t;
  objects : (string, stored_version) Hashtbl.t;
  versions : (string, stored_version list) Hashtbl.t;
  multipart_uploads : (string, multipart_upload) Hashtbl.t;
  mutable policy : Awskit_s3.Policy.t option;
  mutable bucket_tags : Awskit_s3.Tag.Set.t;
  mutable versioning : Awskit_s3.Bucket.Versioning.Status.t option;
  mutable encryption : Awskit_s3.Bucket.Encryption.config option;
  mutable cors : Awskit_s3.Bucket.Cors.config option;
  mutable public_access_block :
    Awskit_s3.Bucket.Public_access_block.config option;
  mutable ownership_controls : Awskit_s3.Bucket.Ownership_controls.config option;
}

type operation =
  [ `Put_object
  | `Get_object
  | `Head_object
  | `Delete_object
  | `List_objects_v2
  | `List_object_versions
  | `Copy_object
  | `Delete_objects
  | `Create_multipart_upload
  | `Upload_part
  | `Complete_multipart_upload
  | `Abort_multipart_upload
  | `List_parts ]

type operation_record = {
  op : operation;
  bucket : string;
  key : string option;
  timestamp : Ptime.t;
  faulted : bool;
}

type store

val create_store : ?config:config -> clock:Clock.t -> unit -> store
val config : store -> config
val clock : store -> Clock.t
val find_bucket : store -> string -> bucket_state option
val bucket_exists : store -> string -> bool
val add_bucket : store -> string -> bucket_state -> unit
val remove_bucket : store -> string -> unit
val buckets : store -> (string * bucket_state) list
val buckets_seq : store -> (string * bucket_state) Seq.t
val objects : bucket_state -> (string * stored_version) list
val versions : bucket_state -> (string * stored_version list) list
val parts : multipart_upload -> stored_part list
val history : store -> operation_record list
val clear_history : store -> unit

type fault = Slow_down | Internal_error | Connection_reset | Response_lost
type t

val connect : store -> credentials:Awskit.Credentials.t -> t
val store : t -> store
val credentials : t -> Awskit.Credentials.t
val now : t -> Ptime.t

val record_operation :
  ?faulted:bool -> t -> operation -> string -> string option -> unit

val allocate_upload_id : t -> Awskit_s3.Multipart.Upload_id.t
val allocate_version_id : t -> Awskit_s3.Object.Version_id.t
val append_faults : t -> fault list -> unit
val clear_faults : t -> unit
val enable_random_faults : t -> seed:int -> prob:float -> unit
val disable_random_faults : t -> unit
val take_fault : t -> fault option
