(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for classic AWS S3 bucket/object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned URLs, runtime-backed clients, and the in-memory simulator used by
    tests.

    Implementation modules, XML codecs, request builders, and parser helpers are
    private to the package. *)

module Credentials = Awskit.Credentials
(** AWS credentials for request signing, re-exported for convenience. *)

module Endpoint = Awskit.Endpoint
(** Endpoint values used by the AWS endpoint resolver. *)

module Region = Awskit.Region
(** AWS region values. *)

(** S3 classifiers over structured {!Awskit.Error.t} values. *)
module Error : sig
  type t = Awskit.Error.t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val to_string_hum : t -> string
  val service_code : t -> string option
  val is_not_found : t -> bool
  val is_no_such_bucket : t -> bool
  val is_no_such_key : t -> bool
  val is_precondition_failed : t -> bool
end

(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)
module Metadata : sig
  type t = Awskit_s3_common.Metadata.t
end

(** Classic S3 object storage classes. *)
module Storage_class : sig
  type t = Awskit_s3_common.Storage_class.t =
    | Standard
    | Standard_ia
    | Onezone_ia
    | Intelligent_tiering
    | Glacier
    | Glacier_ir
    | Deep_archive
    | Express_onezone

  val to_string : t -> string
  val of_string : string -> t option
end

(** S3 tag key/value pair. *)
module Tag : sig
  type t = Awskit_s3_common.Tag.t = { key : string; value : string }
end

(** HTTP byte-range requests for S3 object reads. *)
module Range : sig
  type t = Awskit_s3_common.Range.t

  val bytes : start:int64 -> finish:int64 -> (t, Error.t) result
  val bytes_exn : start:int64 -> finish:int64 -> t
  val from : int64 -> (t, Error.t) result
  val from_exn : int64 -> t
  val suffix : int64 -> (t, Error.t) result
  val suffix_exn : int64 -> t
  val to_header : t -> string
end

(** AWS S3 endpoint and addressing resolution. *)
module Provider :
  Awskit_s3_intf.PROVIDER
    with type addressing_style = Awskit_s3_provider.addressing_style
     and type endpoint_variant = Awskit_s3_provider.endpoint_variant
     and type resolved_style = Awskit_s3_provider.resolved_style
     and type Request.t = Awskit_s3_provider.Request.t
     and type t = Awskit_s3_provider.t

(** Object data types and option records. *)
module Object :
  Awskit_s3_intf.OBJECT_DATA
    with type Etag.t = Awskit_s3_object.Etag.t
     and type Version_id.t = Awskit_s3_object.Version_id.t
     and type Checksum.algorithm = Awskit_s3_object.Checksum.algorithm
     and type Checksum.request = Awskit_s3_object.Checksum.request
     and type Checksum.response = Awskit_s3_object.Checksum.response
     and type Encryption.kms = Awskit_s3_object.Encryption.kms
     and type Encryption.request = Awskit_s3_object.Encryption.request
     and type Encryption.response = Awskit_s3_object.Encryption.response
     and type Etag_condition.t = Awskit_s3_object.Etag_condition.t
     and type Preconditions.Write.t = Awskit_s3_object.Preconditions.Write.t
     and type Preconditions.Read.t = Awskit_s3_object.Preconditions.Read.t
     and type Preconditions.Delete.t = Awskit_s3_object.Preconditions.Delete.t
     and type Preconditions.Copy_source.t =
      Awskit_s3_object.Preconditions.Copy_source.t
     and type Put.options = Awskit_s3_object.Put.options
     and type Put.result = Awskit_s3_object.Put.result
     and type Get.options = Awskit_s3_object.Get.options
     and type Get.info = Awskit_s3_object.Get.info
     and type Head.options = Awskit_s3_object.Head.options
     and type Head.info = Awskit_s3_object.Head.info
     and type Delete.options = Awskit_s3_object.Delete.options
     and type Delete.result = Awskit_s3_object.Delete.result
     and type Delete_many.object_ = Awskit_s3_object.Delete_many.object_
     and type Delete_many.deleted = Awskit_s3_object.Delete_many.deleted
     and type Delete_many.item_error = Awskit_s3_object.Delete_many.item_error
     and type Delete_many.result = Awskit_s3_object.Delete_many.result
     and type Copy.metadata_directive = Awskit_s3_object.Copy.metadata_directive
     and type Copy.options = Awskit_s3_object.Copy.options
     and type Copy.result = Awskit_s3_object.Copy.result
     and type List.options = Awskit_s3_object.List.options
     and type List.object_summary = Awskit_s3_object.List.object_summary
     and type List.page = Awskit_s3_object.List.page
     and type Tagging.result = Awskit_s3_object.Tagging.result

(** Bucket data types and configuration records. *)
module Bucket :
  Awskit_s3_intf.BUCKET_DATA
    with type info = Awskit_s3_bucket.info
     and type Create.options = Awskit_s3_bucket.Create.options
     and type Create.result = Awskit_s3_bucket.Create.result
     and type Delete.result = Awskit_s3_bucket.Delete.result
     and type Head.info = Awskit_s3_bucket.Head.info
     and type Versioning.Status.t = Awskit_s3_bucket.Versioning.Status.t
     and type Versioning.result = Awskit_s3_bucket.Versioning.result
     and type Tagging.result = Awskit_s3_bucket.Tagging.result
     and type Encryption.Algorithm.t = Awskit_s3_bucket.Encryption.Algorithm.t
     and type Encryption.Rule.t = Awskit_s3_bucket.Encryption.Rule.t
     and type Encryption.config = Awskit_s3_bucket.Encryption.config
     and type Encryption.result = Awskit_s3_bucket.Encryption.result
     and type Cors.Method.t = Awskit_s3_bucket.Cors.Method.t
     and type Cors.rule = Awskit_s3_bucket.Cors.rule
     and type Cors.config = Awskit_s3_bucket.Cors.config
     and type Cors.result = Awskit_s3_bucket.Cors.result
     and type Website.config = Awskit_s3_bucket.Website.config
     and type Website.result = Awskit_s3_bucket.Website.result
     and type Public_access_block.config =
      Awskit_s3_bucket.Public_access_block.config
     and type Public_access_block.result =
      Awskit_s3_bucket.Public_access_block.result
     and type Ownership_controls.Object_ownership.t =
      Awskit_s3_bucket.Ownership_controls.Object_ownership.t
     and type Ownership_controls.config =
      Awskit_s3_bucket.Ownership_controls.config
     and type Ownership_controls.result =
      Awskit_s3_bucket.Ownership_controls.result
     and type Request_payment.Payer.t = Awskit_s3_bucket.Request_payment.Payer.t
     and type Request_payment.result = Awskit_s3_bucket.Request_payment.result
     and type Accelerate.Status.t = Awskit_s3_bucket.Accelerate.Status.t
     and type Accelerate.result = Awskit_s3_bucket.Accelerate.result
     and type Policy_status.result = Awskit_s3_bucket.Policy_status.result
     and type Logging.target = Awskit_s3_bucket.Logging.target
     and type Logging.config = Awskit_s3_bucket.Logging.config
     and type Logging.result = Awskit_s3_bucket.Logging.result

(** Multipart upload data types. *)
module Multipart :
  Awskit_s3_intf.MULTIPART_DATA
    with type Upload_id.t = Awskit_s3_multipart.Upload_id.t
     and type Upload.t = Awskit_s3_multipart.Upload.t
     and type Part.t = Awskit_s3_multipart.Part.t
     and type Create.options = Awskit_s3_multipart.Create.options
     and type Create.result = Awskit_s3_multipart.Create.result
     and type Upload_part.options = Awskit_s3_multipart.Upload_part.options
     and type Upload_part.result = Awskit_s3_multipart.Upload_part.result
     and type Complete.result = Awskit_s3_multipart.Complete.result
     and type List_parts.options = Awskit_s3_multipart.List_parts.options
     and type List_parts.part_info = Awskit_s3_multipart.List_parts.part_info
     and type List_parts.page = Awskit_s3_multipart.List_parts.page

module Policy : Awskit_s3_intf.POLICY with type t = Awskit_s3_policy.t
(** Opaque validated bucket-policy JSON payloads. *)

(** Standalone S3 presigned URL generation. *)
module Presigned :
  Awskit_s3_intf.PRESIGNED_DATA
    with type method_ = Awskit_s3_presigned.method_
     and type result = Awskit_s3_presigned.result
     and type Put_object.options = Awskit_s3_presigned.Put_object.options
     and type Get_object.options = Awskit_s3_presigned.Get_object.options

module type RUNTIME = Awskit_s3_intf.RUNTIME
(** Runtime contract required by {!Make}. *)

module type OBJECT = Awskit_s3_intf.OBJECT
(** Runtime-backed object operation surface. *)

module type BUCKET = Awskit_s3_intf.BUCKET
(** Runtime-backed bucket operation surface. *)

module type MULTIPART = Awskit_s3_intf.MULTIPART
(** Runtime-backed multipart operation surface. *)

module type PRESIGNED = Awskit_s3_intf.PRESIGNED
(** Runtime-backed presigned URL operation surface. *)

module type S = Awskit_s3_intf.S
(** Complete runtime-backed S3 client surface. *)

(** Build an S3 client from a runtime implementation. *)
module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type upload_body = R.upload_body
     and type download_reader = R.download_reader

(** In-memory S3 simulator for contract tests. *)
module Sim : sig
  module Clock : sig
    type t

    val create : ?now:Ptime.t -> unit -> t
    val now : t -> Ptime.t
    val advance : t -> Ptime.Span.t -> unit
    val advance_ms : t -> int -> unit
  end

  type config = { max_list_keys : int }

  val default_config : config

  type store

  val create_store : ?config:config -> clock:Clock.t -> unit -> store

  type t

  val connect : store -> credentials:Awskit.Credentials.t -> t
  val store : t -> store

  module Runtime : RUNTIME with type 'a t = 'a and type connection = t

  type fault = Slow_down | Internal_error | Connection_reset | Response_lost

  val inject_fault : t -> fault -> unit
  val inject_faults : t -> fault list -> unit
  val clear_faults : t -> unit
  val enable_buggify : t -> seed:int -> prob:float -> unit
  val disable_buggify : t -> unit

  type op_record = {
    op : [ `Put | `Get | `Head | `Delete | `List | `Copy | `Delete_many ];
    bucket : string;
    key : string option;
    timestamp : Ptime.t;
    faulted : bool;
  }

  type object_meta = {
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
  }

  val object_meta : store -> bucket:string -> key:string -> object_meta option
  val keys : store -> bucket:string -> string list
  val history : store -> op_record list
  val clear_history : store -> unit
  val dump_strings : store -> bucket:string -> (string * string) list

  include
    S
      with type connection := t
       and type 'a io := 'a
       and type upload_body := Runtime.upload_body
       and type download_reader := Runtime.download_reader
end
