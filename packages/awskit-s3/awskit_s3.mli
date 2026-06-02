(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for AWS S3 bucket/object storage: object
    operations, bucket operations and configuration, multipart upload, presigned
    URLs, runtime-backed clients, and the in-memory simulator used by tests.

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
  val is_conditional_request_conflict : t -> bool
  val is_conditional_failure : t -> bool
end

module Metadata = Awskit_s3_internal.Metadata
(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)

module Storage_class = Awskit_s3_internal.Storage_class
(** S3 object storage classes. *)

module Tag = Awskit_s3_internal.Tag
(** S3 tag key/value pair. *)

module Range = Awskit_s3_internal.Range
(** HTTP byte-range requests for S3 object reads. *)

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
(** S3 bucket addressing style. [`Auto] uses virtual-hosted addressing when the
    bucket name is safe for the selected endpoint and path-style otherwise. *)

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]
(** AWS S3 endpoint variant. Ignored when an explicit [endpoint] is supplied. *)

module Endpoint_resolver = Awskit_s3_internal.Endpoint_resolver
(** Endpoint and addressing resolution for custom runtimes. *)

type endpoint_config = Endpoint_resolver.t
(** Opaque S3 endpoint and addressing configuration for custom runtimes. Most
    callers pass endpoint options directly to an adapter [create] function. *)

val endpoint_config :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Endpoint.Scheme.t ->
  ?endpoint:Endpoint.t ->
  unit ->
  endpoint_config
(** Build endpoint configuration for custom runtimes. *)

val default_endpoint_config : endpoint_config
(** Default AWS regional HTTPS endpoint configuration. *)

module Object = Awskit_s3_internal.Object
(** Object data types, operation options, and operation results. *)

module Bucket = Awskit_s3_internal.Bucket
(** Bucket data types, operation options, and operation results. *)

module Multipart = Awskit_s3_internal.Multipart
(** Multipart upload data types, operation options, and operation results. *)

module Transfer = Awskit_s3_internal.Transfer
(** High-level S3 transfer configuration shared by transfer helpers. *)

module Policy = Awskit_s3_internal.Policy
(** Opaque validated bucket-policy JSON payloads. *)

module Presigned = Awskit_s3_internal.Presigned
(** Standalone S3 presigned URL generation. *)

module Put_object = Object.Put
module Get_object = Object.Get
module Head_object = Object.Head
module Delete_object = Object.Delete
module Delete_objects = Object.Delete_many
module Copy_object = Object.Copy
module List_objects_v2 = Object.List
module List_object_versions = Object.Versions
module Create_bucket = Bucket.Create
module Delete_bucket = Bucket.Delete
module Head_bucket = Bucket.Head
module List_buckets = Bucket.List_buckets
module Get_bucket_location = Bucket.Get_location
module Create_multipart_upload = Multipart.Create
module Upload_part = Multipart.Upload_part
module Complete_multipart_upload = Multipart.Complete
module Abort_multipart_upload = Multipart.Abort
module List_parts = Multipart.List_parts

module type OBJECT_DATA = module type of Object
module type BUCKET_DATA = module type of Bucket
module type MULTIPART_DATA = module type of Multipart
module type POLICY = module type of Policy
module type PRESIGNED_DATA = module type of Presigned
module type RUNTIME = Awskit_s3_internal.Core.RUNTIME
module type OBJECT = Awskit_s3_internal.Core.OBJECT
module type BUCKET = Awskit_s3_internal.Core.BUCKET
module type MULTIPART = Awskit_s3_internal.Core.MULTIPART
module type PRESIGNED = Awskit_s3_internal.Core.PRESIGNED
module type S = Awskit_s3_internal.Core.S

(** Build an S3 client from a runtime implementation. *)
module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type request_body = R.request_body
     and type response_body_reader = R.response_body_reader

(** In-memory S3 simulator for contract tests. *)
module Simulator : sig
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
  val enable_random_faults : t -> seed:int -> prob:float -> unit
  val disable_random_faults : t -> unit

  type operation_record = {
    op :
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
      | `List_parts ];
    bucket : string;
    key : string option;
    timestamp : Ptime.t;
    faulted : bool;
  }

  type object_metadata = {
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
  }

  val object_metadata :
    store -> bucket:string -> key:string -> object_metadata option

  val keys : store -> bucket:string -> string list
  val history : store -> operation_record list
  val clear_history : store -> unit
  val objects_as_strings : store -> bucket:string -> (string * string) list

  include
    S
      with type connection := t
       and type 'a io := 'a
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader
end
