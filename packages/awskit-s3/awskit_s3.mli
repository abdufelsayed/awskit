(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for AWS S3 bucket/object storage: object
    operations, bucket operations and configuration, multipart upload, presigned
    URLs, and runtime-backed clients.

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
  (** Return the AWS service error code for [Service] errors, when present. *)

  val is_not_found : t -> bool
  (** True for generic not-found service errors. *)

  val is_no_such_bucket : t -> bool
  (** True for S3 [NoSuchBucket] errors. *)

  val is_no_such_key : t -> bool
  (** True for S3 [NoSuchKey] errors. *)

  val is_precondition_failed : t -> bool
  (** True for S3 conditional failures reported as HTTP 412 or
      [PreconditionFailed]. *)

  val is_conditional_request_conflict : t -> bool
  (** True for S3 [ConditionalRequestConflict] errors. *)

  val is_conditional_failure : t -> bool
  (** True for precondition failures or conditional request conflicts. *)
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
(** Alias for [Awskit_s3.Object.Put]. *)

module Get_object = Object.Get
(** Alias for [Awskit_s3.Object.Get]. *)

module Head_object = Object.Head
(** Alias for [Awskit_s3.Object.Head]. *)

module Delete_object = Object.Delete
(** Alias for [Awskit_s3.Object.Delete]. *)

module Delete_objects = Object.Delete_many
(** Alias for [Awskit_s3.Object.Delete_many]. *)

module Copy_object = Object.Copy
(** Alias for [Awskit_s3.Object.Copy]. *)

module List_objects_v2 = Object.List
(** Alias for [Awskit_s3.Object.List]. *)

module List_object_versions = Object.Versions
(** Alias for [Awskit_s3.Object.Versions]. *)

module Create_bucket = Bucket.Create
(** Alias for [Awskit_s3.Bucket.Create]. *)

module Delete_bucket = Bucket.Delete
(** Alias for [Awskit_s3.Bucket.Delete]. *)

module Head_bucket = Bucket.Head
(** Alias for [Awskit_s3.Bucket.Head]. *)

module List_buckets = Bucket.List_buckets
(** Alias for [Awskit_s3.Bucket.List_buckets]. *)

module Get_bucket_location = Bucket.Get_location
(** Alias for [Awskit_s3.Bucket.Get_location]. *)

module Create_multipart_upload = Multipart.Create
(** Alias for [Awskit_s3.Multipart.Create]. *)

module Upload_part = Multipart.Upload_part
(** Alias for [Awskit_s3.Multipart.Upload_part]. *)

module Complete_multipart_upload = Multipart.Complete
(** Alias for [Awskit_s3.Multipart.Complete]. *)

module Abort_multipart_upload = Multipart.Abort
(** Alias for [Awskit_s3.Multipart.Abort]. *)

module List_parts = Multipart.List_parts
(** Alias for [Awskit_s3.Multipart.List_parts]. *)

module type OBJECT_DATA = module type of Object
(** Public object data module shape. *)

module type BUCKET_DATA = module type of Bucket
(** Public bucket data module shape. *)

module type MULTIPART_DATA = module type of Multipart
(** Public multipart data module shape. *)

module type POLICY = module type of Policy
(** Public bucket policy module shape. *)

module type PRESIGNED_DATA = module type of Presigned
(** Public standalone presigning data module shape. *)

module type RUNTIME = Awskit_s3_internal.Core.RUNTIME
(** Runtime implementation required by {!module:Make}. *)

module type OBJECT = Awskit_s3_internal.Core.OBJECT
(** Runtime-backed object operation module shape. *)

module type BUCKET = Awskit_s3_internal.Core.BUCKET
(** Runtime-backed bucket operation module shape. *)

module type MULTIPART = Awskit_s3_internal.Core.MULTIPART
(** Runtime-backed multipart operation module shape. *)

module type PRESIGNED = Awskit_s3_internal.Core.PRESIGNED
(** Runtime-backed presigned URL helper module shape. *)

module type S = Awskit_s3_internal.Core.S
(** Complete runtime-backed S3 client module shape. *)

(** Build an S3 client from a runtime implementation. *)
module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type request_body = R.request_body
     and type response_body_reader = R.response_body_reader
