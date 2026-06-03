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

(** In-memory S3 simulator for contract tests. *)
module Simulator : sig
  (** Deterministic in-memory S3 implementation for unit and contract-style
      tests. It implements the same object, bucket, multipart, and presigned
      module shape as runtime-backed clients, but performs no network IO. *)
  module Clock : sig
    type t

    val create : ?now:Ptime.t -> unit -> t
    (** Create a controllable simulator clock. *)

    val now : t -> Ptime.t
    val advance : t -> Ptime.Span.t -> unit
    val advance_ms : t -> int -> unit
  end

  type config = { max_list_keys : int }
  (** Simulator limits. [max_list_keys] bounds generated list pages. *)

  val default_config : config

  type store
  (** Shared mutable in-memory bucket/object store. *)

  val create_store : ?config:config -> clock:Clock.t -> unit -> store
  (** Create an empty simulator store. *)

  type t
  (** Simulator connection handle. *)

  val connect : store -> credentials:Awskit.Credentials.t -> t
  (** Connect to a store with credentials used for signing-compatible state. *)

  val store : t -> store

  module Runtime : RUNTIME with type 'a t = 'a and type connection = t

  type fault =
    | Slow_down
    | Internal_error
    | Connection_reset
    | Response_lost
        (** Faults that can be injected to exercise retry and error handling. *)

  val inject_fault : t -> fault -> unit
  (** Inject one fault for the next eligible simulator operation. *)

  val inject_faults : t -> fault list -> unit
  (** Append faults consumed by subsequent eligible operations. *)

  val clear_faults : t -> unit

  val enable_random_faults : t -> seed:int -> prob:float -> unit
  (** Enable deterministic pseudo-random fault injection. *)

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
  (** Operation history entry recorded by the simulator. *)

  type object_metadata = {
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
  }
  (** Inspectable metadata for a stored object. *)

  val object_metadata :
    store -> bucket:string -> key:string -> object_metadata option
  (** Inspect metadata for the current stored object, if present. *)

  val keys : store -> bucket:string -> string list
  (** Inspect current object keys in a bucket. *)

  val history : store -> operation_record list
  (** Return simulator operation history in newest-first order. *)

  val clear_history : store -> unit

  val objects_as_strings : store -> bucket:string -> (string * string) list
  (** Inspect current objects whose bodies can be represented as strings. *)

  include
    S
      with type connection := t
       and type 'a io := 'a
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader
end
