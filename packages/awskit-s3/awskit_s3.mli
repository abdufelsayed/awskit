(** AWS S3 client API.

    [Awskit_s3] is the main entrypoint for AWS S3 bucket and object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned request artifacts, endpoint resolution, and runtime-backed
    clients. *)

module Implementor = Implementor
(** Shared signatures for runtime adapters and direct functor composition.

    Application code normally uses a ready runtime package or the complete
    {!module-type:S} operation surface. *)

module type S = Implementor.Client
(** Complete configured S3 operation surface for one runtime. *)

module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region

module Error : sig
  type t = Awskit.Error.t
  (** Structured Awskit error value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an error. *)

  val equal : t -> t -> bool
  (** Compare two errors structurally. *)

  val to_string_hum : t -> string
  (** Render an error for humans. *)

  val service_code : t -> string option
  (** Return the S3 service error code, when the error came from a modeled
      service response. *)

  val is_not_found : t -> bool
  (** Return [true] for S3 not-found errors recognized by lookup helpers. *)

  val is_no_such_bucket : t -> bool
  (** Return [true] for [NoSuchBucket] service errors. *)

  val is_no_such_key : t -> bool
  (** Return [true] for [NoSuchKey] service errors. *)

  val is_precondition_failed : t -> bool
  (** Return [true] for failed conditional request preconditions. *)

  val is_conditional_request_conflict : t -> bool
  (** Return [true] for S3 conditional-write conflicts. *)

  val is_conditional_failure : t -> bool
  (** Return [true] for any conditional request failure recognized by S3. *)
end

module Bucket_name = Bucket_name
module Object_key = Object_key
module Account_id = Account_id
module Content_type = Content_type
module Header_value = Header_value
module Metadata = Metadata
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Encryption = Encryption
module Endpoint_config = Endpoint_config
module Object = Object
module Bucket = Bucket
module Multipart = Multipart
module Transfer = Transfer
module Policy = Policy
module Presigned = Presigned

module Make (R : Awskit.Runtime.S) :
  Implementor.Runtime_client
    with type runtime_connection = R.connection
     and type 'a io = 'a R.t
     and type request_body = R.request_body
     and type response_body_reader = R.response_body_reader
