(** AWS S3 client API.

    [Awskit_s3] is the main entrypoint for AWS S3 bucket and object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned request artifacts, endpoint policy, and runtime-backed clients. *)

module Runtime_adapter = Runtime_adapter
(** Shared signatures for runtime adapters and direct functor composition.

    Application code normally uses a ready runtime package or the complete
    {!module-type:S} operation surface. *)

module type S = Runtime_adapter.Client
(** Complete configured S3 operation surface for one runtime. *)

module Error : sig
  type t = Awskit.Error.t
  (** Structured Awskit error value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an error. *)

  val pp_sexp : Format.formatter -> t -> unit
  (** Pretty-print the structured S-expression representation. *)

  val equal : t -> t -> bool
  (** Compare two errors structurally. *)

  val to_string_hum : t -> string
  (** Render an error for humans. *)

  val to_sexp_string_hum : t -> string
  (** Render the structured S-expression representation. *)

  val sexp_of_t : t -> Base.Sexp.t
  (** Convert an error to its redacted S-expression representation. *)

  val kind : t -> Awskit.Error.kind
  (** Return the redacted top-level error kind. *)

  val context : t -> Awskit.Error.context list
  (** Return the redacted context stack, newest first. *)

  val retry_class : t -> Awskit.Error.retry_class
  (** Return the coarse caller-handling class. *)

  val is_validation : t -> bool
  (** Return whether the error contains a validation failure. *)

  val is_credentials : t -> bool
  (** Return whether the error contains a credentials failure. *)

  val is_endpoint : t -> bool
  (** Return whether the error contains an endpoint failure. *)

  val is_transport : t -> bool
  (** Return whether the error contains a transport failure. *)

  val is_timeout : t -> bool
  (** Return whether the error contains a timeout. *)

  val is_cancelled : t -> bool
  (** Return whether the error contains cancellation. *)

  val validation_field : t -> string option
  (** Return the invalid field for a validation failure, when available. *)

  val service_code : t -> string option
  (** Return the S3 service error code, when the error came from a modeled
      service response. *)

  val service_status : t -> int option
  (** Return the HTTP status for a service failure, when available. *)

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
  Runtime_adapter.S
    with type runtime_connection = R.connection
     and type 'a io = 'a R.t
     and type Body.t = R.request_body
     and type Reader.t = R.response_body_reader
