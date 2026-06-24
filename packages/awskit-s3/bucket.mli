(** S3 bucket data types, bucket-operation options, and bucket-operation
    results. *)

type info = { name : Bucket_name.t; creation_date : Ptime.t option }
(** Bucket summary returned by list operations. [creation_date] may be absent
    when the service response omits it. *)

module Create : sig
  (** [CreateBucket] options and result metadata. *)
  type options = { region : Awskit.Region.t option }
  (** Optional location constraint. Some regions, notably [us-east-1], have
      special AWS behavior and may omit an explicit constraint. *)

  type result = { response : Awskit.Response.t }
  (** [CreateBucket] result metadata. *)

  val default_options : options
  (** Default [CreateBucket] options: no explicit location constraint. *)

  val options :
    ?region:Awskit.Region.t -> unit -> (options, Awskit.Error.t) Stdlib.result
  (** Build [CreateBucket] options. *)

  val options_exn : ?region:Awskit.Region.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Delete : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner], used to guard against bucket-owner
            confusion. *)
  }
  (** [DeleteBucket] request options. *)

  type result = { response : Awskit.Response.t }
  (** [DeleteBucket] result metadata. *)

  val default_options : options
  (** Default [DeleteBucket] options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [DeleteBucket] options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Head : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [HeadBucket] request options. *)

  type result = {
    name : Bucket_name.t;  (** Bucket name that was checked. *)
    region : Awskit.Region.t option;
        (** Region hint from [x-amz-bucket-region], when present. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [HeadBucket] result metadata. *)

  type info = result

  val default_options : options
  (** Default [HeadBucket] options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [HeadBucket] options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module List_buckets : sig
  type result = { buckets : info list; response : Awskit.Response.t }
  (** [ListBuckets] result metadata. *)
end

module Get_location : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [GetBucketLocation] request options. *)

  type result = {
    region : Awskit.Region.t option;
        (** Bucket region, or [None] for S3's default-location encoding. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [GetBucketLocation] result metadata. [None] represents S3 responses that
      encode the default region without a concrete location constraint. *)

  val default_options : options
  (** Default [GetBucketLocation] options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [GetBucketLocation] options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Policy : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket policy request options. *)

  val default_options : options
  (** Default bucket policy options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket policy request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Versioning : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket versioning request options. *)

  module Status : sig
    (** Bucket versioning state. Absence in a result means versioning was never
        configured for the bucket. Unknown values are preserved on reads for
        forward compatibility, but rejected by write operations. *)
    type t = Enabled | Suspended | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  type result = { status : Status.t option; response : Awskit.Response.t }
  (** [GetBucketVersioning] result metadata. [None] means S3 did not return a
      versioning status. *)

  val default_options : options
  (** Default bucket versioning options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket versioning request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Tagging : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket tagging request options. *)

  type result = { tags : Tag.Set.t; response : Awskit.Response.t }
  (** Bucket tagging result metadata. *)

  val default_options : options
  (** Default bucket tagging options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket tagging request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Encryption : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket encryption request options. *)

  (** Bucket default encryption configuration. *)
  module Algorithm : sig
    (** Server-side encryption algorithms returned by S3. Unknown values are
        preserved for forward compatibility. *)
    type t = Aes256 | Aws_kms | Aws_kms_dsse | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  module Blocked_encryption_type : sig
    (** S3 bucket-key blocked encryption type values. Unknown values are
        preserved. *)
    type t = Sse_c | No_block | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  module Rule : sig
    type t = {
      sse_algorithm : Algorithm.t option;
          (** Default server-side encryption algorithm. *)
      kms_master_key_id : string option;
          (** KMS key id/ARN when AWS KMS encryption is configured. *)
      bucket_key_enabled : bool option;
          (** Whether S3 Bucket Keys are enabled for KMS. *)
      blocked_encryption_types : Blocked_encryption_type.t list;
          (** Encryption types blocked by the bucket configuration, if S3
              returns them. *)
    }
    (** One default-encryption rule. *)
  end

  type config = { rules : Rule.t list }
  (** Bucket encryption configuration. *)

  type result = { config : config; response : Awskit.Response.t }
  (** [GetBucketEncryption] result metadata. *)

  val default_options : options
  (** Default bucket encryption options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket encryption request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Cors : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket CORS request options. *)

  (** Bucket CORS configuration. *)
  module Method : sig
    (** HTTP methods accepted in CORS rules. *)
    type t = Get | Put | Post | Delete | Head

    val to_string : t -> string
    val of_string : string -> t option
  end

  type rule = {
    id : string option;  (** Optional S3 rule id. *)
    allowed_origins : string list;  (** Allowed CORS origins. *)
    allowed_methods : Method.t list;  (** Allowed HTTP methods. *)
    allowed_headers : string list;  (** Request headers allowed by browsers. *)
    expose_headers : string list;
        (** Response headers browsers may expose to callers. *)
    max_age_seconds : int option;  (** Browser preflight cache lifetime. *)
  }
  (** One CORS rule. Lists are emitted in the order supplied by the caller. *)

  type config = { rules : rule list }
  (** Bucket CORS configuration sent to or returned from S3. *)

  type result = { config : config; response : Awskit.Response.t }
  (** [GetBucketCors] result metadata. *)

  val default_options : options
  (** Default bucket CORS options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket CORS request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Public_access_block : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket public-access-block request options. *)

  type config = {
    block_public_acls : bool;
        (** Block calls that attempt to set public ACLs. *)
    ignore_public_acls : bool;
        (** Ignore public ACLs already attached to bucket/object resources. *)
    block_public_policy : bool;  (** Block public bucket policies. *)
    restrict_public_buckets : bool;
        (** Restrict access through public bucket policies. *)
  }
  (** S3 public-access-block configuration. *)

  type result = { config : config; response : Awskit.Response.t }
  (** [GetPublicAccessBlock] result metadata. *)

  val all_false : config
  (** Configuration with every public-access-block switch disabled. *)

  val default_options : options
  (** Default bucket public-access-block options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket public-access-block request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Ownership_controls : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** Bucket ownership-controls request options. *)

  (** S3 object ownership controls. *)
  module Object_ownership : sig
    (** Object ownership mode. Unknown values are preserved on reads for forward
        compatibility, but rejected by write operations. *)
    type t =
      | Bucket_owner_enforced
      | Bucket_owner_preferred
      | Object_writer
      | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  type config = { object_ownership : Object_ownership.t }
  (** Bucket ownership-controls configuration. *)

  type result = { config : config; response : Awskit.Response.t }
  (** [GetBucketOwnershipControls] result metadata. *)

  val default_options : options
  (** Default bucket ownership-controls options: no owner guard. *)

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build bucket ownership-controls request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end
