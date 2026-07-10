(** S3 bucket data types and bucket-operation results. *)

type info = { name : Bucket_name.t; creation_date : Ptime.t option }
(** Bucket summary returned by list operations. [creation_date] may be absent
    when the service response omits it. *)

module Create : sig
  type result = {
    location : string option;
        (** Bucket location from the [Location] response header, when present.
        *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [CreateBucket] result metadata. *)
end

module Head : sig
  type result = {
    name : Bucket_name.t;  (** Bucket name that was checked. *)
    region : Awskit.Region.t option;
        (** Region hint from [x-amz-bucket-region], when present. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [HeadBucket] result metadata. *)

  type info = result
end

module List_buckets : sig
  type result = { buckets : info list; response : Awskit.Response.t }
  (** [ListBuckets] result metadata. *)
end

module Get_location : sig
  type result = {
    region : Awskit.Region.t;
        (** Bucket region. S3's legacy empty default-location encoding is
            normalized to [us-east-1]. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [GetBucketLocation] result metadata. *)
end

module Versioning : sig
  module Status : sig
    (** Closed bucket versioning state accepted by [put]. *)
    type t = Enabled | Suspended

    (** Versioning state observed in a response. *)
    type observed = Known of t | Unknown of string

    val to_string : t -> string
    val observed_to_string : observed -> string
    val observed_of_string : string -> observed
  end

  type result = {
    status : Status.observed option;
    response : Awskit.Response.t;
  }
  (** [GetBucketVersioning] result metadata. [None] means S3 did not return a
      versioning status. *)
end

module Tagging : sig
  type result = { tags : Tag.Set.t; response : Awskit.Response.t }
  (** Bucket tagging result metadata. *)
end

module Encryption : sig
  module Default_encryption : sig
    (** Valid default encryption for new bucket objects. The private variant
        keeps its alternatives inspectable while requiring validated KMS key
        strings. *)
    type t = private
      | Sse_s3
      | Sse_kms of { key_id : string option; bucket_key_enabled : bool option }
      | Dsse_kms of { key_id : string option }

    val sse_s3 : t
    (** S3-managed AES256 encryption. *)

    val sse_kms :
      ?key_id:string ->
      ?bucket_key_enabled:bool ->
      unit ->
      (t, Awskit.Error.t) Stdlib.result
    (** Validated SSE-KMS encryption. An omitted key uses the service default.
    *)

    val sse_kms_exn : ?key_id:string -> ?bucket_key_enabled:bool -> unit -> t

    val dsse_kms : ?key_id:string -> unit -> (t, Awskit.Error.t) Stdlib.result
    (** Validated DSSE-KMS encryption. Bucket keys are absent by construction.
    *)

    val dsse_kms_exn : ?key_id:string -> unit -> t
  end

  module Sse_c_policy : sig
    (** Whether uploads using customer-provided encryption keys are allowed or
        blocked. *)
    type t = Allow | Block
  end

  module Rule : sig
    (** One sendable encryption rule. Every case has a documented effect. *)
    type t =
      | Default of Default_encryption.t
      | Sse_c of Sse_c_policy.t
      | Default_and_sse_c of {
          default_encryption : Default_encryption.t;
          sse_c_policy : Sse_c_policy.t;
        }
  end

  module Config : sig
    type t = private { rules : Rule.t list }
    (** Non-empty ordered bucket encryption configuration. *)

    val singleton : Rule.t -> t
    val of_rules : Rule.t list -> (t, Awskit.Error.t) Stdlib.result
    val of_rules_exn : Rule.t list -> t
  end

  module Observed : sig
    module Algorithm : sig
      type t = Aes256 | Aws_kms | Aws_kms_dsse | Unknown of string

      val to_string : t -> string
      val of_string : string -> t
    end

    module Sse_c_policy : sig
      type t = Allow | Block | Unknown of string

      val to_string : t -> string
      val of_string : string -> t
    end

    type default_encryption = {
      algorithm : Algorithm.t option;
      kms_key_id : string option;
    }

    type rule = {
      default_encryption : default_encryption option;
      bucket_key_enabled : bool option;
      sse_c_policies : Sse_c_policy.t list;
    }

    type t = { rules : rule list }
    (** Configuration observed on the wire. Unknown future tokens and unusual
        response combinations are preserved but cannot be sent by [put]. *)
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
  (** [GetBucketEncryption] result metadata. *)
end

module Cors : sig
  (** Bucket CORS configuration. *)
  module Method : sig
    (** Closed HTTP-method vocabulary for sendable rules. *)
    type t = Get | Put | Post | Delete | Head

    (** A method observed in a response. *)
    type observed = Known of t | Unknown of string

    val to_string : t -> string
    val of_string : string -> t option
    val observed_to_string : observed -> string
    val observed_of_string : string -> observed
  end

  module Rule : sig
    type t = private {
      id : string option;
      allowed_origins : string list;
      allowed_methods : Method.t list;
      allowed_headers : string list;
      expose_headers : string list;
      max_age_seconds : int option;
    }
    (** A sendable CORS rule. Lists are emitted in caller order. *)

    val create :
      ?id:string ->
      allowed_origins:string list ->
      allowed_methods:Method.t list ->
      ?allowed_headers:string list ->
      ?expose_headers:string list ->
      ?max_age_seconds:int ->
      unit ->
      (t, Awskit.Error.t) Stdlib.result

    val create_exn :
      ?id:string ->
      allowed_origins:string list ->
      allowed_methods:Method.t list ->
      ?allowed_headers:string list ->
      ?expose_headers:string list ->
      ?max_age_seconds:int ->
      unit ->
      t
  end

  module Config : sig
    type t = private { rules : Rule.t list }
    (** An ordered CORS configuration containing one through 100 rules. *)

    val singleton : Rule.t -> t
    val of_rules : Rule.t list -> (t, Awskit.Error.t) Stdlib.result
    val of_rules_exn : Rule.t list -> t
  end

  module Observed : sig
    type rule = {
      id : string option;
      allowed_origins : string list;
      allowed_methods : Method.observed list;
      allowed_headers : string list;
      expose_headers : string list;
      max_age_seconds : int option;
    }

    type t = { rules : rule list }
    (** CORS configuration observed on the wire. Unknown methods are preserved
        but cannot be sent by [put]. *)
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
  (** [GetBucketCors] result metadata. *)
end

module Public_access_block : sig
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
end

module Ownership_controls : sig
  (** S3 object ownership controls. *)
  module Object_ownership : sig
    (** Closed object-ownership mode accepted by [put]. *)
    type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

    (** Object-ownership mode observed in a response. *)
    type observed = Known of t | Unknown of string

    val to_string : t -> string
    val observed_to_string : observed -> string
    val observed_of_string : string -> observed
  end

  type config = { object_ownership : Object_ownership.t }
  (** Bucket ownership-controls configuration. *)

  module Observed : sig
    type t = { object_ownership : Object_ownership.observed }
    (** Ownership controls observed in a response. *)
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
  (** [GetBucketOwnershipControls] result metadata. *)
end
