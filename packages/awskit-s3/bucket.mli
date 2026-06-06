(** S3 bucket data types, bucket-operation options, and bucket-operation
    results. *)

type info = { name : string; creation_date : Ptime.t option }
(** Bucket summary returned by list operations. [creation_date] may be absent
    when the service response omits it. *)

module Create : sig
  (** [CreateBucket] options and result metadata. *)
  type options = { region : Awskit.Region.t option }
  (** Optional location constraint. Some regions, notably [us-east-1], have
      special AWS behavior and may omit an explicit constraint. *)

  type result = { response : Awskit.Response.t }

  val default_options : options
end

module Delete : sig
  type result = { response : Awskit.Response.t }
  (** [DeleteBucket] result metadata. *)
end

module Head : sig
  type result = {
    name : string;
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
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
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }
  (** [GetBucketLocation] result metadata. [None] represents S3 responses that
      encode the default region without a concrete location constraint. *)
end

module Versioning : sig
  module Status : sig
    (** Bucket versioning state. Absence in a result means versioning was never
        configured for the bucket. *)
    type t = Enabled | Suspended

    val to_string : t -> string
    val of_string : string -> t option
  end

  type result = { status : Status.t option; response : Awskit.Response.t }
end

module Tagging : sig
  type result = { tags : Tag.t list; response : Awskit.Response.t }
  (** Bucket tagging result metadata. *)
end

module Encryption : sig
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
      kms_master_key_id : string option;
      bucket_key_enabled : bool option;
      blocked_encryption_types : Blocked_encryption_type.t list;
    }
    (** One default-encryption rule. *)
  end

  type config = { rules : Rule.t list }
  (** Bucket encryption configuration. *)

  type result = { config : config; response : Awskit.Response.t }
end

module Cors : sig
  (** Bucket CORS configuration. *)
  module Method : sig
    (** HTTP methods accepted in CORS rules. *)
    type t = Get | Put | Post | Delete | Head

    val to_string : t -> string
    val of_string : string -> t option
  end

  type rule = {
    id : string option;
    allowed_origins : string list;
    allowed_methods : Method.t list;
    allowed_headers : string list;
    expose_headers : string list;
    max_age_seconds : int option;
  }
  (** One CORS rule. Lists are emitted in the order supplied by the caller. *)

  type config = { rules : rule list }
  type result = { config : config; response : Awskit.Response.t }
end

module Public_access_block : sig
  type config = {
    block_public_acls : bool;
    ignore_public_acls : bool;
    block_public_policy : bool;
    restrict_public_buckets : bool;
  }
  (** S3 public-access-block configuration. *)

  type result = { config : config; response : Awskit.Response.t }

  val all_false : config
  (** Configuration with every public-access-block switch disabled. *)
end

module Ownership_controls : sig
  (** S3 object ownership controls. *)
  module Object_ownership : sig
    (** Object ownership mode. *)
    type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

    val to_string : t -> string
    val of_string : string -> t option
  end

  type config = { object_ownership : Object_ownership.t }
  type result = { config : config; response : Awskit.Response.t }
end
