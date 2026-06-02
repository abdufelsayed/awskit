(** S3 bucket data types, bucket-operation options, and bucket-operation
    results. *)

type info = { name : string; creation_date : Ptime.t option }

module Create : sig
  type options = { region : Awskit.Region.t option }
  type result = { response : Awskit.Response.t }

  val default_options : options
end

module Delete : sig
  type result = { response : Awskit.Response.t }
end

module Head : sig
  type result = {
    name : string;
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }

  type info = result
end

module List_buckets : sig
  type result = { buckets : info list; response : Awskit.Response.t }
end

module Get_location : sig
  type result = {
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }
end

module Versioning : sig
  module Status : sig
    type t = Enabled | Suspended

    val to_string : t -> string
    val of_string : string -> t option
  end

  type result = { status : Status.t option; response : Awskit.Response.t }
end

module Tagging : sig
  type result = { tags : Tag.t list; response : Awskit.Response.t }
end

module Encryption : sig
  module Algorithm : sig
    type t = Aes256 | Aws_kms | Aws_kms_dsse | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  module Blocked_encryption_type : sig
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
  end

  type config = { rules : Rule.t list }
  type result = { config : config; response : Awskit.Response.t }
end

module Cors : sig
  module Method : sig
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

  type result = { config : config; response : Awskit.Response.t }

  val all_false : config
end

module Ownership_controls : sig
  module Object_ownership : sig
    type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

    val to_string : t -> string
    val of_string : string -> t option
  end

  type config = { object_ownership : Object_ownership.t }
  type result = { config : config; response : Awskit.Response.t }
end
