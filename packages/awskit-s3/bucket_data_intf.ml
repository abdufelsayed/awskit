open Common

open struct
  module Bucket = Bucket
end

module type BUCKET_DATA = sig
  type info = { name : string; creation_date : Ptime.t option }

  module Create : sig
    type options = { region : Awskit.Region.t option }
    type result = { request : Awskit.Response.t }

    val default_options : options
  end

  module Delete : sig
    type result = { request : Awskit.Response.t }
  end

  module Head : sig
    type info = {
      name : string;
      region : Awskit.Region.t option;
      request : Awskit.Response.t;
    }
  end

  module Versioning : sig
    module Status : sig
      type t = Enabled | Suspended

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { status : Status.t option; request : Awskit.Response.t }
  end

  module Tagging : sig
    type result = { tags : Tag.t list; request : Awskit.Response.t }
  end

  module Encryption : sig
    module Algorithm : sig
      type t = Aes256 | Aws_kms

      val to_string : t -> string
      val of_string : string -> t option
    end

    module Rule : sig
      type t = {
        sse_algorithm : Algorithm.t;
        kms_master_key_id : string option;
      }
    end

    type config = { rules : Rule.t list }
    type result = { config : config; request : Awskit.Response.t }
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
    type result = { config : config; request : Awskit.Response.t }
  end

  module Website : sig
    type config = {
      index_document_suffix : string option;
      error_document_key : string option;
    }

    type result = { config : config; request : Awskit.Response.t }
  end

  module Public_access_block : sig
    type config = {
      block_public_acls : bool;
      ignore_public_acls : bool;
      block_public_policy : bool;
      restrict_public_buckets : bool;
    }

    type result = { config : config; request : Awskit.Response.t }

    val all_false : config
  end

  module Ownership_controls : sig
    module Object_ownership : sig
      type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

      val to_string : t -> string
      val of_string : string -> t option
    end

    type config = { object_ownership : Object_ownership.t }
    type result = { config : config; request : Awskit.Response.t }
  end

  module Request_payment : sig
    module Payer : sig
      type t = Bucket_owner | Requester

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { payer : Payer.t option; request : Awskit.Response.t }
  end

  module Accelerate : sig
    module Status : sig
      type t = Enabled | Suspended

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { status : Status.t option; request : Awskit.Response.t }
  end

  module Policy_status : sig
    type result = { is_public : bool option; request : Awskit.Response.t }
  end

  module Logging : sig
    type target = { target_bucket : string; target_prefix : string }
    type config = { logging : target option }
    type result = { config : config; request : Awskit.Response.t }

    val disabled : config
    val enabled : target_bucket:string -> target_prefix:string -> config
  end
end
