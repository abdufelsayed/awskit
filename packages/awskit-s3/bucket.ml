open Common

type info = { name : string; creation_date : Ptime.t option }

module Create = struct
  type options = { region : Awskit.Region.t option }
  type result = { request : Awskit.Response.t }

  let default_options = { region = None }
end

module Delete = struct
  type result = { request : Awskit.Response.t }
end

module Head = struct
  type info = {
    name : string;
    region : Awskit.Region.t option;
    request : Awskit.Response.t;
  }
end

module Versioning = struct
  module Status = struct
    type t = Enabled | Suspended

    let to_string = function Enabled -> "Enabled" | Suspended -> "Suspended"

    let of_string = function
      | "Enabled" -> Some Enabled
      | "Suspended" -> Some Suspended
      | _ -> None
  end

  type result = { status : Status.t option; request : Awskit.Response.t }
end

module Tagging = struct
  type result = { tags : Tag.t list; request : Awskit.Response.t }
end

module Encryption = struct
  module Algorithm = struct
    type t = Aes256 | Aws_kms

    let to_string = function Aes256 -> "AES256" | Aws_kms -> "aws:kms"

    let of_string = function
      | "AES256" -> Some Aes256
      | "aws:kms" -> Some Aws_kms
      | _ -> None
  end

  module Rule = struct
    type t = { sse_algorithm : Algorithm.t; kms_master_key_id : string option }
  end

  type config = { rules : Rule.t list }
  type result = { config : config; request : Awskit.Response.t }
end

module Cors = struct
  module Method = struct
    type t = Get | Put | Post | Delete | Head

    let to_string = function
      | Get -> "GET"
      | Put -> "PUT"
      | Post -> "POST"
      | Delete -> "DELETE"
      | Head -> "HEAD"

    let of_string = function
      | "GET" -> Some Get
      | "PUT" -> Some Put
      | "POST" -> Some Post
      | "DELETE" -> Some Delete
      | "HEAD" -> Some Head
      | _ -> None
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

module Website = struct
  type config = {
    index_document_suffix : string option;
    error_document_key : string option;
  }

  type result = { config : config; request : Awskit.Response.t }
end

module Public_access_block = struct
  type config = {
    block_public_acls : bool;
    ignore_public_acls : bool;
    block_public_policy : bool;
    restrict_public_buckets : bool;
  }

  type result = { config : config; request : Awskit.Response.t }

  let all_false =
    {
      block_public_acls = false;
      ignore_public_acls = false;
      block_public_policy = false;
      restrict_public_buckets = false;
    }
end

module Ownership_controls = struct
  module Object_ownership = struct
    type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

    let to_string = function
      | Bucket_owner_enforced -> "BucketOwnerEnforced"
      | Bucket_owner_preferred -> "BucketOwnerPreferred"
      | Object_writer -> "ObjectWriter"

    let of_string = function
      | "BucketOwnerEnforced" -> Some Bucket_owner_enforced
      | "BucketOwnerPreferred" -> Some Bucket_owner_preferred
      | "ObjectWriter" -> Some Object_writer
      | _ -> None
  end

  type config = { object_ownership : Object_ownership.t }
  type result = { config : config; request : Awskit.Response.t }
end

module Request_payment = struct
  module Payer = struct
    type t = Bucket_owner | Requester

    let to_string = function
      | Bucket_owner -> "BucketOwner"
      | Requester -> "Requester"

    let of_string = function
      | "BucketOwner" -> Some Bucket_owner
      | "Requester" -> Some Requester
      | _ -> None
  end

  type result = { payer : Payer.t option; request : Awskit.Response.t }
end

module Accelerate = struct
  module Status = struct
    type t = Enabled | Suspended

    let to_string = function Enabled -> "Enabled" | Suspended -> "Suspended"

    let of_string = function
      | "Enabled" -> Some Enabled
      | "Suspended" -> Some Suspended
      | _ -> None
  end

  type result = { status : Status.t option; request : Awskit.Response.t }
end

module Policy_status = struct
  type result = { is_public : bool option; request : Awskit.Response.t }
end

module Logging = struct
  type target = { target_bucket : string; target_prefix : string }
  type config = { logging : target option }
  type result = { config : config; request : Awskit.Response.t }

  let disabled = { logging = None }

  let enabled ~target_bucket ~target_prefix =
    { logging = Some { target_bucket; target_prefix } }
end
