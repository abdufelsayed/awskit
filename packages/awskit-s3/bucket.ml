open Common

type info = { name : Bucket_name.t; creation_date : Ptime.t option }

module Create = struct
  type options = { region : Awskit.Region.t option }
  type result = { response : Awskit.Response.t }

  let default_options = { region = None }
  let options ?region () = Ok { region }

  let options_exn ?region () =
    Awskit.Error.Internal.get_ok_exn (options ?region ())
end

module Delete = struct
  type options = { expected_bucket_owner : Account_id.t option }
  type result = { response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Head = struct
  type options = { expected_bucket_owner : Account_id.t option }

  type result = {
    name : Bucket_name.t;
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }

  type info = result

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module List_buckets = struct
  type result = { buckets : info list; response : Awskit.Response.t }
end

module Get_location = struct
  type options = { expected_bucket_owner : Account_id.t option }

  type result = {
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Policy = struct
  type options = { expected_bucket_owner : Account_id.t option }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Versioning = struct
  type options = { expected_bucket_owner : Account_id.t option }

  module Status = struct
    type t = Enabled | Suspended

    let to_string = function Enabled -> "Enabled" | Suspended -> "Suspended"

    let of_string = function
      | "Enabled" -> Some Enabled
      | "Suspended" -> Some Suspended
      | _ -> None
  end

  type result = { status : Status.t option; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Tagging = struct
  type options = { expected_bucket_owner : Account_id.t option }
  type result = { tags : Tag.Set.t; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Encryption = struct
  type options = { expected_bucket_owner : Account_id.t option }

  module Algorithm = struct
    type t = Aes256 | Aws_kms | Aws_kms_dsse | Unknown of string

    let to_string = function
      | Aes256 -> "AES256"
      | Aws_kms -> "aws:kms"
      | Aws_kms_dsse -> "aws:kms:dsse"
      | Unknown value -> value

    let of_string = function
      | "AES256" -> Aes256
      | "aws:kms" -> Aws_kms
      | "aws:kms:dsse" -> Aws_kms_dsse
      | value -> Unknown value
  end

  module Blocked_encryption_type = struct
    type t = Sse_c | No_block | Unknown of string

    let to_string = function
      | Sse_c -> "SSE-C"
      | No_block -> "NONE"
      | Unknown value -> value

    let of_string = function
      | "SSE-C" -> Sse_c
      | "NONE" -> No_block
      | value -> Unknown value
  end

  module Rule = struct
    type t = {
      sse_algorithm : Algorithm.t option;
      kms_master_key_id : string option;
      bucket_key_enabled : bool option;
      blocked_encryption_types : Blocked_encryption_type.t list;
    }
  end

  type config = { rules : Rule.t list }
  type result = { config : config; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Cors = struct
  type options = { expected_bucket_owner : Account_id.t option }

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
  type result = { config : config; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Public_access_block = struct
  type options = { expected_bucket_owner : Account_id.t option }

  type config = {
    block_public_acls : bool;
    ignore_public_acls : bool;
    block_public_policy : bool;
    restrict_public_buckets : bool;
  }

  type result = { config : config; response : Awskit.Response.t }

  let all_false =
    {
      block_public_acls = false;
      ignore_public_acls = false;
      block_public_policy = false;
      restrict_public_buckets = false;
    }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end

module Ownership_controls = struct
  type options = { expected_bucket_owner : Account_id.t option }

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
  type result = { config : config; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    Awskit.Error.Internal.get_ok_exn (options ?expected_bucket_owner ())
end
