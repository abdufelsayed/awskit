type info = { name : Bucket_name.t; creation_date : Ptime.t option }

module Create = struct
  type result = { response : Awskit.Response.t }
end

module Delete = struct
  type result = { response : Awskit.Response.t }
end

module Head = struct
  type result = {
    name : Bucket_name.t;
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }

  type info = result
end

module List_buckets = struct
  type result = { buckets : info list; response : Awskit.Response.t }
end

module Get_location = struct
  type result = { region : Awskit.Region.t; response : Awskit.Response.t }
end

module Versioning = struct
  module Status = struct
    type t = Enabled | Suspended
    type observed = Known of t | Unknown of string

    let to_string = function Enabled -> "Enabled" | Suspended -> "Suspended"

    let observed_to_string = function
      | Known status -> to_string status
      | Unknown value -> value

    let observed_of_string = function
      | "Enabled" -> Known Enabled
      | "Suspended" -> Known Suspended
      | value -> Unknown value
  end

  type result = {
    status : Status.observed option;
    response : Awskit.Response.t;
  }
end

module Tagging = struct
  type result = { tags : Tag.Set.t; response : Awskit.Response.t }
end

module Encryption = struct
  module Default_encryption = struct
    type t =
      | Sse_s3
      | Sse_kms of { key_id : string option; bucket_key_enabled : bool option }
      | Dsse_kms of { key_id : string option }

    let ( let* ) = Result.bind

    let validate_key_id = function
      | None -> Ok ()
      | Some key_id ->
          let* _ =
            S3_utf8.validate ~field:"kms key id" ~name:"KMS key id" key_id
          in
          S3_validation.validate_header_value ~field:"kms key id" key_id

    let sse_s3 = Sse_s3

    let sse_kms ?key_id ?bucket_key_enabled () =
      Result.map
        (fun () -> Sse_kms { key_id; bucket_key_enabled })
        (validate_key_id key_id)

    let sse_kms_exn ?key_id ?bucket_key_enabled () =
      Awskit.Error.Producer.get_ok_exn (sse_kms ?key_id ?bucket_key_enabled ())

    let dsse_kms ?key_id () =
      Result.map (fun () -> Dsse_kms { key_id }) (validate_key_id key_id)

    let dsse_kms_exn ?key_id () =
      Awskit.Error.Producer.get_ok_exn (dsse_kms ?key_id ())
  end

  module Sse_c_policy = struct
    type t = Allow | Block
  end

  module Rule = struct
    type t =
      | Default of Default_encryption.t
      | Sse_c of Sse_c_policy.t
      | Default_and_sse_c of {
          default_encryption : Default_encryption.t;
          sse_c_policy : Sse_c_policy.t;
        }
  end

  module Config = struct
    type t = { rules : Rule.t list }

    let singleton rule = { rules = [ rule ] }

    let of_rules = function
      | [] ->
          S3_error_context.invalid ~field:"encryption"
            "encryption config must include a rule"
      | rules -> Ok { rules }

    let of_rules_exn rules = Awskit.Error.Producer.get_ok_exn (of_rules rules)
  end

  module Observed = struct
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

    module Sse_c_policy = struct
      type t = Allow | Block | Unknown of string

      let to_string = function
        | Allow -> "NONE"
        | Block -> "SSE-C"
        | Unknown value -> value

      let of_string = function
        | "NONE" -> Allow
        | "SSE-C" -> Block
        | value -> Unknown value
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
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
end

module Cors = struct
  module Method = struct
    type t = Get | Put | Post | Delete | Head
    type observed = Known of t | Unknown of string

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

    let observed_to_string = function
      | Known method_ -> to_string method_
      | Unknown value -> value

    let observed_of_string value =
      match of_string value with
      | Some method_ -> Known method_
      | None -> Unknown value
  end

  module Rule = struct
    type t = {
      id : string option;
      allowed_origins : string list;
      allowed_methods : Method.t list;
      allowed_headers : string list;
      expose_headers : string list;
      max_age_seconds : int option;
    }

    let ( let* ) = Result.bind

    let validate_strings ~field values =
      let rec loop = function
        | [] -> Ok ()
        | value :: rest ->
            let* _ = S3_utf8.validate ~field ~name:field value in
            let* () = S3_validation.validate_header_value ~field value in
            loop rest
      in
      loop values

    let validate_id = function
      | None -> Ok ()
      | Some id ->
          let* _ =
            S3_utf8.validate ~max_scalars:255 ~field:"cors rule id"
              ~name:"CORS rule id" id
          in
          S3_validation.validate_header_value ~field:"cors rule id" id

    let create ?id ~allowed_origins ~allowed_methods ?(allowed_headers = [])
        ?(expose_headers = []) ?max_age_seconds () =
      let* () = validate_id id in
      let* () = validate_strings ~field:"allowed origin" allowed_origins in
      let* () = validate_strings ~field:"allowed header" allowed_headers in
      let* () = validate_strings ~field:"expose header" expose_headers in
      let* () =
        match allowed_origins with
        | [] ->
            S3_error_context.invalid ~field:"cors"
              "CORS rule must include an allowed origin"
        | _ -> Ok ()
      in
      let* () =
        match allowed_methods with
        | [] ->
            S3_error_context.invalid ~field:"cors"
              "CORS rule must include an allowed method"
        | _ -> Ok ()
      in
      let* () =
        match max_age_seconds with
        | Some value when value < 0 ->
            S3_error_context.invalid ~field:"cors"
              "max_age_seconds must be non-negative"
        | _ -> Ok ()
      in
      Ok
        {
          id;
          allowed_origins;
          allowed_methods;
          allowed_headers;
          expose_headers;
          max_age_seconds;
        }

    let create_exn ?id ~allowed_origins ~allowed_methods ?allowed_headers
        ?expose_headers ?max_age_seconds () =
      Awskit.Error.Producer.get_ok_exn
        (create ?id ~allowed_origins ~allowed_methods ?allowed_headers
           ?expose_headers ?max_age_seconds ())
  end

  module Config = struct
    type t = { rules : Rule.t list }

    let singleton rule = { rules = [ rule ] }

    let of_rules rules =
      match List.length rules with
      | 0 ->
          S3_error_context.invalid ~field:"cors"
            "CORS config must include a rule"
      | count when count > 100 ->
          S3_error_context.invalid ~field:"cors"
            "CORS config must include at most 100 rules"
      | _ -> Ok { rules }

    let of_rules_exn rules = Awskit.Error.Producer.get_ok_exn (of_rules rules)
  end

  module Observed = struct
    type rule = {
      id : string option;
      allowed_origins : string list;
      allowed_methods : Method.observed list;
      allowed_headers : string list;
      expose_headers : string list;
      max_age_seconds : int option;
    }

    type t = { rules : rule list }
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
end

module Public_access_block = struct
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
end

module Ownership_controls = struct
  module Object_ownership = struct
    type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer
    type observed = Known of t | Unknown of string

    let to_string = function
      | Bucket_owner_enforced -> "BucketOwnerEnforced"
      | Bucket_owner_preferred -> "BucketOwnerPreferred"
      | Object_writer -> "ObjectWriter"

    let observed_to_string = function
      | Known ownership -> to_string ownership
      | Unknown value -> value

    let observed_of_string = function
      | "BucketOwnerEnforced" -> Known Bucket_owner_enforced
      | "BucketOwnerPreferred" -> Known Bucket_owner_preferred
      | "ObjectWriter" -> Known Object_writer
      | value -> Unknown value
  end

  type config = { object_ownership : Object_ownership.t }

  module Observed = struct
    type t = { object_ownership : Object_ownership.observed }
  end

  type result = { config : Observed.t; response : Awskit.Response.t }
end
