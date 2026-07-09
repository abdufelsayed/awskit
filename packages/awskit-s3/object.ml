let ( let* ) = S3_result.( let* )

module Etag = struct
  type t = string

  let of_string value =
    if value = "" then
      S3_error_context.invalid ~field:"etag" "etag must be non-empty"
    else if S3_string.has_ctl_or_del value then
      S3_error_context.invalid ~field:"etag" "etag contains control characters"
    else Ok value

  let of_string_exn value = S3_result.result_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Version_id = struct
  type t = string

  let of_string value =
    if value = "" then
      S3_error_context.invalid ~field:"version_id"
        "version id must be non-empty"
    else if S3_string.has_ctl_or_del value then
      S3_error_context.invalid ~field:"version_id"
        "version id contains control characters"
    else Ok value

  let of_string_exn value = S3_result.result_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Checksum = struct
  module Algorithm = struct
    type t =
      | Crc32
      | Crc32c
      | Crc64nvme
      | Sha1
      | Sha256
      | Sha512
      | Md5
      | Xxhash64
      | Xxhash3
      | Xxhash128

    type observed = Known of t | Unknown of string

    let to_string = function
      | Crc32 -> "CRC32"
      | Crc32c -> "CRC32C"
      | Crc64nvme -> "CRC64NVME"
      | Sha1 -> "SHA1"
      | Sha256 -> "SHA256"
      | Sha512 -> "SHA512"
      | Md5 -> "MD5"
      | Xxhash64 -> "XXHASH64"
      | Xxhash3 -> "XXHASH3"
      | Xxhash128 -> "XXHASH128"

    let known_of_string = function
      | "CRC32" -> Some Crc32
      | "CRC32C" -> Some Crc32c
      | "CRC64NVME" -> Some Crc64nvme
      | "SHA1" -> Some Sha1
      | "SHA256" -> Some Sha256
      | "SHA512" -> Some Sha512
      | "MD5" -> Some Md5
      | "XXHASH64" -> Some Xxhash64
      | "XXHASH3" -> Some Xxhash3
      | "XXHASH128" -> Some Xxhash128
      | _ -> None

    let of_string value =
      match known_of_string value with
      | Some algorithm -> Ok algorithm
      | None ->
          S3_error_context.invalid ~field:"checksum_algorithm"
            "unsupported checksum algorithm %S" value

    let of_string_exn value = S3_result.result_exn (of_string value)

    let observed_of_string value =
      match known_of_string value with
      | Some algorithm -> Known algorithm
      | None -> Unknown value

    let observed_to_string = function
      | Known algorithm -> to_string algorithm
      | Unknown value -> value
  end

  module Type = struct
    type t = Composite | Full_object
    type observed = Known of t | Unknown of string

    let to_string = function
      | Composite -> "COMPOSITE"
      | Full_object -> "FULL_OBJECT"

    let known_of_string = function
      | "COMPOSITE" -> Some Composite
      | "FULL_OBJECT" -> Some Full_object
      | _ -> None

    let of_string value =
      match known_of_string value with
      | Some checksum_type -> Ok checksum_type
      | None ->
          S3_error_context.invalid ~field:"checksum_type"
            "unsupported checksum type %S" value

    let of_string_exn value = S3_result.result_exn (of_string value)

    let observed_of_string value =
      match known_of_string value with
      | Some checksum_type -> Known checksum_type
      | None -> Unknown value

    let observed_to_string = function
      | Known checksum_type -> to_string checksum_type
      | Unknown value -> value
  end

  module Mode = struct
    type t = Enabled

    let to_string = function Enabled -> "ENABLED"
  end

  type value = { algorithm : Algorithm.t; value : string }

  let value ~algorithm ~value =
    let* () =
      S3_validation.validate_header_value ~field:"checksum_value" value
    in
    Ok { algorithm; value }

  let value_exn ~algorithm ~value:checksum =
    S3_result.result_exn (value ~algorithm ~value:checksum)

  type observed_value = { algorithm : Algorithm.observed; value : string }

  type response = {
    values : observed_value list;
    checksum_type : Type.observed option;
  }

  type summary = {
    algorithms : Algorithm.observed list;
    checksum_type : Type.observed option;
  }

  let empty_response = { values = []; checksum_type = None }
  let empty_summary = { algorithms = []; checksum_type = None }
end

module Owner = struct
  type t = { id : string option; display_name : string option }

  let non_empty = function None | Some "" -> None | Some _ as value -> value

  let create ?id ?display_name () =
    let id = non_empty id in
    let display_name = non_empty display_name in
    match (id, display_name) with
    | None, None -> None
    | _ -> Some { id; display_name }
end

module Etag_condition = struct
  type t = Any | Etag of Etag.t

  let any = Any
  let etag etag = Etag etag
end

module Preconditions = struct
  module Write = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
    }

    let none = { if_match = None; if_none_match = None }
    let if_absent = { none with if_none_match = Some Etag_condition.Any }
    let if_etag etag = { none with if_match = Some (Etag_condition.Etag etag) }
  end

  module Read = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
      if_modified_since : Ptime.t option;
      if_unmodified_since : Ptime.t option;
    }

    let none =
      {
        if_match = None;
        if_none_match = None;
        if_modified_since = None;
        if_unmodified_since = None;
      }
  end

  module Delete = struct
    type t = { if_match : Etag_condition.t option }

    let none = { if_match = None }
    let if_etag etag = { if_match = Some (Etag_condition.Etag etag) }
  end

  module Copy_source = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
      if_modified_since : Ptime.t option;
      if_unmodified_since : Ptime.t option;
    }

    let none =
      {
        if_match = None;
        if_none_match = None;
        if_modified_since = None;
        if_unmodified_since = None;
      }
  end
end

module Put = struct
  type options = {
    content_type : Content_type.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.Set.t;
    cache_control : Header_value.t option;
    content_encoding : Header_value.t option;
    content_disposition : Header_value.t option;
    preconditions : Preconditions.Write.t;
    checksum : Checksum.value option;
    encryption : Encryption.Destination.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type result = {
    etag : Etag.t option;
    version_id : Version_id.t option;
    checksum : Checksum.response;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      content_type = None;
      metadata = Metadata.empty;
      storage_class = None;
      tags = Tag.Set.empty;
      cache_control = None;
      content_encoding = None;
      content_disposition = None;
      preconditions = Preconditions.Write.none;
      checksum = None;
      encryption = None;
      expected_bucket_owner = None;
    }

  let options ?content_type ?(metadata = Metadata.empty) ?storage_class
      ?(tags = Tag.Set.empty) ?cache_control ?content_encoding
      ?content_disposition ?(preconditions = Preconditions.Write.none) ?checksum
      ?encryption ?expected_bucket_owner () =
    {
      content_type;
      metadata;
      storage_class;
      tags;
      cache_control;
      content_encoding;
      content_disposition;
      preconditions;
      checksum;
      encryption;
      expected_bucket_owner;
    }
end

module Get = struct
  type options = {
    range : Range.t option;
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    source_encryption : Encryption.Source.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type info = {
    etag : Etag.t option;
    content_type : Content_type.t option;
    content_length : int64 option;
    content_range : Range.Content_range.t option;
    last_modified : Ptime.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    version_id : Version_id.t option;
    checksum : Checksum.response;
    encryption : Encryption.Observed.t option;
    response : Awskit.Response.t;
  }

  type 'a result = {
    value : 'a;
    etag : Etag.t option;
    content_type : Content_type.t option;
    content_length : int64 option;
    content_range : Range.Content_range.t option;
    last_modified : Ptime.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    version_id : Version_id.t option;
    checksum : Checksum.response;
    encryption : Encryption.Observed.t option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      range = None;
      preconditions = Preconditions.Read.none;
      version_id = None;
      checksum_mode = None;
      source_encryption = None;
      expected_bucket_owner = None;
    }

  let options ?range ?(preconditions = Preconditions.Read.none) ?version_id
      ?checksum_mode ?source_encryption ?expected_bucket_owner () =
    {
      range;
      preconditions;
      version_id;
      checksum_mode;
      source_encryption;
      expected_bucket_owner;
    }
end

module Head = struct
  type options = {
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    source_encryption : Encryption.Source.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type result = Get.info

  let default_options =
    {
      preconditions = Preconditions.Read.none;
      version_id = None;
      checksum_mode = None;
      source_encryption = None;
      expected_bucket_owner = None;
    }

  let options ?(preconditions = Preconditions.Read.none) ?version_id
      ?checksum_mode ?source_encryption ?expected_bucket_owner () =
    {
      preconditions;
      version_id;
      checksum_mode;
      source_encryption;
      expected_bucket_owner;
    }
end

module Delete = struct
  type options = {
    preconditions : Preconditions.Delete.t;
    version_id : Version_id.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type result = {
    delete_marker : bool option;
    version_id : Version_id.t option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      preconditions = Preconditions.Delete.none;
      version_id = None;
      expected_bucket_owner = None;
    }

  let options ?(preconditions = Preconditions.Delete.none) ?version_id
      ?expected_bucket_owner () =
    { preconditions; version_id; expected_bucket_owner }
end

module Delete_many = struct
  let max_objects = 1000

  type object_ = {
    key : Object_key.t;
    version_id : Version_id.t option;
    etag : Etag.t option;
  }

  let object_ ~key ?version_id ?etag () = { key; version_id; etag }

  type deleted = {
    key : Object_key.t;
    version_id : Version_id.t option;
    delete_marker : bool option;
    delete_marker_version_id : Version_id.t option;
  }

  type item_error = {
    key : Object_key.t;
    version_id : Version_id.t option;
    code : string;
    message : string option;
  }

  type result = {
    deleted : deleted list;
    errors : item_error list;
    response : Awskit.Response.t;
  }
end

module Copy = struct
  type metadata_directive = [ `Copy | `Replace of Metadata.t ]

  type options = {
    source_version_id : Version_id.t option;
    source_preconditions : Preconditions.Copy_source.t;
    metadata_directive : metadata_directive option;
    storage_class : Storage_class.t option;
    checksum_algorithm : Checksum.Algorithm.t option;
    destination_encryption : Encryption.Destination.t option;
    source_encryption : Encryption.Source.t option;
    expected_bucket_owner : Account_id.t option;
    source_expected_bucket_owner : Account_id.t option;
  }

  type result = {
    etag : Etag.t option;
    last_modified : Ptime.t option;
    version_id : Version_id.t option;
    copy_source_version_id : Version_id.t option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      source_version_id = None;
      source_preconditions = Preconditions.Copy_source.none;
      metadata_directive = None;
      storage_class = None;
      checksum_algorithm = None;
      destination_encryption = None;
      source_encryption = None;
      expected_bucket_owner = None;
      source_expected_bucket_owner = None;
    }

  let options ?source_version_id
      ?(source_preconditions = Preconditions.Copy_source.none)
      ?metadata_directive ?storage_class ?checksum_algorithm
      ?destination_encryption ?source_encryption ?expected_bucket_owner
      ?source_expected_bucket_owner () =
    {
      source_version_id;
      source_preconditions;
      metadata_directive;
      storage_class;
      checksum_algorithm;
      destination_encryption;
      source_encryption;
      expected_bucket_owner;
      source_expected_bucket_owner;
    }
end

module Versions = struct
  module Delimiter = struct
    include Object_key.Delimiter

    let slash = of_string_exn "/"
  end

  type options = {
    prefix : Object_key.Prefix.t option;
    delimiter : Delimiter.t option;
    max_keys : int option;
    key_marker : Object_key.t option;
    version_id_marker : Version_id.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type object_version = {
    key : Object_key.t;
    version_id : Version_id.t option;
    is_latest : bool option;
    last_modified : Ptime.t option;
    etag : Etag.t option;
    size : int64 option;
    storage_class : Storage_class.t option;
    owner : Owner.t option;
    checksum : Checksum.summary;
  }

  type delete_marker = {
    key : Object_key.t;
    version_id : Version_id.t option;
    is_latest : bool option;
    last_modified : Ptime.t option;
    owner : Owner.t option;
  }

  type page = {
    bucket : Bucket_name.t option;
    prefix : Object_key.Prefix.t option;
    delimiter : Delimiter.t option;
    versions : object_version list;
    delete_markers : delete_marker list;
    common_prefixes : Object_key.Prefix.t list;
    is_truncated : bool;
    key_marker : Object_key.t option;
    version_id_marker : Version_id.t option;
    next_key_marker : Object_key.t option;
    next_version_id_marker : Version_id.t option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      prefix = None;
      delimiter = None;
      max_keys = None;
      key_marker = None;
      version_id_marker = None;
      expected_bucket_owner = None;
    }

  let validate_max_keys = function
    | None -> Ok ()
    | Some value when value >= 0 && value <= 1000 -> Ok ()
    | Some _ ->
        S3_error_context.invalid ~field:"max_keys"
          "max_keys must be between 0 and 1000"

  let validate_markers ~key_marker ~version_id_marker =
    match (key_marker, version_id_marker) with
    | None, Some _ ->
        S3_error_context.invalid ~field:"version_id_marker"
          "version_id_marker requires key_marker"
    | _ -> Ok ()

  let options ?prefix ?delimiter ?max_keys ?key_marker ?version_id_marker
      ?expected_bucket_owner () =
    let* () = validate_max_keys max_keys in
    let* () = validate_markers ~key_marker ~version_id_marker in
    Ok
      {
        prefix;
        delimiter;
        max_keys;
        key_marker;
        version_id_marker;
        expected_bucket_owner;
      }

  let options_exn ?prefix ?delimiter ?max_keys ?key_marker ?version_id_marker
      ?expected_bucket_owner () =
    Awskit.Error.Producer.get_ok_exn
      (options ?prefix ?delimiter ?max_keys ?key_marker ?version_id_marker
         ?expected_bucket_owner ())
end

module List = struct
  module Continuation_token = struct
    type t = string

    let of_string value =
      if value = "" then
        S3_error_context.invalid ~field:"continuation_token"
          "continuation token must be non-empty"
      else if S3_string.has_ctl_or_del value then
        S3_error_context.invalid ~field:"continuation_token"
          "continuation token contains control characters"
      else Ok value

    let of_string_exn value = S3_result.result_exn (of_string value)
    let to_string value = value
    let pp fmt value = Format.pp_print_string fmt value
    let equal = String.equal
  end

  module Delimiter = struct
    include Object_key.Delimiter

    let slash = of_string_exn "/"
  end

  type options = {
    prefix : Object_key.Prefix.t option;
    delimiter : Delimiter.t option;
    max_keys : int option;
    start_after : Object_key.t option;
    continuation_token : Continuation_token.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type object_summary = {
    key : Object_key.t;
    size : int64 option;
    etag : Etag.t option;
    last_modified : Ptime.t option;
    storage_class : Storage_class.t option;
    checksum : Checksum.summary;
  }

  type page = {
    bucket : Bucket_name.t option;
    prefix : Object_key.Prefix.t option;
    delimiter : Delimiter.t option;
    objects : object_summary list;
    common_prefixes : Object_key.Prefix.t list;
    key_count : int option;
    is_truncated : bool;
    continuation_token : Continuation_token.t option;
    next_continuation_token : Continuation_token.t option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      prefix = None;
      delimiter = None;
      max_keys = None;
      start_after = None;
      continuation_token = None;
      expected_bucket_owner = None;
    }

  let validate_max_keys = function
    | None -> Ok ()
    | Some value when value >= 0 && value <= 1000 -> Ok ()
    | Some _ ->
        S3_error_context.invalid ~field:"max_keys"
          "max_keys must be between 0 and 1000"

  let options ?prefix ?delimiter ?max_keys ?start_after ?continuation_token
      ?expected_bucket_owner () =
    let* () = validate_max_keys max_keys in
    Ok
      {
        prefix;
        delimiter;
        max_keys;
        start_after;
        continuation_token;
        expected_bucket_owner;
      }

  let options_exn ?prefix ?delimiter ?max_keys ?start_after ?continuation_token
      ?expected_bucket_owner () =
    Awskit.Error.Producer.get_ok_exn
      (options ?prefix ?delimiter ?max_keys ?start_after ?continuation_token
         ?expected_bucket_owner ())
end

module Tagging = struct
  type result = { tags : Tag.Set.t; response : Awskit.Response.t }
end
