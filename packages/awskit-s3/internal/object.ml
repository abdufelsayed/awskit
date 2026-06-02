open Common

module Etag = struct
  type t = string

  let of_string value =
    if value = "" then invalid ~field:"etag" "etag must be non-empty"
    else if has_ctl_or_del value then
      invalid ~field:"etag" "etag contains control characters"
    else Ok value

  let of_string_exn value = result_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Version_id = struct
  type t = string

  let of_string value =
    if value = "" then
      invalid ~field:"version_id" "version id must be non-empty"
    else if has_ctl_or_del value then
      invalid ~field:"version_id" "version id contains control characters"
    else Ok value

  let of_string_exn value = result_exn (of_string value)
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
      | Unknown of string

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
      | Unknown value -> value

    let of_string = function
      | "CRC32" -> Crc32
      | "CRC32C" -> Crc32c
      | "CRC64NVME" -> Crc64nvme
      | "SHA1" -> Sha1
      | "SHA256" -> Sha256
      | "SHA512" -> Sha512
      | "MD5" -> Md5
      | "XXHASH64" -> Xxhash64
      | "XXHASH3" -> Xxhash3
      | "XXHASH128" -> Xxhash128
      | value -> Unknown value
  end

  module Type = struct
    type t = Composite | Full_object | Unknown of string

    let to_string = function
      | Composite -> "COMPOSITE"
      | Full_object -> "FULL_OBJECT"
      | Unknown value -> value

    let of_string = function
      | "COMPOSITE" -> Composite
      | "FULL_OBJECT" -> Full_object
      | value -> Unknown value
  end

  module Mode = struct
    type t = Enabled

    let to_string = function Enabled -> "ENABLED"
  end

  type value = { algorithm : Algorithm.t; value : string }
  type response = { values : value list; checksum_type : Type.t option }

  type summary = {
    algorithms : Algorithm.t list;
    checksum_type : Type.t option;
  }

  let empty_response = { values = []; checksum_type = None }
  let empty_summary = { algorithms = []; checksum_type = None }
end

module Encryption = struct
  type kms = { key_id : string option; bucket_key_enabled : bool option }
  type request = [ `AES256 | `Aws_kms of kms ]
  type response = [ `AES256 | `Aws_kms of kms | `Unknown of string ]
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
    content_type : string option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.t list;
    cache_control : string option;
    content_encoding : string option;
    content_disposition : string option;
    preconditions : Preconditions.Write.t;
    checksum : Checksum.value option;
    server_side_encryption : Encryption.request option;
    expected_bucket_owner : string option;
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
      metadata = [];
      storage_class = None;
      tags = [];
      cache_control = None;
      content_encoding = None;
      content_disposition = None;
      preconditions = Preconditions.Write.none;
      checksum = None;
      server_side_encryption = None;
      expected_bucket_owner = None;
    }
end

module Get = struct
  type options = {
    range : Range.t option;
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    expected_bucket_owner : string option;
  }

  type result = {
    etag : Etag.t option;
    content_type : string option;
    content_length : int64 option;
    last_modified : Ptime.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    version_id : Version_id.t option;
    checksum : Checksum.response;
    server_side_encryption : Encryption.response option;
    response : Awskit.Response.t;
  }

  type info = result

  let default_options =
    {
      range = None;
      preconditions = Preconditions.Read.none;
      version_id = None;
      checksum_mode = None;
      expected_bucket_owner = None;
    }
end

module Head = struct
  type options = {
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    expected_bucket_owner : string option;
  }

  type info = Get.result
  type result = info

  let default_options =
    {
      preconditions = Preconditions.Read.none;
      version_id = None;
      checksum_mode = None;
      expected_bucket_owner = None;
    }
end

module Delete = struct
  type options = {
    preconditions : Preconditions.Delete.t;
    version_id : Version_id.t option;
    expected_bucket_owner : string option;
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
end

module Delete_many = struct
  type object_ = {
    key : string;
    version_id : Version_id.t option;
    etag : Etag.t option;
  }

  type deleted = {
    key : string;
    version_id : Version_id.t option;
    delete_marker : bool option;
  }

  type item_error = { key : string; code : string; message : string option }

  type result = {
    deleted : deleted list;
    errors : item_error list;
    response : Awskit.Response.t;
  }

  type options = { expected_bucket_owner : string option }

  let default_options = { expected_bucket_owner = None }
end

module Copy = struct
  type metadata_directive = [ `Copy | `Replace of Metadata.t ]

  type options = {
    source_version_id : Version_id.t option;
    source_preconditions : Preconditions.Copy_source.t;
    metadata_directive : metadata_directive option;
    storage_class : Storage_class.t option;
    checksum_algorithm : Checksum.Algorithm.t option;
    server_side_encryption : Encryption.request option;
    expected_bucket_owner : string option;
    source_expected_bucket_owner : string option;
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
      server_side_encryption = None;
      expected_bucket_owner = None;
      source_expected_bucket_owner = None;
    }
end

module Versions = struct
  type options = {
    prefix : string option;
    delimiter : string option;
    max_keys : int option;
    key_marker : string option;
    version_id_marker : Version_id.t option;
    expected_bucket_owner : string option;
  }

  type object_version = {
    key : string;
    version_id : Version_id.t option;
    is_latest : bool option;
    last_modified : Ptime.t option;
    etag : Etag.t option;
    size : int64 option;
    storage_class : Storage_class.t option;
    owner : string option;
    checksum : Checksum.summary;
  }

  type delete_marker = {
    key : string;
    version_id : Version_id.t option;
    is_latest : bool option;
    last_modified : Ptime.t option;
    owner : string option;
  }

  type page = {
    bucket : string option;
    prefix : string option;
    delimiter : string option;
    versions : object_version list;
    delete_markers : delete_marker list;
    common_prefixes : string list;
    is_truncated : bool;
    key_marker : string option;
    version_id_marker : Version_id.t option;
    next_key_marker : string option;
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
end

module List = struct
  type options = {
    prefix : string option;
    delimiter : string option;
    max_keys : int option;
    start_after : string option;
    continuation_token : string option;
    expected_bucket_owner : string option;
  }

  type object_summary = {
    key : string;
    size : int64 option;
    etag : Etag.t option;
    last_modified : Ptime.t option;
    storage_class : Storage_class.t option;
    checksum : Checksum.summary;
  }

  type page = {
    bucket : string option;
    prefix : string option;
    delimiter : string option;
    objects : object_summary list;
    common_prefixes : string list;
    key_count : int option;
    is_truncated : bool;
    continuation_token : string option;
    next_continuation_token : string option;
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
end

module Tagging = struct
  type options = { expected_bucket_owner : string option }
  type result = { tags : Tag.t list; response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
end
