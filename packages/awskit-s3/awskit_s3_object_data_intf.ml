open Awskit_s3_common

module type OBJECT_DATA = sig
  module Etag : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  module Version_id : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  module Checksum : sig
    type algorithm = [ `CRC32 | `CRC32C | `CRC64NVME | `SHA1 | `SHA256 ]
    type request = { algorithm : algorithm; value : string option }
    type response = { algorithm : algorithm; value : string }
  end

  module Encryption : sig
    type kms = { key_id : string option; bucket_key_enabled : bool option }
    type request = [ `AES256 | `Aws_kms of kms ]
    type response = [ `AES256 | `Aws_kms of kms | `Unknown of string ]
  end

  module Etag_condition : sig
    type t = Any | Etag of Etag.t

    val any : t
    val etag : Etag.t -> t
  end

  module Preconditions : sig
    module Write : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
      }

      val none : t
      val if_absent : t
      val if_etag : Etag.t -> t
    end

    module Read : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
        if_modified_since : Ptime.t option;
        if_unmodified_since : Ptime.t option;
      }

      val none : t
    end

    module Delete : sig
      type t = {
        if_match : Etag_condition.t option;
        if_match_last_modified_time : Ptime.t option;
        if_match_size : int64 option;
      }

      val none : t
      val if_etag : Etag.t -> t
    end

    module Copy_source : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
        if_modified_since : Ptime.t option;
        if_unmodified_since : Ptime.t option;
      }

      val none : t
    end
  end

  module Put : sig
    type options = {
      content_type : string option;
      metadata : Metadata.t;
      storage_class : Storage_class.t option;
      tags : Tag.t list;
      cache_control : string option;
      content_encoding : string option;
      content_disposition : string option;
      preconditions : Preconditions.Write.t;
      checksum : Checksum.request option;
      server_side_encryption : Encryption.request option;
    }

    type result = {
      etag : Etag.t option;
      version_id : Version_id.t option;
      checksum : Checksum.response option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Get : sig
    type options = {
      range : Range.t option;
      preconditions : Preconditions.Read.t;
      version_id : Version_id.t option;
    }

    type info = {
      etag : Etag.t option;
      content_type : string option;
      content_length : int64 option;
      last_modified : Ptime.t option;
      metadata : Metadata.t;
      storage_class : Storage_class.t option;
      version_id : Version_id.t option;
      checksum : Checksum.response option;
      server_side_encryption : Encryption.response option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Head : sig
    type options = {
      preconditions : Preconditions.Read.t;
      version_id : Version_id.t option;
    }

    type info = Get.info

    val default_options : options
  end

  module Delete : sig
    type options = {
      preconditions : Preconditions.Delete.t;
      version_id : Version_id.t option;
    }

    type result = {
      delete_marker : bool option;
      version_id : Version_id.t option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Delete_many : sig
    type object_ = {
      key : string;
      version_id : Version_id.t option;
      etag : Etag.t option;
      last_modified_time : Ptime.t option;
      size : int64 option;
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
      request : Awskit.Response.t;
    }
  end

  module Copy : sig
    type metadata_directive = [ `Copy | `Replace of Metadata.t ]

    type options = {
      source_preconditions : Preconditions.Copy_source.t;
      metadata : metadata_directive option;
      storage_class : Storage_class.t option;
      tags : Tag.t list option;
      checksum : Checksum.request option;
      server_side_encryption : Encryption.request option;
    }

    type result = {
      etag : Etag.t option;
      last_modified : Ptime.t option;
      version_id : Version_id.t option;
      copy_source_version_id : Version_id.t option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module List : sig
    type options = {
      prefix : string option;
      delimiter : string option;
      max_keys : int option;
      start_after : string option;
      continuation_token : string option;
    }

    type object_summary = {
      key : string;
      size : int64 option;
      etag : Etag.t option;
      last_modified : Ptime.t option;
      storage_class : Storage_class.t option;
      owner : string option;
      checksums : Checksum.response list;
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
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Tagging : sig
    type result = { tags : Tag.t list; request : Awskit.Response.t }
  end
end
