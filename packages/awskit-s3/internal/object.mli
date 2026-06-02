(** S3 object data types, object-operation options, and object-operation
    results. *)

module Etag : sig
  type t

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Version_id : sig
  type t

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Checksum : sig
  module Algorithm : sig
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

    val to_string : t -> string
    val of_string : string -> t
  end

  module Type : sig
    type t = Composite | Full_object | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  module Mode : sig
    type t = Enabled

    val to_string : t -> string
  end

  type value = { algorithm : Algorithm.t; value : string }
  type response = { values : value list; checksum_type : Type.t option }

  type summary = {
    algorithms : Algorithm.t list;
    checksum_type : Type.t option;
  }
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
    type t = { if_match : Etag_condition.t option }

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

  val default_options : options
end

module Get : sig
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

  val default_options : options
end

module Head : sig
  type options = {
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    expected_bucket_owner : string option;
  }

  type result = Get.result

  val default_options : options
end

module Delete : sig
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

  val default_options : options
end

module Delete_many : sig
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

  val default_options : options
end

module Copy : sig
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

  val default_options : options
end

module Versions : sig
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

  val default_options : options
end

module List : sig
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

  val default_options : options
end

module Tagging : sig
  type options = { expected_bucket_owner : string option }
  type result = { tags : Tag.t list; response : Awskit.Response.t }

  val default_options : options
end
