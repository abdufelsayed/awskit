(** S3 object data types, object-operation options, and object-operation
    results. *)

module Etag : sig
  type t
  (** Opaque S3 ETag value. ETags are returned by S3 and can be used in
      conditional requests; they are not guaranteed to be a plain MD5 digest. *)

  val of_string : string -> (t, Awskit.Error.t) result
  (** Validate and wrap an ETag string. Quoted and unquoted values are accepted
      according to the library's ETag parser. *)

  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Version_id : sig
  type t
  (** Opaque S3 object version id. Present only for versioned buckets or APIs
      that explicitly target a version. *)

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Checksum : sig
  module Algorithm : sig
    (** S3 checksum algorithm names. [Unknown] preserves forward-compatible
        values returned by AWS. *)
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
    (** Render the AWS header/API spelling. *)

    val of_string : string -> t
    (** Parse an AWS checksum algorithm name. Unknown values are preserved. *)
  end

  module Type : sig
    (** Whether an S3 checksum covers the full object or a composite multipart
        checksum. Unknown values are preserved. *)
    type t = Composite | Full_object | Unknown of string

    val to_string : t -> string
    val of_string : string -> t
  end

  module Mode : sig
    (** Request mode for APIs that can ask S3 to return checksum metadata. *)
    type t = Enabled

    val to_string : t -> string
  end

  type value = { algorithm : Algorithm.t; value : string }
  (** Explicit checksum value supplied by the caller or returned for a part. The
      value is the base64/string payload expected by the selected algorithm. *)

  type response = { values : value list; checksum_type : Type.t option }
  (** Modeled checksum headers returned by object and multipart operations. *)

  type summary = {
    algorithms : Algorithm.t list;
    checksum_type : Type.t option;
  }
  (** Compact checksum metadata returned by list operations. *)
end

module Encryption : sig
  type kms = { key_id : string option; bucket_key_enabled : bool option }
  (** Server-side encryption request and response metadata. *)

  type request = [ `AES256 | `Aws_kms of kms ]
  (** Encryption settings that can be sent with write/copy/create requests. *)

  type response = [ `AES256 | `Aws_kms of kms | `Unknown of string ]
  (** Encryption settings reported by S3. Unknown values are preserved. *)
end

module Etag_condition : sig
  (** ETag condition used by object precondition records. *)
  type t = Any | Etag of Etag.t

  val any : t
  (** Match any existing object, rendered as ["*"]. *)

  val etag : Etag.t -> t
  (** Match a specific ETag. *)
end

module Preconditions : sig
  (** Conditional request records. Failed conditions are returned as structured
      service errors, typically [PreconditionFailed] or [NotModified]. *)
  module Write : sig
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
    }
    (** Conditions for object writes. *)

    val none : t
    (** No write preconditions. *)

    val if_absent : t
    (** Require the destination key to be absent. *)

    val if_etag : Etag.t -> t
    (** Require the destination object to match an ETag. *)
  end

  module Read : sig
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
      if_modified_since : Ptime.t option;
      if_unmodified_since : Ptime.t option;
    }
    (** Conditions for object reads and heads. *)

    val none : t
    (** No read preconditions. *)
  end

  module Delete : sig
    type t = { if_match : Etag_condition.t option }
    (** Conditions for object deletes. *)

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
    (** Conditions applied to the source object in copy requests. *)

    val none : t
    (** No source-object preconditions. *)
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
  (** [PutObject] request options and result metadata. *)

  type result = {
    etag : Etag.t option;
    version_id : Version_id.t option;
    checksum : Checksum.response;
    response : Awskit.Response.t;
  }

  val default_options : options
  (** Default [PutObject] options: no optional headers, tags, checksum, or
      preconditions. *)
end

module Get : sig
  type options = {
    range : Range.t option;
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    expected_bucket_owner : string option;
  }
  (** [GetObject] request options and response metadata. The object body is
      consumed through the selected runtime and is not stored in [result]. *)

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
  (** Default [GetObject] options: full current object, no checksum mode, and no
      conditional headers. *)
end

module Head : sig
  type options = {
    preconditions : Preconditions.Read.t;
    version_id : Version_id.t option;
    checksum_mode : Checksum.Mode.t option;
    expected_bucket_owner : string option;
  }
  (** [HeadObject] request options and response metadata. *)

  type result = Get.result

  val default_options : options
end

module Delete : sig
  type options = {
    preconditions : Preconditions.Delete.t;
    version_id : Version_id.t option;
    expected_bucket_owner : string option;
  }
  (** [DeleteObject] request options and result metadata. *)

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
  (** [DeleteObjects] request and result data. *)

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
  (** [CopyObject] request options and result metadata. *)
  type metadata_directive = [ `Copy | `Replace of Metadata.t ]
  (** [`Copy] preserves source metadata. [`Replace metadata] writes new user
      metadata on the destination object. *)

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
  (** [ListObjectVersions] options and page data. *)

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
  (** [ListObjectsV2] options and page data. *)

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
  (** Object tagging request options and result data. *)

  type result = { tags : Tag.t list; response : Awskit.Response.t }

  val default_options : options
end
