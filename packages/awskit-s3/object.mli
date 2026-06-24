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
  (** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_string : t -> string
  (** Render the normalized ETag value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an ETag. *)

  val equal : t -> t -> bool
  (** Compare two ETags. *)
end

module Version_id : sig
  type t
  (** Opaque S3 object version id. Present only for versioned buckets or APIs
      that explicitly target a version. *)

  val of_string : string -> (t, Awskit.Error.t) result
  (** Validate and wrap a version id. Empty version ids are rejected. *)

  val of_string_exn : string -> t
  (** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_string : t -> string
  (** Return the version id string. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print a version id. *)

  val equal : t -> t -> bool
  (** Compare two version ids. *)
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

  type value = private { algorithm : Algorithm.t; value : string }
  (** Explicit checksum value supplied by the caller or returned for a part. The
      value is the base64/string payload expected by the selected algorithm.
      Values remain inspectable, but use {!val:value} to construct outbound
      request checksums so unknown response-only algorithms cannot be sent. *)

  val value :
    algorithm:Algorithm.t -> value:string -> (value, Awskit.Error.t) result
  (** Validate and wrap an explicit outbound checksum value. Unknown algorithms
      are rejected because they cannot be rendered safely in request headers. *)

  val value_exn : algorithm:Algorithm.t -> value:string -> value
  (** Like {!val:value}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  type response = { values : value list; checksum_type : Type.t option }
  (** Modeled checksum headers returned by object and multipart operations. *)

  type summary = {
    algorithms : Algorithm.t list;
    checksum_type : Type.t option;
  }
  (** Compact checksum metadata returned by list operations. *)

  val empty_summary : summary
  (** Empty checksum summary for responses that do not include checksum
      metadata. *)
end

module Encryption : sig
  type kms = { key_id : string option; bucket_key_enabled : bool option }
  (** Server-side encryption request and response metadata. *)

  type request = [ `AES256 | `Aws_kms of kms ]
  (** Encryption settings that can be sent with write/copy/create requests. *)

  type response = [ `AES256 | `Aws_kms of kms | `Unknown of string ]
  (** Encryption settings reported by S3. Unknown values are preserved. *)
end

module Owner : sig
  type t = private { id : string option; display_name : string option }
  (** S3 owner metadata returned by listing APIs. [id] is the canonical owner
      identifier when S3 returns it. [display_name] is optional and is not
      returned by all S3 regions. *)

  val create : ?id:string -> ?display_name:string -> unit -> t option
  (** Preserve a returned owner when S3 supplied at least one field. Empty
      fields are treated as absent. *)
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
    content_type : Content_type.t option;
        (** [Content-Type] header for the object. *)
    metadata : Metadata.t;  (** User metadata sent as [x-amz-meta-*] headers. *)
    storage_class : Storage_class.t option;
        (** Optional S3 storage class for the new object. *)
    tags : Tag.Set.t;  (** Object tags sent with the write request. *)
    cache_control : Header_value.t option;
        (** Optional [Cache-Control] header. *)
    content_encoding : Header_value.t option;
        (** Optional [Content-Encoding] header. *)
    content_disposition : Header_value.t option;
        (** Optional [Content-Disposition] header. *)
    preconditions : Preconditions.Write.t;  (** Conditional write headers. *)
    checksum : Checksum.value option;
        (** Explicit checksum header for the request body. *)
    server_side_encryption : Encryption.request option;
        (** Server-side encryption headers for the new object. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner], used to guard against bucket-owner
            confusion. *)
  }
  (** [PutObject] request options. *)

  type result = {
    etag : Etag.t option;  (** ETag returned by S3, when present. *)
    version_id : Version_id.t option;
        (** New object version id for versioned buckets. *)
    checksum : Checksum.response;  (** Checksum headers returned by S3. *)
    response : Awskit.Response.t;
        (** Raw response metadata, including status and request ids. *)
  }
  (** [PutObject] result metadata. The uploaded body is not retained. *)

  val default_options : options
  (** Default [PutObject] options: no optional headers, tags, checksum, or
      preconditions. *)

  val options :
    ?content_type:Content_type.t ->
    ?metadata:Metadata.t ->
    ?storage_class:Storage_class.t ->
    ?tags:Tag.Set.t ->
    ?cache_control:Header_value.t ->
    ?content_encoding:Header_value.t ->
    ?content_disposition:Header_value.t ->
    ?preconditions:Preconditions.Write.t ->
    ?checksum:Checksum.value ->
    ?server_side_encryption:Encryption.request ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [PutObject] options. *)

  val options_exn :
    ?content_type:Content_type.t ->
    ?metadata:Metadata.t ->
    ?storage_class:Storage_class.t ->
    ?tags:Tag.Set.t ->
    ?cache_control:Header_value.t ->
    ?content_encoding:Header_value.t ->
    ?content_disposition:Header_value.t ->
    ?preconditions:Preconditions.Write.t ->
    ?checksum:Checksum.value ->
    ?server_side_encryption:Encryption.request ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Get : sig
  type options = {
    range : Range.t option;  (** Optional HTTP byte range. *)
    preconditions : Preconditions.Read.t;  (** Conditional read headers. *)
    version_id : Version_id.t option;
        (** Object version to read instead of the current version. *)
    checksum_mode : Checksum.Mode.t option;
        (** Request S3 checksum metadata in the response. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [GetObject] request options. *)

  type info = {
    etag : Etag.t option;  (** Object ETag returned by S3. *)
    content_type : Content_type.t option;
        (** Object [Content-Type] response header. *)
    content_length : int64 option;
        (** Number of response-body bytes when S3 supplied [Content-Length]. *)
    content_range : Range.Content_range.t option;
        (** Parsed [Content-Range] response header for ranged responses. *)
    last_modified : Ptime.t option;
        (** Last modified timestamp parsed from the response. *)
    metadata : Metadata.t;
        (** User metadata parsed from [x-amz-meta-*] headers. *)
    storage_class : Storage_class.t option;
        (** Storage class reported for the object, if present. *)
    version_id : Version_id.t option;  (** Version id of the returned object. *)
    checksum : Checksum.response;  (** Checksum response headers. *)
    server_side_encryption : Encryption.response option;
        (** Server-side encryption metadata reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [GetObject] response metadata. The object body is consumed through the
      selected runtime and is not stored in [info]. *)

  type 'a result = {
    value : 'a;  (** Value returned by the response-body consumer. *)
    etag : Etag.t option;  (** Object ETag returned by S3. *)
    content_type : Content_type.t option;
        (** Object [Content-Type] response header. *)
    content_length : int64 option;
        (** Number of response-body bytes when S3 supplied [Content-Length]. *)
    content_range : Range.Content_range.t option;
        (** Parsed [Content-Range] response header for ranged responses. *)
    last_modified : Ptime.t option;
        (** Last modified timestamp parsed from the response. *)
    metadata : Metadata.t;
        (** User metadata parsed from [x-amz-meta-*] headers. *)
    storage_class : Storage_class.t option;
        (** Storage class reported for the object, if present. *)
    version_id : Version_id.t option;  (** Version id of the returned object. *)
    checksum : Checksum.response;  (** Checksum response headers. *)
    server_side_encryption : Encryption.response option;
        (** Server-side encryption metadata reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [GetObject] result containing response metadata and the caller's consumed
      body value. *)

  val default_options : options
  (** Default [GetObject] options: full current object, no checksum mode, and no
      conditional headers. *)

  val options :
    ?range:Range.t ->
    ?preconditions:Preconditions.Read.t ->
    ?version_id:Version_id.t ->
    ?checksum_mode:Checksum.Mode.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [GetObject] options. *)

  val options_exn :
    ?range:Range.t ->
    ?preconditions:Preconditions.Read.t ->
    ?version_id:Version_id.t ->
    ?checksum_mode:Checksum.Mode.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Head : sig
  type options = {
    preconditions : Preconditions.Read.t;
        (** Conditional headers for the object metadata request. *)
    version_id : Version_id.t option;
        (** Object version to inspect instead of the current version. *)
    checksum_mode : Checksum.Mode.t option;
        (** Request S3 checksum metadata in the response. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [HeadObject] request options. *)

  type result = Get.info
  (** [HeadObject] metadata has the same shape as {!type:Get.info}; no object
      body is returned. *)

  val default_options : options

  val options :
    ?preconditions:Preconditions.Read.t ->
    ?version_id:Version_id.t ->
    ?checksum_mode:Checksum.Mode.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [HeadObject] options. *)

  val options_exn :
    ?preconditions:Preconditions.Read.t ->
    ?version_id:Version_id.t ->
    ?checksum_mode:Checksum.Mode.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Delete : sig
  type options = {
    preconditions : Preconditions.Delete.t;  (** Conditional delete headers. *)
    version_id : Version_id.t option;
        (** Object version to delete instead of the current version. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [DeleteObject] request options. *)

  type result = {
    delete_marker : bool option;
        (** Whether S3 created or addressed a delete marker. *)
    version_id : Version_id.t option;
        (** Version id affected by the delete operation. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [DeleteObject] result metadata. *)

  val default_options : options

  val options :
    ?preconditions:Preconditions.Delete.t ->
    ?version_id:Version_id.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [DeleteObject] options. *)

  val options_exn :
    ?preconditions:Preconditions.Delete.t ->
    ?version_id:Version_id.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Delete_many : sig
  val max_objects : int
  (** Maximum number of objects accepted by one [DeleteObjects] request. *)

  type object_ = {
    key : Object_key.t;  (** Object key to delete. *)
    version_id : Version_id.t option;  (** Optional version id to delete. *)
    etag : Etag.t option;
        (** Optional ETag condition for conditional delete support. *)
  }
  (** One [DeleteObjects] request member. *)

  val object_ :
    key:Object_key.t ->
    ?version_id:Version_id.t ->
    ?etag:Etag.t ->
    unit ->
    object_
  (** Build one [DeleteObjects] request member. *)

  type deleted = {
    key : Object_key.t;  (** Deleted key reported by S3. *)
    version_id : Version_id.t option;  (** Deleted version id, when present. *)
    delete_marker : bool option;  (** Delete-marker flag reported by S3. *)
    delete_marker_version_id : Version_id.t option;
        (** Delete-marker version id, when S3 reports one. *)
  }
  (** One successful [DeleteObjects] member result. *)

  type item_error = {
    key : Object_key.t;
    code : string;
    message : string option;
  }
  (** Per-object [DeleteObjects] failure returned inside an otherwise decoded
      response. *)

  type result = {
    deleted : deleted list;  (** Successfully deleted members. *)
    errors : item_error list;  (** Per-member failures reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [DeleteObjects] result data. Check [errors] even when the operation itself
      returned [Ok]. *)

  type options = { expected_bucket_owner : Account_id.t option }
  (** [DeleteObjects] request options. *)

  val default_options : options

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [DeleteObjects] options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Copy : sig
  (** [CopyObject] request options and result metadata. *)
  type metadata_directive = [ `Copy | `Replace of Metadata.t ]
  (** [`Copy] preserves source metadata. [`Replace metadata] writes new user
      metadata on the destination object. *)

  type options = {
    source_version_id : Version_id.t option;
        (** Source object version to copy. *)
    source_preconditions : Preconditions.Copy_source.t;
        (** Preconditions applied to the source object. *)
    metadata_directive : metadata_directive option;
        (** Whether to copy source metadata or replace it. *)
    storage_class : Storage_class.t option;
        (** Storage class for the destination object. *)
    checksum_algorithm : Checksum.Algorithm.t option;
        (** Checksum algorithm S3 should use for the destination object. *)
    server_side_encryption : Encryption.request option;
        (** Server-side encryption for the destination object. *)
    expected_bucket_owner : Account_id.t option;
        (** Expected owner for the destination bucket. *)
    source_expected_bucket_owner : Account_id.t option;
        (** Expected owner for the source bucket. *)
  }
  (** [CopyObject] request options. *)

  type result = {
    etag : Etag.t option;  (** Destination object ETag. *)
    last_modified : Ptime.t option;
        (** Destination last-modified timestamp returned in the copy payload. *)
    version_id : Version_id.t option;  (** Destination object version id. *)
    copy_source_version_id : Version_id.t option;
        (** Source version id reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [CopyObject] result metadata. *)

  val default_options : options

  val options :
    ?source_version_id:Version_id.t ->
    ?source_preconditions:Preconditions.Copy_source.t ->
    ?metadata_directive:metadata_directive ->
    ?storage_class:Storage_class.t ->
    ?checksum_algorithm:Checksum.Algorithm.t ->
    ?server_side_encryption:Encryption.request ->
    ?expected_bucket_owner:Account_id.t ->
    ?source_expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [CopyObject] options. *)

  val options_exn :
    ?source_version_id:Version_id.t ->
    ?source_preconditions:Preconditions.Copy_source.t ->
    ?metadata_directive:metadata_directive ->
    ?storage_class:Storage_class.t ->
    ?checksum_algorithm:Checksum.Algorithm.t ->
    ?server_side_encryption:Encryption.request ->
    ?expected_bucket_owner:Account_id.t ->
    ?source_expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Versions : sig
  module Delimiter : sig
    type t = Object_key.Delimiter.t
    (** Object-version listing delimiter. *)

    val slash : t
    (** Slash delimiter, the common S3 path-like grouping delimiter. *)

    val of_string : string -> (t, Awskit.Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  type options = {
    prefix : Object_key.Prefix.t option;
        (** Return versions whose keys begin with this prefix. *)
    delimiter : Delimiter.t option;
        (** Group keys using this delimiter, commonly ["/"]. *)
    max_keys : int option;
        (** Maximum number of keys/markers S3 should return in one page. *)
    key_marker : Object_key.t option;
        (** Pagination marker for keys. Usually supplied from the previous
            page's [next_key_marker]. *)
    version_id_marker : Version_id.t option;
        (** Pagination marker for versions. Usually supplied with [key_marker].
        *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [ListObjectVersions] request options. *)

  type object_version = {
    key : Object_key.t;  (** Object key. *)
    version_id : Version_id.t option;  (** Object version id. *)
    is_latest : bool option;
        (** Whether this entry is the latest version for the key. *)
    last_modified : Ptime.t option;  (** Last modified timestamp. *)
    etag : Etag.t option;  (** Version ETag. *)
    size : int64 option;  (** Object size in bytes. *)
    storage_class : Storage_class.t option;
        (** Storage class for this version. *)
    owner : Owner.t option;  (** Owner metadata when returned by S3. *)
    checksum : Checksum.summary;  (** Checksum summary metadata. *)
  }
  (** One object version entry from [ListObjectVersions]. *)

  type delete_marker = {
    key : Object_key.t;  (** Object key. *)
    version_id : Version_id.t option;  (** Delete marker version id. *)
    is_latest : bool option;
        (** Whether this delete marker is latest for the key. *)
    last_modified : Ptime.t option;  (** Delete marker timestamp. *)
    owner : Owner.t option;  (** Owner metadata when returned by S3. *)
  }
  (** One delete marker entry from [ListObjectVersions]. *)

  type page = {
    bucket : Bucket_name.t option;  (** Bucket name echoed by S3. *)
    prefix : Object_key.Prefix.t option;  (** Prefix applied to this page. *)
    delimiter : Delimiter.t option;  (** Delimiter applied to this page. *)
    versions : object_version list;  (** Object versions in this page. *)
    delete_markers : delete_marker list;  (** Delete markers in this page. *)
    common_prefixes : Object_key.Prefix.t list;
        (** Grouped prefixes returned when [delimiter] is set. *)
    is_truncated : bool;  (** Whether more pages are available. *)
    key_marker : Object_key.t option;  (** Current page key marker. *)
    version_id_marker : Version_id.t option;
        (** Current page version marker. *)
    next_key_marker : Object_key.t option;
        (** Marker to use for the next page. *)
    next_version_id_marker : Version_id.t option;
        (** Version marker to use for the next page. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** One [ListObjectVersions] page. *)

  val default_options : options

  val options :
    ?prefix:Object_key.Prefix.t ->
    ?delimiter:Delimiter.t ->
    ?max_keys:int ->
    ?key_marker:Object_key.t ->
    ?version_id_marker:Version_id.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [ListObjectVersions] options. *)

  val options_exn :
    ?prefix:Object_key.Prefix.t ->
    ?delimiter:Delimiter.t ->
    ?max_keys:int ->
    ?key_marker:Object_key.t ->
    ?version_id_marker:Version_id.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module List : sig
  module Continuation_token : sig
    type t
    (** Opaque [ListObjectsV2] continuation token returned by S3.

        Tokens are service values. Callers may store and pass them back, but
        should not parse them. *)

    val of_string : string -> (t, Awskit.Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  module Delimiter : sig
    type t = Object_key.Delimiter.t
    (** Object listing delimiter. *)

    val slash : t
    (** Slash delimiter, the common S3 path-like grouping delimiter. *)

    val of_string : string -> (t, Awskit.Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  type options = {
    prefix : Object_key.Prefix.t option;
        (** Return keys that begin with this prefix. *)
    delimiter : Delimiter.t option;
        (** Group keys using this delimiter, commonly ["/"]. *)
    max_keys : int option;
        (** Maximum number of objects S3 should return in one page. *)
    start_after : Object_key.t option;
        (** Start listing after this key for the first page. *)
    continuation_token : Continuation_token.t option;
        (** Pagination token, usually supplied from a previous page. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [ListObjectsV2] request options. *)

  type object_summary = {
    key : Object_key.t;  (** Object key. *)
    size : int64 option;  (** Object size in bytes. *)
    etag : Etag.t option;  (** Object ETag. *)
    last_modified : Ptime.t option;  (** Last modified timestamp. *)
    storage_class : Storage_class.t option;  (** Object storage class. *)
    checksum : Checksum.summary;  (** Checksum summary metadata. *)
  }
  (** One object summary from a listing page. *)

  type page = {
    bucket : Bucket_name.t option;  (** Bucket name echoed by S3. *)
    prefix : Object_key.Prefix.t option;  (** Prefix applied to this page. *)
    delimiter : Delimiter.t option;  (** Delimiter applied to this page. *)
    objects : object_summary list;  (** Object summaries in this page. *)
    common_prefixes : Object_key.Prefix.t list;
        (** Grouped prefixes returned when [delimiter] is set. *)
    key_count : int option;  (** Number of keys S3 reports in this page. *)
    is_truncated : bool;  (** Whether more pages are available. *)
    continuation_token : Continuation_token.t option;
        (** Token used to request this page. *)
    next_continuation_token : Continuation_token.t option;
        (** Token to use for the next page. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** One [ListObjectsV2] page. *)

  val default_options : options

  val options :
    ?prefix:Object_key.Prefix.t ->
    ?delimiter:Delimiter.t ->
    ?max_keys:int ->
    ?start_after:Object_key.t ->
    ?continuation_token:Continuation_token.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [ListObjectsV2] options. *)

  val options_exn :
    ?prefix:Object_key.Prefix.t ->
    ?delimiter:Delimiter.t ->
    ?max_keys:int ->
    ?start_after:Object_key.t ->
    ?continuation_token:Continuation_token.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module Tagging : sig
  type options = { expected_bucket_owner : Account_id.t option }
  (** Object tagging request options. *)

  type result = { tags : Tag.Set.t; response : Awskit.Response.t }
  (** Object tag set returned by [GetObjectTagging]. *)

  val default_options : options

  val options :
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build object tagging request options. *)

  val options_exn : ?expected_bucket_owner:Account_id.t -> unit -> options
  (** Like {!val:options}, but raises on validation failure. *)
end
