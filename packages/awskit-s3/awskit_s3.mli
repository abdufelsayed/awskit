(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for classic AWS S3 bucket/object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned URLs, runtime-backed clients, and the in-memory simulator used by
    tests.

    Implementation modules, XML codecs, request builders, and parser helpers are
    private to the package. *)

module Credentials = Awskit.Credentials
(** AWS credentials for request signing, re-exported for convenience. *)

module Endpoint = Awskit.Endpoint
(** Endpoint values used by the AWS endpoint resolver. *)

module Region = Awskit.Region
(** AWS region values. *)

(** S3 classifiers over structured {!Awskit.Error.t} values. *)
module Error : sig
  type t = Awskit.Error.t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val to_string_hum : t -> string
  val service_code : t -> string option
  val is_not_found : t -> bool
  val is_no_such_bucket : t -> bool
  val is_no_such_key : t -> bool
  val is_precondition_failed : t -> bool
end

(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)
module Metadata : sig
  type t = Awskit_s3_common.Metadata.t
end

(** Classic S3 object storage classes. *)
module Storage_class : sig
  type t = Awskit_s3_common.Storage_class.t =
    | Standard
    | Standard_ia
    | Onezone_ia
    | Intelligent_tiering
    | Glacier
    | Glacier_ir
    | Deep_archive
    | Express_onezone

  val to_string : t -> string
  val of_string : string -> t option
end

(** S3 tag key/value pair. *)
module Tag : sig
  type t = Awskit_s3_common.Tag.t = { key : string; value : string }
end

(** HTTP byte-range requests for S3 object reads. *)
module Range : sig
  type t = Awskit_s3_common.Range.t

  val bytes : start:int64 -> finish:int64 -> (t, Error.t) result
  val bytes_exn : start:int64 -> finish:int64 -> t
  val from : int64 -> (t, Error.t) result
  val from_exn : int64 -> t
  val suffix : int64 -> (t, Error.t) result
  val suffix_exn : int64 -> t
  val to_header : t -> string
end

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
(** S3 bucket addressing style. [`Auto] uses virtual-hosted addressing when the
    bucket name is safe for the selected endpoint and path-style otherwise. *)

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]
(** AWS S3 endpoint variant. Ignored when an explicit [endpoint] is supplied. *)

type endpoint_config = Awskit_s3_endpoint.t
(** Opaque S3 endpoint and addressing configuration for custom runtimes. Most
    callers pass endpoint options directly to an adapter [create] function. *)

val endpoint_config :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Endpoint.Scheme.t ->
  ?endpoint:Endpoint.t ->
  unit ->
  endpoint_config
(** Build endpoint configuration for custom runtimes. *)

val default_endpoint_config : endpoint_config
(** Default AWS regional HTTPS endpoint configuration. *)

(** Object data types and option records. *)
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
    (** S3 object checksum algorithms accepted by object and multipart
        operations. Requests may carry a precomputed [value], or only an
        [algorithm] when AWS should calculate the checksum. Responses contain
        checksum values returned by S3. *)

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
      source_version_id : Version_id.t option;
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

  module Versions : sig
    type options = {
      prefix : string option;
      delimiter : string option;
      max_keys : int option;
      key_marker : string option;
      version_id_marker : Version_id.t option;
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

module Object : OBJECT_DATA

(** Bucket data types and configuration records. *)
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

module Bucket : BUCKET_DATA

(** Multipart upload data types. *)
module type MULTIPART_DATA = sig
  module Upload_id : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
  end

  module Upload : sig
    type t = private { bucket : string; key : string; upload_id : Upload_id.t }

    val create :
      bucket:string ->
      key:string ->
      upload_id:Upload_id.t ->
      (t, Error.t) result

    val create_exn : bucket:string -> key:string -> upload_id:Upload_id.t -> t
  end

  module Part : sig
    type t = private { part_number : int; etag : Object.Etag.t }

    val create : part_number:int -> etag:Object.Etag.t -> (t, Error.t) result
    val create_exn : part_number:int -> etag:Object.Etag.t -> t
  end

  module Create : sig
    type options = {
      content_type : string option;
      metadata : Metadata.t;
      storage_class : Storage_class.t option;
      tags : Tag.t list;
      checksum : Object.Checksum.request option;
      server_side_encryption : Object.Encryption.request option;
    }

    type result = { upload : Upload.t; request : Awskit.Response.t }

    val default_options : options
  end

  module Upload_part : sig
    type options = { checksum : Object.Checksum.request option }

    type result = {
      part : Part.t;
      checksum : Object.Checksum.response option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Complete : sig
    type result = {
      etag : Object.Etag.t option;
      version_id : Object.Version_id.t option;
      checksum : Object.Checksum.response option;
      request : Awskit.Response.t;
    }
  end

  module List_parts : sig
    type options = { max_parts : int option; part_number_marker : int option }

    type part_info = {
      part_number : int;
      etag : Object.Etag.t option;
      size : int64 option;
      last_modified : Ptime.t option;
      checksum : Object.Checksum.response option;
    }

    type page = {
      parts : part_info list;
      is_truncated : bool;
      next_part_number_marker : int option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Managed : sig
    val min_part_size : int
    val default_part_size : int
    val max_parts : int

    type options = {
      part_size : int;
      create_options : Create.options;
      upload_part_options : Upload_part.options;
    }

    type result = {
      upload : Upload.t;
      parts : Part.t list;
      complete : Complete.result;
    }

    val default_options : options
    val validate_options : options -> (unit, Error.t) Stdlib.result
  end
end

module Multipart : MULTIPART_DATA

(** Opaque validated bucket-policy JSON payloads. *)
module type POLICY = sig
  type t

  val of_json : string -> (t, Error.t) result
  val to_json : t -> string
end

module Policy : POLICY

(** Standalone S3 presigned URL generation. *)
module type PRESIGNED_DATA = sig
  type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

  type endpoint_variant =
    [ `Regional
    | `Dualstack
    | `Fips
    | `Fips_dualstack
    | `Accelerate
    | `Accelerate_dualstack ]

  type method_ = [ `GET | `PUT | `HEAD | `DELETE ]

  type result = {
    url : string;
    method_ : method_;
    headers : (string * string) list;
    expires_at : Ptime.t option;
  }

  module Put_object : sig
    type options = {
      expires_in : Ptime.Span.t option;
      content_type : string option;
      checksum : Object.Checksum.request option;
      server_side_encryption : Object.Encryption.request option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  module Get_object : sig
    type options = {
      expires_in : Ptime.Span.t option;
      response_content_type : string option;
      response_content_disposition : string option;
      version_id : Object.Version_id.t option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  module Upload_part : sig
    type options = {
      expires_in : Ptime.Span.t option;
      checksum : Object.Checksum.request option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  val get_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val put_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val head_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val delete_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?expires_in:Ptime.Span.t ->
    unit ->
    (result, Error.t) Stdlib.result

  val upload_part :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Upload_part.options ->
    unit ->
    (result, Error.t) Stdlib.result
end

module Presigned : PRESIGNED_DATA

module type RUNTIME = sig
  include Awskit.Runtime.S

  val s3_endpoint_config : connection -> endpoint_config
end

module type OBJECT = sig
  type connection
  type +'a io
  type upload_body
  type download_reader

  val put :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Put.options ->
    body:upload_body ->
    unit ->
    (Object.Put.result, Error.t) result io

  val get :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Get.options ->
    consume:(download_reader -> ('a, Error.t) result io) ->
    unit ->
    (Object.Get.info * 'a, Error.t) result io

  val head :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.info, Error.t) result io

  val exists :
    connection -> bucket:string -> key:string -> (bool, Error.t) result io

  val delete :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Delete.options ->
    unit ->
    (Object.Delete.result, Error.t) result io

  val delete_many :
    connection ->
    bucket:string ->
    objects:Object.Delete_many.object_ list ->
    (Object.Delete_many.result, Error.t) result io

  val copy :
    connection ->
    src_bucket:string ->
    src_key:string ->
    dst_bucket:string ->
    dst_key:string ->
    ?options:Object.Copy.options ->
    unit ->
    (Object.Copy.result, Error.t) result io

  val list_versions :
    connection ->
    bucket:string ->
    ?options:Object.Versions.options ->
    unit ->
    (Object.Versions.page, Error.t) result io

  val list :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (Object.List.page, Error.t) result io

  val list_keys :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (string list, Error.t) result io

  module Paginator : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.List.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.page list, Error.t) result io

    val objects :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.object_summary list, Error.t) result io

    val keys :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (string list, Error.t) result io
  end

  module Versions : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.Versions.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.page list, Error.t) result io

    val object_versions :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.object_version list, Error.t) result io

    val delete_markers :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.delete_marker list, Error.t) result io
  end

  module Buffer : sig
    val put_string :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Put.options ->
      string ->
      (Object.Put.result, Error.t) result io

    val put_bytes :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Put.options ->
      bytes ->
      (Object.Put.result, Error.t) result io

    val get_string :
      connection ->
      bucket:string ->
      key:string ->
      max_size:int64 ->
      ?options:Object.Get.options ->
      unit ->
      (Object.Get.info * string, Error.t) result io

    val get_bytes :
      connection ->
      bucket:string ->
      key:string ->
      max_size:int64 ->
      ?options:Object.Get.options ->
      unit ->
      (Object.Get.info * bytes, Error.t) result io
  end

  module Tagging : sig
    val get :
      connection ->
      bucket:string ->
      key:string ->
      (Object.Tagging.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      key:string ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection ->
      bucket:string ->
      key:string ->
      (Awskit.Response.t, Error.t) result io
  end
end

module type BUCKET = sig
  type connection
  type +'a io

  val create :
    connection ->
    bucket:string ->
    ?options:Bucket.Create.options ->
    unit ->
    (Bucket.Create.result, Error.t) result io

  val delete :
    connection -> bucket:string -> (Bucket.Delete.result, Error.t) result io

  val head :
    connection -> bucket:string -> (Bucket.Head.info, Error.t) result io

  val exists : connection -> bucket:string -> (bool, Error.t) result io
  val list : connection -> (Bucket.info list, Error.t) result io

  val get_location :
    connection -> bucket:string -> (Awskit.Region.t option, Error.t) result io

  module Policy : sig
    val get : connection -> bucket:string -> (Policy.t, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Policy.t ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Policy_status : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Policy_status.result, Error.t) result io
  end

  module Versioning : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Versioning.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Versioning.Status.t ->
      (Awskit.Response.t, Error.t) result io
  end

  module Tagging : sig
    val get :
      connection -> bucket:string -> (Bucket.Tagging.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Encryption : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Encryption.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Encryption.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Cors : sig
    val get :
      connection -> bucket:string -> (Bucket.Cors.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Cors.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Website : sig
    val get :
      connection -> bucket:string -> (Bucket.Website.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Website.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Public_access_block : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Public_access_block.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Public_access_block.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Ownership_controls : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Ownership_controls.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Ownership_controls.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection -> bucket:string -> (Awskit.Response.t, Error.t) result io
  end

  module Request_payment : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Request_payment.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Request_payment.Payer.t ->
      (Awskit.Response.t, Error.t) result io
  end

  module Accelerate : sig
    val get :
      connection ->
      bucket:string ->
      (Bucket.Accelerate.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Accelerate.Status.t ->
      (Awskit.Response.t, Error.t) result io
  end

  module Logging : sig
    val get :
      connection -> bucket:string -> (Bucket.Logging.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      Bucket.Logging.config ->
      (Awskit.Response.t, Error.t) result io
  end
end

module type MULTIPART = sig
  type connection
  type +'a io
  type upload_body

  val create :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Multipart.Create.options ->
    unit ->
    (Multipart.Create.result, Error.t) result io

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    body:upload_body ->
    ?options:Multipart.Upload_part.options ->
    unit ->
    (Multipart.Upload_part.result, Error.t) result io

  val complete :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    Multipart.Part.t list ->
    (Multipart.Complete.result, Error.t) result io

  val abort :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    (Awskit.Response.t, Error.t) result io

  val list_parts :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.List_parts.options ->
    unit ->
    (Multipart.List_parts.page, Error.t) result io

  module Paginator : sig
    val fold_pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Multipart.List_parts.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.page list, Error.t) result io

    val parts :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.part_info list, Error.t) result io
  end

  module Managed : sig
    val upload_string :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Multipart.Managed.options ->
      string ->
      (Multipart.Managed.result, Error.t) result io

    val upload_bytes :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Multipart.Managed.options ->
      bytes ->
      (Multipart.Managed.result, Error.t) result io
  end
end

module type PRESIGNED = sig
  type connection
  type +'a io

  val get_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Error.t) result io

  val put_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Error.t) result io

  val head_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Error.t) result io

  val delete_object :
    connection ->
    bucket:string ->
    key:string ->
    ?expires_in:Ptime.Span.t ->
    unit ->
    (Presigned.result, Error.t) result io

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Error.t) result io
end

module type S = sig
  type connection
  type +'a io
  type upload_body
  type download_reader

  module Object :
    OBJECT
      with type connection = connection
       and type 'a io = 'a io
       and type upload_body = upload_body
       and type download_reader = download_reader

  module Bucket :
    BUCKET with type connection = connection and type 'a io = 'a io

  module Multipart :
    MULTIPART
      with type connection = connection
       and type 'a io = 'a io
       and type upload_body = upload_body

  module Presigned :
    PRESIGNED with type connection = connection and type 'a io = 'a io
end

(** Build an S3 client from a runtime implementation. *)
module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type upload_body = R.upload_body
     and type download_reader = R.download_reader

(** In-memory S3 simulator for contract tests. *)
module Sim : sig
  module Clock : sig
    type t

    val create : ?now:Ptime.t -> unit -> t
    val now : t -> Ptime.t
    val advance : t -> Ptime.Span.t -> unit
    val advance_ms : t -> int -> unit
  end

  type config = { max_list_keys : int }

  val default_config : config

  type store

  val create_store : ?config:config -> clock:Clock.t -> unit -> store

  type t

  val connect : store -> credentials:Awskit.Credentials.t -> t
  val store : t -> store

  module Runtime : RUNTIME with type 'a t = 'a and type connection = t

  type fault = Slow_down | Internal_error | Connection_reset | Response_lost

  val inject_fault : t -> fault -> unit
  val inject_faults : t -> fault list -> unit
  val clear_faults : t -> unit
  val enable_buggify : t -> seed:int -> prob:float -> unit
  val disable_buggify : t -> unit

  type op_record = {
    op :
      [ `Put
      | `Get
      | `Head
      | `Delete
      | `List
      | `List_versions
      | `Copy
      | `Delete_many
      | `Multipart_create
      | `Multipart_upload_part
      | `Multipart_complete
      | `Multipart_abort
      | `Multipart_list_parts ];
    bucket : string;
    key : string option;
    timestamp : Ptime.t;
    faulted : bool;
  }

  type object_meta = {
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
  }

  val object_meta : store -> bucket:string -> key:string -> object_meta option
  val keys : store -> bucket:string -> string list
  val history : store -> op_record list
  val clear_history : store -> unit
  val dump_strings : store -> bucket:string -> (string * string) list

  include
    S
      with type connection := t
       and type 'a io := 'a
       and type upload_body := Runtime.upload_body
       and type download_reader := Runtime.download_reader
end
