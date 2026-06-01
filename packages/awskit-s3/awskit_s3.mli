(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for AWS S3 bucket/object storage: object
    operations, bucket operations and configuration, multipart upload, presigned
    URLs, runtime-backed clients, and the in-memory simulator used by tests.

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
  val is_conditional_request_conflict : t -> bool
  val is_conditional_failure : t -> bool
end

(** User metadata represented as unprefixed [x-amz-meta-*] key/value pairs. *)
module Metadata : sig
  type t = Common.Metadata.t
end

(** S3 object storage classes. *)
module Storage_class : sig
  type t = Common.Storage_class.t =
    | Standard
    | Standard_ia
    | Onezone_ia
    | Intelligent_tiering
    | Glacier
    | Glacier_ir
    | Deep_archive

  val to_string : t -> string
  val of_string : string -> t option
end

(** S3 tag key/value pair. *)
module Tag : sig
  type t = Common.Tag.t = { key : string; value : string }
end

(** HTTP byte-range requests for S3 object reads. *)
module Range : sig
  type t = Common.Range.t

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

type endpoint_config = Endpoint_resolver.t
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

  module Tagging : sig
    type result = { tags : Tag.t list; response : Awskit.Response.t }
  end
end

module Object : OBJECT_DATA

module Put_object : sig
  type options = {
    content_type : string option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.t list;
    cache_control : string option;
    content_encoding : string option;
    content_disposition : string option;
    preconditions : Object.Preconditions.Write.t;
    checksum : Object.Checksum.request option;
    server_side_encryption : Object.Encryption.request option;
  }

  type result = {
    etag : Object.Etag.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module Get_object : sig
  type options = {
    range : Range.t option;
    preconditions : Object.Preconditions.Read.t;
    version_id : Object.Version_id.t option;
  }

  type result = {
    etag : Object.Etag.t option;
    content_type : string option;
    content_length : int64 option;
    last_modified : Ptime.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response option;
    server_side_encryption : Object.Encryption.response option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module Head_object : sig
  type options = {
    preconditions : Object.Preconditions.Read.t;
    version_id : Object.Version_id.t option;
  }

  type result = Get_object.result

  val default_options : options
end

module Delete_object : sig
  type options = {
    preconditions : Object.Preconditions.Delete.t;
    version_id : Object.Version_id.t option;
  }

  type result = {
    delete_marker : bool option;
    version_id : Object.Version_id.t option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module Delete_objects : sig
  type object_ = {
    key : string;
    version_id : Object.Version_id.t option;
    etag : Object.Etag.t option;
  }

  type deleted = {
    key : string;
    version_id : Object.Version_id.t option;
    delete_marker : bool option;
  }

  type item_error = { key : string; code : string; message : string option }

  type result = {
    deleted : deleted list;
    errors : item_error list;
    response : Awskit.Response.t;
  }
end

module Copy_object : sig
  type metadata_directive = [ `Copy | `Replace of Metadata.t ]

  type options = {
    source_version_id : Object.Version_id.t option;
    source_preconditions : Object.Preconditions.Copy_source.t;
    metadata_directive : metadata_directive option;
    storage_class : Storage_class.t option;
    checksum : Object.Checksum.request option;
    server_side_encryption : Object.Encryption.request option;
  }

  type result = {
    etag : Object.Etag.t option;
    last_modified : Ptime.t option;
    version_id : Object.Version_id.t option;
    copy_source_version_id : Object.Version_id.t option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module List_object_versions : sig
  type options = {
    prefix : string option;
    delimiter : string option;
    max_keys : int option;
    key_marker : string option;
    version_id_marker : Object.Version_id.t option;
  }

  type object_version = {
    key : string;
    version_id : Object.Version_id.t option;
    is_latest : bool option;
    last_modified : Ptime.t option;
    etag : Object.Etag.t option;
    size : int64 option;
    storage_class : Storage_class.t option;
    owner : string option;
  }

  type delete_marker = {
    key : string;
    version_id : Object.Version_id.t option;
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
    version_id_marker : Object.Version_id.t option;
    next_key_marker : string option;
    next_version_id_marker : Object.Version_id.t option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module List_objects_v2 : sig
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
    etag : Object.Etag.t option;
    last_modified : Ptime.t option;
    storage_class : Storage_class.t option;
    owner : string option;
    checksums : Object.Checksum.response list;
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

(** Bucket data types and configuration records. *)
module type BUCKET_DATA = sig
  type info = { name : string; creation_date : Ptime.t option }

  module Versioning : sig
    module Status : sig
      type t = Enabled | Suspended

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { status : Status.t option; response : Awskit.Response.t }
  end

  module Tagging : sig
    type result = { tags : Tag.t list; response : Awskit.Response.t }
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
    type result = { config : config; response : Awskit.Response.t }
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
    type result = { config : config; response : Awskit.Response.t }
  end

  module Website : sig
    type config = {
      index_document_suffix : string option;
      error_document_key : string option;
    }

    type result = { config : config; response : Awskit.Response.t }
  end

  module Public_access_block : sig
    type config = {
      block_public_acls : bool;
      ignore_public_acls : bool;
      block_public_policy : bool;
      restrict_public_buckets : bool;
    }

    type result = { config : config; response : Awskit.Response.t }

    val all_false : config
  end

  module Ownership_controls : sig
    module Object_ownership : sig
      type t = Bucket_owner_enforced | Bucket_owner_preferred | Object_writer

      val to_string : t -> string
      val of_string : string -> t option
    end

    type config = { object_ownership : Object_ownership.t }
    type result = { config : config; response : Awskit.Response.t }
  end

  module Request_payment : sig
    module Payer : sig
      type t = Bucket_owner | Requester

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { payer : Payer.t option; response : Awskit.Response.t }
  end

  module Accelerate : sig
    module Status : sig
      type t = Enabled | Suspended

      val to_string : t -> string
      val of_string : string -> t option
    end

    type result = { status : Status.t option; response : Awskit.Response.t }
  end

  module Policy_status : sig
    type result = { is_public : bool option; response : Awskit.Response.t }
  end

  module Logging : sig
    type target = { target_bucket : string; target_prefix : string }
    type config = { logging : target option }
    type result = { config : config; response : Awskit.Response.t }

    val disabled : config
    val enabled : target_bucket:string -> target_prefix:string -> config
  end
end

module Bucket : BUCKET_DATA

module Create_bucket : sig
  type options = { region : Awskit.Region.t option }
  type result = { response : Awskit.Response.t }

  val default_options : options
end

module Delete_bucket : sig
  type result = { response : Awskit.Response.t }
end

module Head_bucket : sig
  type result = {
    name : string;
    region : Awskit.Region.t option;
    response : Awskit.Response.t;
  }
end

module List_buckets : sig
  type result = Bucket.info list
end

module Get_bucket_location : sig
  type result = Awskit.Region.t option
end

(** Multipart upload data types. *)
module rec Multipart : sig
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

  module Managed : sig
    val min_part_size : int
    val default_part_size : int
    val max_parts : int

    type options = {
      part_size : int;
      create_options : Create_multipart_upload.options;
      upload_part_options : Upload_part.options;
    }

    type result = {
      upload : Upload.t;
      parts : Part.t list;
      complete : Complete_multipart_upload.result;
    }

    val default_options : options
    val validate_options : options -> (unit, Error.t) Stdlib.result
  end
end

and Create_multipart_upload : sig
  type options = {
    content_type : string option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.t list;
    checksum : Object.Checksum.request option;
    server_side_encryption : Object.Encryption.request option;
  }

  type result = { upload : Multipart.Upload.t; response : Awskit.Response.t }

  val default_options : options
end

and Upload_part : sig
  type options = { checksum : Object.Checksum.request option }

  type result = {
    part : Multipart.Part.t;
    checksum : Object.Checksum.response option;
    response : Awskit.Response.t;
  }

  val default_options : options
end

and Complete_multipart_upload : sig
  type result = {
    etag : Object.Etag.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response option;
    response : Awskit.Response.t;
  }
end

and Abort_multipart_upload : sig
  type result = Awskit.Response.t
end

and List_parts : sig
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
    response : Awskit.Response.t;
  }

  val default_options : options
end

module type MULTIPART_DATA = module type of Multipart

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
  type request_body
  type response_body_reader

  val put :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    body:request_body ->
    unit ->
    (Put_object.result, Error.t) result io

  val get :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    consume:(response_body_reader -> ('a, Error.t) result io) ->
    unit ->
    (Get_object.result * 'a, Error.t) result io

  val head :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Head_object.options ->
    unit ->
    (Head_object.result, Error.t) result io

  val exists :
    connection -> bucket:string -> key:string -> (bool, Error.t) result io

  val delete :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Delete_object.options ->
    unit ->
    (Delete_object.result, Error.t) result io

  val delete_objects :
    connection ->
    bucket:string ->
    objects:Delete_objects.object_ list ->
    (Delete_objects.result, Error.t) result io

  val copy :
    connection ->
    source_bucket:string ->
    source_key:string ->
    destination_bucket:string ->
    destination_key:string ->
    ?options:Copy_object.options ->
    unit ->
    (Copy_object.result, Error.t) result io

  val list_versions :
    connection ->
    bucket:string ->
    ?options:List_object_versions.options ->
    unit ->
    (List_object_versions.page, Error.t) result io

  val list :
    connection ->
    bucket:string ->
    ?options:List_objects_v2.options ->
    unit ->
    (List_objects_v2.page, Error.t) result io

  val list_keys :
    connection ->
    bucket:string ->
    ?options:List_objects_v2.options ->
    unit ->
    (string list, Error.t) result io

  module List_objects_v2 : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> List_objects_v2.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (List_objects_v2.page list, Error.t) result io

    val objects :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (List_objects_v2.object_summary list, Error.t) result io

    val keys :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (string list, Error.t) result io
  end

  module List_object_versions : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> List_object_versions.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.page list, Error.t) result io

    val object_versions :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.object_version list, Error.t) result io

    val delete_markers :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.delete_marker list, Error.t) result io
  end

  val put_string :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    string ->
    (Put_object.result, Error.t) result io

  val put_bytes :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    bytes ->
    (Put_object.result, Error.t) result io

  val get_as_string :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Get_object.options ->
    unit ->
    (Get_object.result * string, Error.t) result io

  val get_as_bytes :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Get_object.options ->
    unit ->
    (Get_object.result * bytes, Error.t) result io

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
    ?options:Create_bucket.options ->
    unit ->
    (Create_bucket.result, Error.t) result io

  val delete :
    connection -> bucket:string -> (Delete_bucket.result, Error.t) result io

  val head :
    connection -> bucket:string -> (Head_bucket.result, Error.t) result io

  val exists : connection -> bucket:string -> (bool, Error.t) result io
  val list : connection -> (List_buckets.result, Error.t) result io

  val get_location :
    connection ->
    bucket:string ->
    (Get_bucket_location.result, Error.t) result io

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
  type request_body

  val create_upload :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Create_multipart_upload.options ->
    unit ->
    (Create_multipart_upload.result, Error.t) result io

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    body:request_body ->
    ?options:Upload_part.options ->
    unit ->
    (Upload_part.result, Error.t) result io

  val complete_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    Multipart.Part.t list ->
    (Complete_multipart_upload.result, Error.t) result io

  val abort_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    (Abort_multipart_upload.result, Error.t) result io

  val list_parts :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:List_parts.options ->
    unit ->
    (List_parts.page, Error.t) result io

  module List_parts : sig
    val fold_pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:List_parts.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> List_parts.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:List_parts.options ->
      ?max_pages:int ->
      unit ->
      (List_parts.page list, Error.t) result io

    val parts :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:List_parts.options ->
      ?max_pages:int ->
      unit ->
      (List_parts.part_info list, Error.t) result io
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
  type request_body
  type response_body_reader

  module Object :
    OBJECT
      with type connection = connection
       and type 'a io = 'a io
       and type request_body = request_body
       and type response_body_reader = response_body_reader

  module Bucket :
    BUCKET with type connection = connection and type 'a io = 'a io

  module Multipart :
    MULTIPART
      with type connection = connection
       and type 'a io = 'a io
       and type request_body = request_body

  module Presigned :
    PRESIGNED with type connection = connection and type 'a io = 'a io
end

(** Build an S3 client from a runtime implementation. *)
module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type request_body = R.request_body
     and type response_body_reader = R.response_body_reader

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
      | `Delete_objects
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
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader
end
