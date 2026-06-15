module type RUNTIME = sig
  (** Runtime required by the S3 functor.

      This extends [Awskit.Runtime.S] with S3-specific endpoint resolution
      configuration. Runtime adapters implement this once and then reuse the
      pure S3 operation functor. *)

  include Awskit.Runtime.S

  val s3_endpoint_config : connection -> Endpoint_resolver.t
  (** Return S3 addressing/endpoint configuration for this connection. *)
end

module type BODY = sig
  type +'a io
  type t
  type writer

  val empty : t
  val of_string : string -> t
  val of_bytes : bytes -> t

  val of_stream :
    content_length:int64 ->
    write:(writer -> (unit, Awskit.Error.t) result io) ->
    t

  val content_length : t -> int64 option

  module Writer : sig
    type t = writer

    val write_string : t -> string -> (unit, Awskit.Error.t) result io
    val write_bytes : t -> bytes -> (unit, Awskit.Error.t) result io
  end
end

module type READER = sig
  type +'a io
  type t

  val read : t -> bytes -> off:int -> len:int -> (int, Awskit.Error.t) result io
  val next : ?chunk_size:int -> t -> (bytes option, Awskit.Error.t) result io

  val fold :
    ?chunk_size:int ->
    t ->
    init:'a ->
    f:('a -> bytes -> ('a, Awskit.Error.t) result io) ->
    ('a, Awskit.Error.t) result io

  val iter :
    ?chunk_size:int ->
    t ->
    f:(bytes -> (unit, Awskit.Error.t) result io) ->
    (unit, Awskit.Error.t) result io

  val to_bytes :
    ?chunk_size:int ->
    ?max_bytes:int64 ->
    t ->
    (bytes, Awskit.Error.t) result io

  val to_string :
    ?chunk_size:int ->
    ?max_bytes:int64 ->
    t ->
    (string, Awskit.Error.t) result io
end

module type OBJECT = sig
  (** Object operations produced by runtime-backed S3 clients. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  type request_body
  (** Runtime-owned request body type. *)

  type response_body_reader
  (** Scoped runtime response-body reader type. *)

  val put :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Put.options ->
    body:request_body ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload an object body with [PutObject].

      [body] must carry an accurate request-body descriptor. S3 uploads require
      a known content length in this library; runtime helpers such as
      [Runtime.Request_body.of_string] and [of_bytes] satisfy that contract. *)

  val get :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Get.options ->
    consume:(response_body_reader -> ('a, Awskit.Error.t) result io) ->
    unit ->
    (Object.Get.result * 'a, Awskit.Error.t) result io
  (** Fetch an object and consume its body inside [consume].

      The response-body reader is scoped to the callback and must not escape it.
      The returned pair contains response metadata and the callback result. *)

  val find :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Get.options ->
    consume:(response_body_reader -> ('a, Awskit.Error.t) result io) ->
    unit ->
    ((Object.Get.result * 'a) option, Awskit.Error.t) result io
  (** Return and consume an object when it is present.

      This is the option-returning lookup form. It returns [Ok None] for object
      not-found errors; other service, auth, transport, and decode failures
      remain [Error]. Use {!val:get} when callers need the raw GetObject service
      behavior. *)

  val head :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.result, Awskit.Error.t) result io
  (** Fetch object metadata without reading an object body. *)

  val find_metadata :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.result option, Awskit.Error.t) result io
  (** Return object metadata when the object is present.

      This is the option-returning lookup form. It returns [Ok None] for object
      not-found errors. S3 [HeadObject] responses may only expose HTTP status,
      so a code-less 404 response is treated as an absent object. Coded
      [NoSuchBucket] responses and other service, auth, transport, and decode
      failures remain [Error]. Use {!val:head} when callers need the raw
      HeadObject service behavior. *)

  val exists :
    connection ->
    bucket:string ->
    key:string ->
    (bool, Awskit.Error.t) result io
  (** Return [false] for S3 not-found responses and [true] for success.

      Other errors, including auth/transport/decode failures, are returned as
      [Error]. *)

  val delete :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Delete.options ->
    unit ->
    (Object.Delete.result, Awskit.Error.t) result io
  (** Delete an object or a specific object version. *)

  val delete_objects :
    connection ->
    bucket:string ->
    objects:Object.Delete_many.object_ list ->
    ?options:Object.Delete_many.options ->
    unit ->
    (Object.Delete_many.result, Awskit.Error.t) result io
  (** Delete multiple objects with [DeleteObjects].

      Per-object failures are represented in [Object.Delete_many.result.errors]
      even when the operation itself returns [Ok]. *)

  val copy :
    connection ->
    source_bucket:string ->
    source_key:string ->
    destination_bucket:string ->
    destination_key:string ->
    ?options:Object.Copy.options ->
    unit ->
    (Object.Copy.result, Awskit.Error.t) result io
  (** Copy an object from one bucket/key to another. *)

  val list_versions :
    connection ->
    bucket:string ->
    ?options:Object.Versions.options ->
    unit ->
    (Object.Versions.page, Awskit.Error.t) result io
  (** Fetch one [ListObjectVersions] page. Use [List_object_versions] helpers to
      follow pagination. *)

  val list :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (Object.List.page, Awskit.Error.t) result io
  (** Fetch one [ListObjectsV2] page. Use [List_objects_v2] helpers to follow
      pagination. *)

  val list_keys :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (string list, Awskit.Error.t) result io
  (** Fetch one listing page and return only object keys from that page. *)

  module List_objects_v2 : sig
    (** Pagination helpers for [ListObjectsV2]. *)

    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.List.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow continuation tokens and fold pages until S3 stops returning a
        next token or [max_pages] is reached. *)

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.page list, Awskit.Error.t) result io
    (** Collect listing pages. *)

    val objects :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.object_summary list, Awskit.Error.t) result io
    (** Collect object summaries across listing pages. *)

    val keys :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (string list, Awskit.Error.t) result io
    (** Collect object keys across listing pages. *)
  end

  module List_object_versions : sig
    (** Pagination helpers for [ListObjectVersions]. *)

    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.Versions.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow key/version markers and fold pages until S3 stops returning next
        markers or [max_pages] is reached. *)

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.page list, Awskit.Error.t) result io
    (** Collect version listing pages. *)

    val object_versions :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.object_version list, Awskit.Error.t) result io
    (** Collect object-version entries across pages. *)

    val delete_markers :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.delete_marker list, Awskit.Error.t) result io
    (** Collect delete-marker entries across pages. *)
  end

  module Tagging : sig
    (** Object tagging operations. *)

    val get :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Awskit.Error.t) result io
    (** Fetch object tags. *)

    val put :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      Tag.t list ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the object's tag set. *)

    val delete :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Remove all tags from the object. *)
  end
end

module type BUCKET = sig
  (** Bucket lifecycle and bucket-configuration operations. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  val create :
    connection ->
    bucket:string ->
    ?options:Bucket.Create.options ->
    unit ->
    (Bucket.Create.result, Awskit.Error.t) result io
  (** Create a bucket. *)

  val delete :
    connection ->
    bucket:string ->
    ?expected_bucket_owner:string ->
    unit ->
    (Bucket.Delete.result, Awskit.Error.t) result io
  (** Delete an empty bucket. *)

  val head :
    connection ->
    bucket:string ->
    ?expected_bucket_owner:string ->
    unit ->
    (Bucket.Head.result, Awskit.Error.t) result io
  (** Check bucket existence and return metadata such as the region hint. *)

  val exists :
    connection ->
    bucket:string ->
    ?expected_bucket_owner:string ->
    unit ->
    (bool, Awskit.Error.t) result io
  (** Return [false] for S3 not-found responses and [true] for success. *)

  val list :
    connection -> (Bucket.List_buckets.result, Awskit.Error.t) result io
  (** List buckets visible to the credentials. *)

  val get_location :
    connection ->
    bucket:string ->
    ?expected_bucket_owner:string ->
    unit ->
    (Bucket.Get_location.result, Awskit.Error.t) result io
  (** Fetch the bucket location constraint/region. *)

  module Policy : sig
    (** Bucket policy operations. Policy documents are opaque validated JSON. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Policy.t, Awskit.Error.t) result io
    (** Fetch a bucket policy document. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Policy.t ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket policy document. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete the bucket policy. *)
  end

  module Versioning : sig
    (** Bucket versioning operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Versioning.result, Awskit.Error.t) result io
    (** Fetch bucket versioning state. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Versioning.Status.t ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Set bucket versioning to [Enabled] or [Suspended]. *)
  end

  module Tagging : sig
    (** Bucket tagging operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Tagging.result, Awskit.Error.t) result io
    (** Fetch the bucket tag set. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Tag.t list ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket tag set. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Remove all bucket tags. *)
  end

  module Encryption : sig
    (** Bucket default-encryption operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Encryption.result, Awskit.Error.t) result io
    (** Fetch bucket default-encryption configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Encryption.config ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket default-encryption configuration. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket default-encryption configuration. *)
  end

  module Cors : sig
    (** Bucket CORS operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Cors.result, Awskit.Error.t) result io
    (** Fetch bucket CORS configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Cors.config ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket CORS configuration. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket CORS configuration. *)
  end

  module Public_access_block : sig
    (** Bucket public-access-block operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Public_access_block.result, Awskit.Error.t) result io
    (** Fetch public-access-block configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Public_access_block.config ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace public-access-block configuration. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete public-access-block configuration. *)
  end

  module Ownership_controls : sig
    (** Bucket ownership-controls operations. *)

    val get :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Bucket.Ownership_controls.result, Awskit.Error.t) result io
    (** Fetch ownership-controls configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Ownership_controls.config ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace ownership-controls configuration. *)

    val delete :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete ownership-controls configuration. *)
  end
end

module type MULTIPART = sig
  (** Multipart upload operations. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  type request_body
  (** Runtime-owned request body type. *)

  val create_upload :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Multipart.Create.options ->
    unit ->
    (Multipart.Create.result, Awskit.Error.t) result io
  (** Start a multipart upload and return its upload id. *)

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    body:request_body ->
    ?options:Multipart.Upload_part.options ->
    unit ->
    (Multipart.Upload_part.result, Awskit.Error.t) result io
  (** Upload one multipart part.

      [part_number] must be in S3's valid range, and [body] must have an
      accurate known content length. *)

  val complete_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.Complete.options ->
    Multipart.Part.t list ->
    (Multipart.Complete.result, Awskit.Error.t) result io
  (** Complete a multipart upload using the supplied completed part list. *)

  val abort_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.Abort.options ->
    unit ->
    (Multipart.Abort.result, Awskit.Error.t) result io
  (** Abort a multipart upload. *)

  val list_parts :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.List_parts.options ->
    unit ->
    (Multipart.List_parts.page, Awskit.Error.t) result io
  (** Fetch one [ListParts] page. Use [List_parts] helpers to follow pagination.
  *)

  module List_parts : sig
    (** Pagination helpers for [ListParts]. *)

    val fold_pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Multipart.List_parts.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow part-number markers and fold pages until S3 stops returning a
        next marker or [max_pages] is reached. *)

    val pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.page list, Awskit.Error.t) result io
    (** Collect uploaded-part pages. *)

    val parts :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.part_info list, Awskit.Error.t) result io
    (** Collect uploaded part summaries across pages. *)
  end
end

module type PRESIGNED = sig
  (** Presigned URL helpers bound to a client connection. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  val get_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [GET Object] URL. *)

  val put_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [PUT Object] URL. Headers returned in the result must
      be sent by the eventual uploader. *)

  val head_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [HEAD Object] URL. *)

  val delete_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [DELETE Object] URL. *)

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [UploadPart] URL for one multipart part. *)
end

module type S = sig
  (** Complete S3 client surface for one runtime. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  type request_body
  (** Runtime-owned request body type. *)

  type response_body_reader
  (** Scoped runtime response-body reader type. *)

  module Runtime :
    RUNTIME
      with type connection = connection
       and type 'a t = 'a io
       and type request_body = request_body
       and type response_body_reader = response_body_reader

  module Body : BODY with type 'a io = 'a io and type t = request_body

  module Reader :
    READER with type 'a io = 'a io and type t = response_body_reader

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

module type Sigs = sig
  module Credentials = Awskit.Credentials
  module Endpoint = Awskit.Endpoint
  module Region = Awskit.Region

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

  module Metadata = Metadata
  module Storage_class = Storage_class
  module Tag = Tag
  module Range = Range

  type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

  type endpoint_variant =
    [ `Regional
    | `Dualstack
    | `Fips
    | `Fips_dualstack
    | `Accelerate
    | `Accelerate_dualstack ]

  module Endpoint_resolver = Endpoint_resolver

  type endpoint_config = Endpoint_resolver.t

  val endpoint_config :
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Endpoint.Scheme.t ->
    ?endpoint:string ->
    unit ->
    endpoint_config

  val default_endpoint_config : endpoint_config

  module Object = Object
  module Bucket = Bucket
  module Multipart = Multipart
  module Transfer = Transfer
  module Policy = Policy
  module Presigned = Presigned
  module Put_object = Object.Put
  module Get_object = Object.Get
  module Head_object = Object.Head
  module Delete_object = Object.Delete
  module Delete_objects = Object.Delete_many
  module Copy_object = Object.Copy
  module List_objects_v2 = Object.List
  module List_object_versions = Object.Versions
  module Create_bucket = Bucket.Create
  module Delete_bucket = Bucket.Delete
  module Head_bucket = Bucket.Head
  module List_buckets = Bucket.List_buckets
  module Get_bucket_location = Bucket.Get_location
  module Create_multipart_upload = Multipart.Create
  module Upload_part = Multipart.Upload_part
  module Complete_multipart_upload = Multipart.Complete
  module Abort_multipart_upload = Multipart.Abort
  module List_parts = Multipart.List_parts

  module type RUNTIME = RUNTIME
  module type BODY = BODY
  module type READER = READER
  module type OBJECT = OBJECT
  module type BUCKET = BUCKET
  module type MULTIPART = MULTIPART
  module type PRESIGNED = PRESIGNED
  module type S = S

  module Make (R : RUNTIME) :
    S
      with type connection = R.connection
       and type 'a io = 'a R.t
       and type request_body = R.request_body
       and type response_body_reader = R.response_body_reader
end
