module type BODY = sig
  type +'a io
  type t
  type writer

  val empty : t
  val of_string : string -> t
  val of_bytes : bytes -> t

  val of_stream :
    content_length:int64 ->
    replayable:bool ->
    write:(writer -> (unit, Awskit.Error.t) result io) ->
    (t, Awskit.Error.t) result
  (** Build a streaming request body with a known [content_length].

      [replayable] must be [true] only when retrying the request can recreate
      the same bytes by invoking [write] again. Negative content lengths are
      rejected before a request is sent. *)

  val content_length : t -> int64 option
  val replayable : t -> bool

  module Writer : sig
    type t = writer

    val write_string : t -> string -> (unit, Awskit.Error.t) result io
    val write_bytes : t -> bytes -> (unit, Awskit.Error.t) result io

    val write_subbytes :
      t -> bytes -> off:int -> len:int -> (unit, Awskit.Error.t) result io
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
    ?chunk_size:int -> max_bytes:int64 -> t -> (bytes, Awskit.Error.t) result io

  val to_string :
    ?chunk_size:int ->
    max_bytes:int64 ->
    t ->
    (string, Awskit.Error.t) result io
end

module type OBJECT = sig
  (** Object operations produced by runtime-backed S3 clients. *)

  type client
  (** Configured S3 client. *)

  type +'a io
  (** Runtime effect type. *)

  type request_body
  (** Runtime-owned request body type. *)

  type response_body_reader
  (** Scoped runtime response-body reader type. *)

  val put :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    body:request_body ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload an object body with [PutObject].

      [body] must carry an accurate request-body descriptor. S3 uploads require
      a known content length in this library; runtime helpers such as
      [Runtime.Request_body.of_string] and [of_bytes] satisfy that contract. *)

  val put_string :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    contents:string ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload an in-memory string using the same [PutObject] operation model as
      {!val:put}. *)

  val put_bytes :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    contents:bytes ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload in-memory bytes using the same [PutObject] operation model as
      {!val:put}. *)

  val get :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    consume:(response_body_reader -> ('a, Awskit.Error.t) result io) ->
    unit ->
    ('a Object.Get.result, Awskit.Error.t) result io
  (** Fetch an object and consume its body inside [consume].

      The response-body reader is scoped to the callback and must not escape it.
      The returned record contains response metadata and the callback result. *)

  val get_string :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (string Object.Get.result, Awskit.Error.t) result io
  (** Fetch an object into memory as a string.

      [max_bytes] is required so callers choose an explicit memory bound. Use
      {!val:get} with a streaming [consume] callback for large objects. *)

  val get_bytes :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (bytes Object.Get.result, Awskit.Error.t) result io
  (** Fetch an object into memory as bytes, bounded by [max_bytes]. *)

  val find :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    consume:(response_body_reader -> ('a, Awskit.Error.t) result io) ->
    unit ->
    ('a Object.Get.result option, Awskit.Error.t) result io
  (** Return and consume an object when it is present.

      This is the option-returning lookup form. It returns [Ok None] for object
      not-found errors; other service, auth, transport, and decode failures
      remain [Error]. Use {!val:get} when callers need the raw GetObject service
      behavior. *)

  val find_string :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (string Object.Get.result option, Awskit.Error.t) result io
  (** Return and consume an object as a bounded in-memory string when it is
      present. *)

  val find_bytes :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (bytes Object.Get.result option, Awskit.Error.t) result io
  (** Return and consume an object as bounded in-memory bytes when it is
      present. *)

  val head :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.result, Awskit.Error.t) result io
  (** Fetch object metadata without reading an object body. *)

  val find_metadata :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
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
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Head.options ->
    unit ->
    (bool, Awskit.Error.t) result io
  (** Return [false] for object-not-found responses and [true] for success.

      Accepts the same options as {!val:head}, including version id,
      preconditions, checksum mode, and expected-owner guards. Code-less
      [HeadObject] 404 responses are treated as absent objects. Coded
      [NoSuchBucket] responses and other service, auth, transport, and decode
      failures remain [Error]. *)

  val delete :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Delete.options ->
    unit ->
    (Object.Delete.result, Awskit.Error.t) result io
  (** Delete an object or a specific object version. *)

  val delete_objects :
    client ->
    bucket:Bucket_name.t ->
    objects:Object.Delete_many.object_ list ->
    ?options:Object.Delete_many.options ->
    unit ->
    (Object.Delete_many.result, Awskit.Error.t) result io
  (** Delete multiple objects with [DeleteObjects].

      Per-object failures are represented in [Object.Delete_many.result.errors]
      even when the operation itself returns [Ok]. *)

  val copy :
    client ->
    source_bucket:Bucket_name.t ->
    source_key:Object_key.t ->
    destination_bucket:Bucket_name.t ->
    destination_key:Object_key.t ->
    ?options:Object.Copy.options ->
    unit ->
    (Object.Copy.result, Awskit.Error.t) result io
  (** Copy an object from one bucket/key to another. *)

  val list_versions :
    client ->
    bucket:Bucket_name.t ->
    ?options:Object.Versions.options ->
    unit ->
    (Object.Versions.page, Awskit.Error.t) result io
  (** Fetch one [ListObjectVersions] page. Use {!module:Versions} helpers to
      follow pagination. *)

  val list :
    client ->
    bucket:Bucket_name.t ->
    ?options:Object.List.options ->
    unit ->
    (Object.List.page, Awskit.Error.t) result io
  (** Fetch one [ListObjectsV2] page. Use {!module:List} helpers to follow
      pagination. *)

  module List : sig
    (** Pagination helpers for [ListObjectsV2]. *)

    type 'acc fold_step =
      | Continue of 'acc
      | Stop of 'acc
          (** Decision returned by {!val:fold_pages_until}. [Stop] returns the
              accumulator without fetching another page. *)

    val fold_pages :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.List.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow continuation tokens and fold pages until S3 stops returning a
        next token or [max_pages] is reached. *)

    val fold_pages_until :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.List.page -> ('acc fold_step, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow continuation tokens and fold pages until S3 stops returning a
        next token, [max_pages] is reached, or [f] returns [Stop]. *)

    val pages :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      max_pages:int ->
      unit ->
      (Object.List.page list, Awskit.Error.t) result io
    (** Collect listing pages up to the explicit [max_pages] bound.

        Returns an error if S3 reports more pages than the bound allows. *)

    val objects :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      max_pages:int ->
      unit ->
      (Object.List.object_summary list, Awskit.Error.t) result io
    (** Collect object summaries across listing pages up to [max_pages]. *)

    val keys :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      max_pages:int ->
      unit ->
      (Object_key.t list, Awskit.Error.t) result io
    (** Collect object keys across listing pages up to [max_pages]. *)
  end

  module Versions : sig
    (** Pagination helpers for [ListObjectVersions]. *)

    type 'acc fold_step =
      | Continue of 'acc
      | Stop of 'acc
          (** Decision returned by {!val:fold_pages_until}. [Stop] returns the
              accumulator without fetching another page. *)

    val fold_pages :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.Versions.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow key/version markers and fold pages until S3 stops returning next
        markers or [max_pages] is reached. *)

    val fold_pages_until :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:
        ('acc ->
        Object.Versions.page ->
        ('acc fold_step, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow key/version markers and fold pages until S3 stops returning next
        markers, [max_pages] is reached, or [f] returns [Stop]. *)

    val pages :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      max_pages:int ->
      unit ->
      (Object.Versions.page list, Awskit.Error.t) result io
    (** Collect version listing pages up to the explicit [max_pages] bound.

        Returns an error if S3 reports more pages than the bound allows. *)

    val object_versions :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      max_pages:int ->
      unit ->
      (Object.Versions.object_version list, Awskit.Error.t) result io
    (** Collect object-version entries across pages up to [max_pages]. *)

    val delete_markers :
      client ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      max_pages:int ->
      unit ->
      (Object.Versions.delete_marker list, Awskit.Error.t) result io
    (** Collect delete-marker entries across pages up to [max_pages]. *)
  end

  module Tagging : sig
    (** Object tagging operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      key:Object_key.t ->
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Awskit.Error.t) result io
    (** Fetch object tags. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      key:Object_key.t ->
      ?options:Object.Tagging.options ->
      tags:Tag.Set.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the object's tag set. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      key:Object_key.t ->
      ?options:Object.Tagging.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Remove all tags from the object. *)
  end
end

module type BUCKET = sig
  (** Bucket lifecycle and bucket-configuration operations. *)

  type client
  (** Configured S3 client. *)

  type +'a io
  (** Runtime effect type. *)

  val create :
    client ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Create.options ->
    unit ->
    (Bucket.Create.result, Awskit.Error.t) result io
  (** Create a bucket. *)

  val delete :
    client ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Delete.options ->
    unit ->
    (Bucket.Delete.result, Awskit.Error.t) result io
  (** Delete an empty bucket. *)

  val head :
    client ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Head.options ->
    unit ->
    (Bucket.Head.result, Awskit.Error.t) result io
  (** Check bucket existence and return metadata such as the region hint. *)

  val exists :
    client ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Head.options ->
    unit ->
    (bool, Awskit.Error.t) result io
  (** Return [false] for S3 not-found responses and [true] for success. *)

  val list : client -> (Bucket.List_buckets.result, Awskit.Error.t) result io
  (** List buckets visible to the credentials. *)

  val get_location :
    client ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Get_location.options ->
    unit ->
    (Bucket.Get_location.result, Awskit.Error.t) result io
  (** Fetch the bucket location constraint/region. *)

  module Policy : sig
    (** Bucket policy operations. Policy documents are opaque validated JSON. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      unit ->
      (Policy.t, Awskit.Error.t) result io
    (** Fetch a bucket policy document. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      policy:Policy.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket policy document. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete the bucket policy. *)
  end

  module Versioning : sig
    (** Bucket versioning operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Versioning.options ->
      unit ->
      (Bucket.Versioning.result, Awskit.Error.t) result io
    (** Fetch bucket versioning state. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Versioning.options ->
      status:Bucket.Versioning.Status.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Set bucket versioning to [Enabled] or [Suspended]. *)
  end

  module Tagging : sig
    (** Bucket tagging operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      unit ->
      (Bucket.Tagging.result, Awskit.Error.t) result io
    (** Fetch the bucket tag set. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      tags:Tag.Set.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket tag set. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Remove all bucket tags. *)
  end

  module Encryption : sig
    (** Bucket default-encryption operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      unit ->
      (Bucket.Encryption.result, Awskit.Error.t) result io
    (** Fetch bucket default-encryption configuration. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      config:Bucket.Encryption.Config.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket default-encryption configuration. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket default-encryption configuration. *)
  end

  module Cors : sig
    (** Bucket CORS operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      unit ->
      (Bucket.Cors.result, Awskit.Error.t) result io
    (** Fetch bucket CORS configuration. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      config:Bucket.Cors.Config.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket CORS configuration. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket CORS configuration. *)
  end

  module Public_access_block : sig
    (** Bucket public-access-block operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      unit ->
      (Bucket.Public_access_block.result, Awskit.Error.t) result io
    (** Fetch public-access-block configuration. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      config:Bucket.Public_access_block.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace public-access-block configuration. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete public-access-block configuration. *)
  end

  module Ownership_controls : sig
    (** Bucket ownership-controls operations. *)

    val get :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
      unit ->
      (Bucket.Ownership_controls.result, Awskit.Error.t) result io
    (** Fetch ownership-controls configuration. *)

    val put :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
      config:Bucket.Ownership_controls.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace ownership-controls configuration. *)

    val delete :
      client ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete ownership-controls configuration. *)
  end
end

module type MULTIPART = sig
  (** Multipart upload operations. *)

  type client
  (** Configured S3 client. *)

  type +'a io
  (** Runtime effect type. *)

  type request_body
  (** Runtime-owned request body type. *)

  val create_upload :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Multipart.Create.options ->
    unit ->
    (Multipart.Create.result, Awskit.Error.t) result io
  (** Start a multipart upload and return its upload handle. *)

  val upload_part :
    client ->
    upload:_ Multipart.Upload.t ->
    part_number:Multipart.Part_number.t ->
    body:request_body ->
    ?options:Multipart.Upload_part.options ->
    unit ->
    (Multipart.Upload_part.result, Awskit.Error.t) result io
  (** Upload one multipart part.

      [part_number] must be in S3's valid range, and [body] must have an
      accurate known content length. *)

  val complete_upload :
    client ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.Complete.options ->
    parts:Multipart.Part.t list ->
    unit ->
    (Multipart.Complete.result, Awskit.Error.t) result io
  (** Complete a multipart upload using the supplied completed part list. *)

  val abort_upload :
    client ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.Abort.options ->
    unit ->
    (Multipart.Abort.result, Awskit.Error.t) result io
  (** Abort a multipart upload. *)

  val list_parts :
    client ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.List_parts.options ->
    unit ->
    (Multipart.List_parts.page, Awskit.Error.t) result io
  (** Fetch one [ListParts] page. Use [List_parts] helpers to follow pagination.
  *)

  module List_parts : sig
    (** Pagination helpers for [ListParts]. *)

    val fold_pages :
      client ->
      upload:_ Multipart.Upload.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Multipart.List_parts.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io
    (** Follow part-number markers and fold pages until S3 stops returning a
        next marker or [max_pages] is reached. *)

    val pages :
      client ->
      upload:_ Multipart.Upload.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.page list, Awskit.Error.t) result io
    (** Collect uploaded-part pages. *)

    val parts :
      client ->
      upload:_ Multipart.Upload.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.part_info list, Awskit.Error.t) result io
    (** Collect uploaded part summaries across pages. *)
  end
end

module type PRESIGNED = sig
  (** Presigned request artifact helpers bound to a configured S3 client. *)

  type client
  (** Configured S3 client. *)

  type +'a io
  (** Runtime effect type. *)

  val get_object :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [GET Object] request artifact. *)

  val put_object :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [PUT Object] request artifact. Headers returned in
      the result must be sent by the eventual uploader. *)

  val head_object :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Head_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [HEAD Object] request artifact. *)

  val delete_object :
    client ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [DELETE Object] request artifact. *)

  val upload_part :
    client ->
    upload:_ Multipart.Upload.t ->
    part_number:Multipart.Part_number.t ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [UploadPart] request artifact for one multipart part.
  *)
end

module type S = sig
  type t
  type runtime_connection
  type +'a io
  type request_body
  type response_body_reader

  val create : ?endpoint_config:Endpoint_config.t -> runtime_connection -> t
  val runtime_connection : t -> runtime_connection

  module Body : BODY with type 'a io = 'a io and type t = request_body

  module Reader :
    READER with type 'a io = 'a io and type t = response_body_reader

  module Object :
    OBJECT
      with type client = t
       and type 'a io = 'a io
       and type request_body = request_body
       and type response_body_reader = response_body_reader

  module Bucket : BUCKET with type client = t and type 'a io = 'a io

  module Multipart :
    MULTIPART
      with type client = t
       and type 'a io = 'a io
       and type request_body = request_body

  module Presigned : PRESIGNED with type client = t and type 'a io = 'a io
end

module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region
module Error = S3_error
module Bucket_name = Bucket_name
module Object_key = Object_key
module Account_id = Account_id
module Content_type = Content_type
module Header_value = Header_value
module Metadata = Metadata
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Encryption = Encryption
module Endpoint_config = Endpoint_config
module Endpoint_resolver = Endpoint_resolver
module Object = Object
module Bucket = Bucket
module Multipart = Multipart
module Transfer = Transfer
module Policy = Policy
module Presigned = Presigned

type addressing_style = Endpoint_config.addressing_style
type endpoint_variant = Endpoint_config.endpoint_variant
type endpoint_config = Endpoint_resolver.t

let endpoint_config ?addressing_style ?endpoint_variant () =
  Endpoint_config.aws ?addressing_style ?endpoint_variant ()

let default_endpoint_config = Endpoint_resolver.default

module Make (R : Awskit.Runtime.S) = struct
  type runtime_connection = R.connection

  type t = {
    runtime_connection : R.connection;
    endpoint_config : Endpoint_config.t;
  }

  type 'a io = 'a R.t
  type request_body = R.request_body
  type response_body_reader = R.response_body_reader

  let create ?(endpoint_config = Endpoint_config.default) runtime_connection =
    { runtime_connection; endpoint_config }

  let runtime_connection client = client.runtime_connection

  module Streaming = Streaming.Make (R)
  module Body = Streaming.Body
  module Reader = Streaming.Reader

  module Context = struct
    module R = R

    type connection = t
    type 'a io = 'a R.t
    type request_body = R.request_body
    type response_body_reader = R.response_body_reader

    let bind = R.IO.bind
    let ( let* ) = R.IO.bind
    let return = R.IO.return
    let return_ok value = R.IO.return (Ok value)
    let return_error error = R.IO.return (Error error)
    let empty_hash = Awskit.Body.Payload_hash.sha256_of_string ""
    let runtime_connection client = client.runtime_connection
    let endpoint_config client = client.endpoint_config
    let region client = R.Endpoint.region (runtime_connection client)
    let now client = R.Clock.now (runtime_connection client)
    let credentials client = R.Credentials.resolve (runtime_connection client)

    let object_request conn ~bucket ~key =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket -> (
          match Object_key.of_string key with
          | Error _ as error -> error
          | Ok key ->
              Endpoint_resolver.resolve_object_request (endpoint_config conn)
                ~region:(region conn) ~bucket ~key)

    let bucket_request conn ~bucket ~suffix ~signing_suffix =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket ->
          Endpoint_resolver.resolve_bucket_request (endpoint_config conn)
            ~region:(region conn) ~bucket ~suffix ~signing_suffix

    let root_request conn =
      match
        Endpoint_resolver.endpoint (endpoint_config conn) ~region:(region conn)
      with
      | Error _ as error -> error
      | Ok endpoint ->
          Ok
            {
              Endpoint_resolver.Request.endpoint;
              path = "/";
              signing_path = "/";
              signing_region =
                Endpoint_config.signing_region (endpoint_config conn)
                  ~client_region:(region conn);
              style = `Path;
            }

    let bounded_body_chunk_size = 8192

    let bounded_body_context error =
      Awskit.Error.Producer.with_context "reading bounded response body" error

    let with_bounded_body_context result =
      let* result = result in
      match result with
      | Ok _ as ok -> return ok
      | Error error -> return_error (bounded_body_context error)

    let read_body reader ~max_size =
      with_bounded_body_context
        (Reader.to_string ~chunk_size:bounded_body_chunk_size
           ~max_bytes:max_size reader)

    let read_body_bytes reader ~max_size =
      with_bounded_body_context
        (Reader.to_bytes ~chunk_size:bounded_body_chunk_size ~max_bytes:max_size
           reader)

    let read_response_body body ~max_size =
      R.Response_body.with_reader body ~consume:(read_body ~max_size)

    let discard_response_body = R.Response_body.discard

    let first_some first second =
      match first with Some _ -> first | None -> second

    let service_error response body =
      let error =
        match body with
        | None -> S3_xml.empty_service_error
        | Some body -> S3_xml.service_error body
      in
      Awskit.Error.Producer.service
        ~status:(Awskit.Response.status response)
        ?code:error.code ?message:error.message
        ?request_id:
          (first_some (Awskit.Response.request_id response) error.request_id)
        ?host_id:(first_some (Awskit.Response.host_id response) error.host_id)
        ~headers:(Awskit.Response.headers response)
        ?body ()

    let signed_request conn ~method_ ~(request : Endpoint_resolver.Request.t)
        ~query ~headers ~payload_hash =
      let headers =
        ("host", Awskit.Endpoint.authority request.endpoint) :: headers
      in
      let* credentials = credentials conn in
      match credentials with
      | Error error -> return_error error
      | Ok credentials -> (
          match
            Awskit.Signing.sign_request_params ~credentials
              ~region:request.signing_region ~service:"s3" ~method_
              ~path:request.signing_path ~query_params:query ~headers
              ~payload_hash ~now:(now conn)
          with
          | Error error -> return_error error
          | Ok signed -> (
              match
                Awskit.Request.Target.create
                  ~scheme:(Awskit.Endpoint.scheme request.endpoint)
                  ~host:(Awskit.Endpoint.host request.endpoint)
                  ?port:(Awskit.Endpoint.port request.endpoint)
                  ~path:request.path ~query ()
              with
              | Error error -> return_error error
              | Ok target -> (
                  match
                    Awskit.Request.create ~method_ ~target
                      ~headers:signed.headers ()
                  with
                  | Error error -> return_error error
                  | Ok request -> return_ok request)))

    let retry_or_error conn ~attempt ~budget_state ~replayable error retry =
      let policy = R.Retry.policy (runtime_connection conn) in
      let max_attempts = Awskit.Retry.max_attempts policy in
      match
        Awskit.Retry.delay policy ~attempt ~error
          ~random_float:(R.Random.float (runtime_connection conn))
      with
      | Some delay when replayable -> (
          match Awskit.Retry.charge_retry policy budget_state error with
          | None ->
              return_error
                (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                   ~reason:"retry budget exhausted" error)
          | Some budget_state ->
              let* () = R.Sleeper.sleep (runtime_connection conn) delay in
              retry budget_state (attempt + 1))
      | Some _delay ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"not retried because request body is not replayable"
               error)
      | None when attempt >= max_attempts ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"retry attempts exhausted" error)
      | None ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"error is not retryable by policy" error)

    type 'a response_action =
      | Done of ('a, Awskit.Error.t) result
      | Retry of Awskit.Error.t

    let retryable_service_error error =
      let open Awskit.Error in
      match kind error with
      | Service _ -> (
          match retry_class error with
          | Retryable | Throttled -> true
          | Auth | Conflict | Not_found | Fatal | Unknown -> false)
      | Validation _ | Credentials _ | Signing _ | Endpoint _ | Transport _
      | Timeout _ | Cancelled _ | Body _ | Decode _ | Retry_exhausted _
      | Not_supported _ | Multiple _ ->
          false

    let with_response_action conn ~method_ ~request ~query ~headers
        ~payload_hash body ~success_action =
      let replayable = (R.Request_body.descriptor body).replayable in
      let policy = R.Retry.policy (runtime_connection conn) in
      let initial_budget_state = Awskit.Retry.initial_budget_state policy in
      let rec attempt budget_state attempt_number =
        let* request =
          signed_request conn ~method_ ~request ~query ~headers ~payload_hash
        in
        match request with
        | Error error -> return_error error
        | Ok request -> (
            let* response =
              R.Transport.with_response (runtime_connection conn) request ~body
                ~consume:(fun response response_body ->
                  if Awskit.Response.is_success response then
                    success_action response response_body
                  else
                    let* body =
                      read_response_body response_body ~max_size:1_048_576L
                    in
                    match body with
                    | Error error -> return_ok (Done (Error error))
                    | Ok body ->
                        let error = service_error response (Some body) in
                        return_ok (Retry error))
            in
            match response with
            | Error error ->
                retry_or_error conn ~attempt:attempt_number ~budget_state
                  ~replayable error attempt
            | Ok (Done result) -> return result
            | Ok (Retry error) ->
                retry_or_error conn ~attempt:attempt_number ~budget_state
                  ~replayable error attempt)
      in
      attempt initial_budget_state 1

    let with_response conn ~method_ ~request ~query ~headers ~payload_hash body
        ~f =
      with_response_action conn ~method_ ~request ~query ~headers ~payload_hash
        body ~success_action:(fun response response_body ->
          let* result = f response response_body in
          return_ok (Done result))

    let with_retryable_embedded_response conn ~method_ ~request ~query ~headers
        ~payload_hash body ~f =
      with_response_action conn ~method_ ~request ~query ~headers ~payload_hash
        body ~success_action:(fun response response_body ->
          let* result = f response response_body in
          match result with
          | Error error when retryable_service_error error ->
              return_ok (Retry error)
          | Ok _ | Error _ -> return_ok (Done result))

    let with_empty_response conn ~method_ ~request ~query ~headers ~f =
      with_response conn ~method_ ~request ~query ~headers
        ~payload_hash:empty_hash R.Request_body.empty ~f

    let content_md5 body =
      Digestif.MD5.(digest_string body |> to_raw_string) |> Base64.encode_exn
  end

  module Multipart = Multipart_request.Make (Context)
  module Object = Object_request.Make (Context)
  module Bucket = Bucket_request.Make (Context)
  module Presigned = Presigned_request.Make (Context)
end
