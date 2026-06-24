(** AWS S3 client API.

    [Awskit_s3] is the main entrypoint for AWS S3 bucket and object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned request artifacts, endpoint resolution, and runtime-backed
    clients. *)

module type RUNTIME = sig
  (** Runtime required by the S3 functor.

      This extends [Awskit.Runtime.S] with S3-specific endpoint resolution
      configuration. Runtime adapters implement this once and then reuse the
      pure S3 operation functor. *)

  include Awskit.Runtime.S

  module S3_endpoint : sig
    type nonrec connection = connection

    val s3_endpoint_config : connection -> Endpoint_resolver.t
    (** Return S3 addressing/endpoint configuration for this connection. *)
  end
end

module type BODY = sig
  type +'a io

  type t
  (** Runtime request body descriptor.

      Values may be replayable, such as in-memory or file-backed bodies, or
      one-shot, such as bodies backed by a caller-owned stream. *)

  type writer
  (** Scoped writer passed to streaming body callbacks.

      The writer is valid only while the callback supplied to {!val:of_stream}
      is running. Do not store it or use it after the callback returns. *)

  val empty : t
  (** Empty replayable body with content length [0]. *)

  val of_string : string -> t
  (** Build a replayable in-memory body from a string. *)

  val of_bytes : bytes -> t
  (** Build a replayable in-memory body from bytes. *)

  val of_stream :
    content_length:int64 ->
    replayable:bool ->
    write:(writer -> (unit, Awskit.Error.t) result io) ->
    (t, Awskit.Error.t) result
  (** Build a streaming request body with a known [content_length].

      [replayable] must be [true] only when retrying the request can recreate
      the same bytes by invoking [write] again. Negative content lengths are
      rejected before a request is sent. The supplied writer is scoped to one
      invocation of [write]. *)

  val content_length : t -> int64 option
  (** Return the known content length for the body, when available. *)

  val replayable : t -> bool
  (** Return whether the body can be safely written again for retries. *)

  module Writer : sig
    type t = writer

    val write_string : t -> string -> (unit, Awskit.Error.t) result io
    (** Write one string chunk to a streaming request body. *)

    val write_bytes : t -> bytes -> (unit, Awskit.Error.t) result io
    (** Write one bytes chunk to a streaming request body. *)

    val write_subbytes :
      t -> bytes -> off:int -> len:int -> (unit, Awskit.Error.t) result io
    (** Write the slice [bytes.{off .. off + len - 1}] to a streaming request
        body. *)
  end
end

module type READER = sig
  type +'a io

  type t
  (** Scoped response body reader.

      Readers passed to object [consume] callbacks are valid only until the
      callback returns. Drain, copy, or decode the body inside that callback. *)

  val read : t -> bytes -> off:int -> len:int -> (int, Awskit.Error.t) result io
  (** Read up to [len] bytes into [bytes] starting at [off].

      Returns [Ok 0] at end of body. *)

  val next : ?chunk_size:int -> t -> (bytes option, Awskit.Error.t) result io
  (** Read the next fresh chunk, or [None] at end of body.

      [chunk_size] must be positive when supplied. *)

  val fold :
    ?chunk_size:int ->
    t ->
    init:'a ->
    f:('a -> bytes -> ('a, Awskit.Error.t) result io) ->
    ('a, Awskit.Error.t) result io
  (** Fold over response-body chunks until end of body or the callback returns
      an error. *)

  val iter :
    ?chunk_size:int ->
    t ->
    f:(bytes -> (unit, Awskit.Error.t) result io) ->
    (unit, Awskit.Error.t) result io
  (** Iterate over response-body chunks until end of body or the callback
      returns an error. *)

  val to_bytes :
    ?chunk_size:int -> max_bytes:int64 -> t -> (bytes, Awskit.Error.t) result io
  (** Drain the body into memory as bytes, failing if it exceeds [max_bytes]. *)

  val to_string :
    ?chunk_size:int ->
    max_bytes:int64 ->
    t ->
    (string, Awskit.Error.t) result io
  (** Drain the body into a string, failing if it exceeds [max_bytes]. *)
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
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    body:request_body ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload an object body with [PutObject].

      [body] must carry an accurate request-body descriptor. S3 uploads require
      a known content length in this library; adapter body constructors such as
      [Body.of_string] and [Body.of_bytes] satisfy that contract. *)

  val put_string :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    contents:string ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload an in-memory string using the same [PutObject] operation model as
      {!val:put}. *)

  val put_bytes :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Put.options ->
    contents:bytes ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io
  (** Upload in-memory bytes using the same [PutObject] operation model as
      {!val:put}. *)

  val get :
    connection ->
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
    connection ->
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
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (bytes Object.Get.result, Awskit.Error.t) result io
  (** Fetch an object into memory as bytes, bounded by [max_bytes]. *)

  val find :
    connection ->
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
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (string Object.Get.result option, Awskit.Error.t) result io
  (** Return and consume an object as a bounded in-memory string when it is
      present. *)

  val find_bytes :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Get.options ->
    max_bytes:int64 ->
    unit ->
    (bytes Object.Get.result option, Awskit.Error.t) result io
  (** Return and consume an object as bounded in-memory bytes when it is
      present. *)

  val head :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.result, Awskit.Error.t) result io
  (** Fetch object metadata without reading an object body. *)

  val find_metadata :
    connection ->
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
    connection ->
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
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Object.Delete.options ->
    unit ->
    (Object.Delete.result, Awskit.Error.t) result io
  (** Delete an object or a specific object version. *)

  val delete_objects :
    connection ->
    bucket:Bucket_name.t ->
    objects:Object.Delete_many.object_ list ->
    ?options:Object.Delete_many.options ->
    unit ->
    (Object.Delete_many.result, Awskit.Error.t) result io
  (** Delete multiple objects with [DeleteObjects].

      Per-object failures are represented in [Object.Delete_many.result.errors]
      even when the operation itself returns [Ok]. *)

  val copy :
    connection ->
    source_bucket:Bucket_name.t ->
    source_key:Object_key.t ->
    destination_bucket:Bucket_name.t ->
    destination_key:Object_key.t ->
    ?options:Object.Copy.options ->
    unit ->
    (Object.Copy.result, Awskit.Error.t) result io
  (** Copy an object from one bucket/key to another. *)

  val list_versions :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Object.Versions.options ->
    unit ->
    (Object.Versions.page, Awskit.Error.t) result io
  (** Fetch one [ListObjectVersions] page. Use {!module:Versions} helpers to
      follow pagination. *)

  val list :
    connection ->
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
      connection ->
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
      connection ->
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
      connection ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      max_pages:int ->
      unit ->
      (Object.List.page list, Awskit.Error.t) result io
    (** Collect listing pages up to the explicit [max_pages] bound.

        Returns an error if S3 reports more pages than the bound allows. *)

    val objects :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Object.List.options ->
      max_pages:int ->
      unit ->
      (Object.List.object_summary list, Awskit.Error.t) result io
    (** Collect object summaries across listing pages up to [max_pages]. *)

    val keys :
      connection ->
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
      connection ->
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
      connection ->
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
      connection ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      max_pages:int ->
      unit ->
      (Object.Versions.page list, Awskit.Error.t) result io
    (** Collect version listing pages up to the explicit [max_pages] bound.

        Returns an error if S3 reports more pages than the bound allows. *)

    val object_versions :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Object.Versions.options ->
      max_pages:int ->
      unit ->
      (Object.Versions.object_version list, Awskit.Error.t) result io
    (** Collect object-version entries across pages up to [max_pages]. *)

    val delete_markers :
      connection ->
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
      connection ->
      bucket:Bucket_name.t ->
      key:Object_key.t ->
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Awskit.Error.t) result io
    (** Fetch object tags. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      key:Object_key.t ->
      ?options:Object.Tagging.options ->
      tags:Tag.Set.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the object's tag set. *)

    val delete :
      connection ->
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

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  val create :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Create.options ->
    unit ->
    (Bucket.Create.result, Awskit.Error.t) result io
  (** Create a bucket. *)

  val delete :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Delete.options ->
    unit ->
    (Bucket.Delete.result, Awskit.Error.t) result io
  (** Delete an empty bucket. *)

  val head :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Head.options ->
    unit ->
    (Bucket.Head.result, Awskit.Error.t) result io
  (** Check bucket existence and return metadata such as the region hint. *)

  val exists :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Head.options ->
    unit ->
    (bool, Awskit.Error.t) result io
  (** Return [false] for S3 not-found responses and [true] for success. *)

  val list :
    connection -> (Bucket.List_buckets.result, Awskit.Error.t) result io
  (** List buckets visible to the credentials. *)

  val get_location :
    connection ->
    bucket:Bucket_name.t ->
    ?options:Bucket.Get_location.options ->
    unit ->
    (Bucket.Get_location.result, Awskit.Error.t) result io
  (** Fetch the bucket location constraint/region. *)

  module Policy : sig
    (** Bucket policy operations. Policy documents are opaque validated JSON. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      unit ->
      (Policy.t, Awskit.Error.t) result io
    (** Fetch a bucket policy document. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      policy:Policy.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket policy document. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Policy.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete the bucket policy. *)
  end

  module Versioning : sig
    (** Bucket versioning operations. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Versioning.options ->
      unit ->
      (Bucket.Versioning.result, Awskit.Error.t) result io
    (** Fetch bucket versioning state. *)

    val put :
      connection ->
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
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      unit ->
      (Bucket.Tagging.result, Awskit.Error.t) result io
    (** Fetch the bucket tag set. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      tags:Tag.Set.t ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace the bucket tag set. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Tagging.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Remove all bucket tags. *)
  end

  module Encryption : sig
    (** Bucket default-encryption operations. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      unit ->
      (Bucket.Encryption.result, Awskit.Error.t) result io
    (** Fetch bucket default-encryption configuration. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      config:Bucket.Encryption.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket default-encryption configuration. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Encryption.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket default-encryption configuration. *)
  end

  module Cors : sig
    (** Bucket CORS operations. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      unit ->
      (Bucket.Cors.result, Awskit.Error.t) result io
    (** Fetch bucket CORS configuration. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      config:Bucket.Cors.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace bucket CORS configuration. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Cors.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete bucket CORS configuration. *)
  end

  module Public_access_block : sig
    (** Bucket public-access-block operations. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      unit ->
      (Bucket.Public_access_block.result, Awskit.Error.t) result io
    (** Fetch public-access-block configuration. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      config:Bucket.Public_access_block.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace public-access-block configuration. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Public_access_block.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Delete public-access-block configuration. *)
  end

  module Ownership_controls : sig
    (** Bucket ownership-controls operations. *)

    val get :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
      unit ->
      (Bucket.Ownership_controls.result, Awskit.Error.t) result io
    (** Fetch ownership-controls configuration. *)

    val put :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
      config:Bucket.Ownership_controls.config ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
    (** Replace ownership-controls configuration. *)

    val delete :
      connection ->
      bucket:Bucket_name.t ->
      ?options:Bucket.Ownership_controls.options ->
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
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Multipart.Create.options ->
    unit ->
    (Multipart.Create.result, Awskit.Error.t) result io
  (** Start a multipart upload and return its upload handle. *)

  val upload_part :
    connection ->
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
    connection ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.Complete.options ->
    parts:Multipart.Part.t list ->
    unit ->
    (Multipart.Complete.result, Awskit.Error.t) result io
  (** Complete a multipart upload using the supplied completed part list. *)

  val abort_upload :
    connection ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.Abort.options ->
    unit ->
    (Multipart.Abort.result, Awskit.Error.t) result io
  (** Abort a multipart upload. *)

  val list_parts :
    connection ->
    upload:_ Multipart.Upload.t ->
    ?options:Multipart.List_parts.options ->
    unit ->
    (Multipart.List_parts.page, Awskit.Error.t) result io
  (** Fetch one [ListParts] page. Use [List_parts] helpers to follow pagination.
  *)

  module List_parts : sig
    (** Pagination helpers for [ListParts]. *)

    val fold_pages :
      connection ->
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
      connection ->
      upload:_ Multipart.Upload.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.page list, Awskit.Error.t) result io
    (** Collect uploaded-part pages.

        Returns an error if [max_pages] is supplied and S3 reports more pages
        than the bound allows. *)

    val parts :
      connection ->
      upload:_ Multipart.Upload.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.part_info list, Awskit.Error.t) result io
    (** Collect uploaded part summaries across pages.

        Returns an error if [max_pages] is supplied and S3 reports more pages
        than the bound allows. *)
  end
end

module type PRESIGNED = sig
  (** Presigned request artifact helpers bound to a client connection. *)

  type connection
  (** Client connection handle. *)

  type +'a io
  (** Runtime effect type. *)

  val get_object :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [GET Object] request artifact. *)

  val put_object :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [PUT Object] request artifact. Headers returned in
      the result must be sent by the eventual uploader. *)

  val head_object :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Head_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [HEAD Object] request artifact. *)

  val delete_object :
    connection ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [DELETE Object] request artifact. *)

  val upload_part :
    connection ->
    upload:_ Multipart.Upload.t ->
    part_number:Multipart.Part_number.t ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
  (** Generate a presigned [UploadPart] request artifact for one multipart part.
  *)
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

  (** Request-body constructors for this runtime adapter. *)
  module Body : BODY with type 'a io = 'a io and type t = request_body

  (** Response-body readers for object [consume] callbacks. *)
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

module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region

module Error : sig
  type t = Awskit.Error.t
  (** Structured Awskit error value. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an error. *)

  val equal : t -> t -> bool
  (** Compare two errors structurally. *)

  val to_string_hum : t -> string
  (** Render an error for humans. *)

  val service_code : t -> string option
  (** Return the S3 service error code, when the error came from a modeled
      service response. *)

  val is_not_found : t -> bool
  (** Return [true] for S3 not-found errors recognized by lookup helpers. *)

  val is_no_such_bucket : t -> bool
  (** Return [true] for [NoSuchBucket] service errors. *)

  val is_no_such_key : t -> bool
  (** Return [true] for [NoSuchKey] service errors. *)

  val is_precondition_failed : t -> bool
  (** Return [true] for failed conditional request preconditions. *)

  val is_conditional_request_conflict : t -> bool
  (** Return [true] for S3 conditional-write conflicts. *)

  val is_conditional_failure : t -> bool
  (** Return [true] for any conditional request failure recognized by S3. *)
end

module Bucket_name = Bucket_name
module Object_key = Object_key
module Account_id = Account_id
module Content_type = Content_type
module Header_value = Header_value
module Metadata = Metadata
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Endpoint_config = Endpoint_config
module Endpoint_resolver = Endpoint_resolver

type addressing_style = Endpoint_config.addressing_style
type endpoint_variant = Endpoint_config.endpoint_variant
type endpoint_config = Endpoint_resolver.t

val endpoint_config :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  unit ->
  endpoint_config
(** Build an AWS S3 endpoint resolver from addressing and endpoint variant
    preferences.

    Use {!Awskit_s3.Endpoint_config.s3_compatible},
    {!Awskit_s3.Endpoint_config.local_plaintext}, or
    {!Awskit_s3.Endpoint_config.unsafe_plaintext} before passing
    [~endpoint_config] to an adapter when targeting an explicit non-AWS
    endpoint. *)

val default_endpoint_config : endpoint_config
(** Default AWS S3 regional HTTPS endpoint resolver. *)

module Object = Object
module Bucket = Bucket
module Multipart = Multipart
module Transfer = Transfer
module Policy = Policy
module Presigned = Presigned

module Make (R : RUNTIME) :
  S
    with type connection = R.connection
     and type 'a io = 'a R.t
     and type request_body = R.request_body
     and type response_body_reader = R.response_body_reader
