open Common
open Client_data_intf

module type OBJECT = sig
  type connection
  (** Runtime-backed S3 object operations.

      Primitive operations stream request and response bodies through the
      selected runtime. Convenience helpers for strings/bytes are intentionally
      bounded or in-memory so callers can choose the right ownership model. *)

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
  (** Upload an object with a runtime-owned request body. The body descriptor is
      used for content length, signing, and retry replayability. *)

  val get :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    consume:(response_body_reader -> ('a, Error.t) result io) ->
    unit ->
    (Get_object.result * 'a, Error.t) result io
  (** Fetch an object and pass the response body reader to [consume].

      The reader is scoped to the callback. The operation returns response
      metadata together with the callback result. *)

  val head :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Head_object.options ->
    unit ->
    (Head_object.result, Error.t) result io
  (** Fetch object metadata without reading the object body. *)

  val exists :
    connection -> bucket:string -> key:string -> (bool, Error.t) result io
  (** Return [Ok true] when the current object exists, [Ok false] for not-found
      responses, and [Error] for other failures. *)

  val delete :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Delete_object.options ->
    unit ->
    (Delete_object.result, Error.t) result io
  (** Delete the current object or a specific version when options include a
      version id. *)

  val delete_objects :
    connection ->
    bucket:string ->
    objects:Delete_objects.object_ list ->
    ?options:Delete_objects.options ->
    unit ->
    (Delete_objects.result, Error.t) result io
  (** Delete multiple objects in one S3 [DeleteObjects] request. Per-object
      failures are returned in the result payload. *)

  val copy :
    connection ->
    source_bucket:string ->
    source_key:string ->
    destination_bucket:string ->
    destination_key:string ->
    ?options:Copy_object.options ->
    unit ->
    (Copy_object.result, Error.t) result io
  (** Copy an object, optionally replacing metadata, setting storage class, or
      applying source preconditions. *)

  val list_versions :
    connection ->
    bucket:string ->
    ?options:List_object_versions.options ->
    unit ->
    (List_object_versions.page, Error.t) result io
  (** Return one [ListObjectVersions] page. Use {!module:List_object_versions}
      helpers to follow pagination markers. *)

  val list :
    connection ->
    bucket:string ->
    ?options:List_objects_v2.options ->
    unit ->
    (List_objects_v2.page, Error.t) result io
  (** Return one [ListObjectsV2] page. Use {!module:List_objects_v2} helpers to
      follow continuation tokens. *)

  val list_keys :
    connection ->
    bucket:string ->
    ?options:List_objects_v2.options ->
    unit ->
    (string list, Error.t) result io
  (** Return keys from a single [ListObjectsV2] page. For all pages, use
      {!val:List_objects_v2.keys}. *)

  module List_objects_v2 : sig
    (** Pagination helpers for [ListObjectsV2]. Each helper preserves the
        supplied options and updates only continuation tokens between pages. *)
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> List_objects_v2.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io
    (** Fold pages until S3 stops returning continuation tokens, [max_pages] is
        reached, or [f] returns an error. *)

    val pages :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (List_objects_v2.page list, Error.t) result io
    (** Collect pages up to [max_pages]. *)

    val objects :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (List_objects_v2.object_summary list, Error.t) result io
    (** Collect object summaries across pages. *)

    val keys :
      connection ->
      bucket:string ->
      ?options:List_objects_v2.options ->
      ?max_pages:int ->
      unit ->
      (string list, Error.t) result io
    (** Collect object keys across pages. *)
  end

  module List_object_versions : sig
    (** Pagination helpers for [ListObjectVersions]. Marker fields are updated
        between requests while other options are preserved. *)
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> List_object_versions.page -> ('acc, Error.t) result io) ->
      unit ->
      ('acc, Error.t) result io
    (** Fold version-listing pages until S3 has no next markers, [max_pages] is
        reached, or [f] returns an error. *)

    val pages :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.page list, Error.t) result io
    (** Collect version-listing pages up to [max_pages]. *)

    val object_versions :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.object_version list, Error.t) result io
    (** Collect object version entries across pages. *)

    val delete_markers :
      connection ->
      bucket:string ->
      ?options:List_object_versions.options ->
      ?max_pages:int ->
      unit ->
      (List_object_versions.delete_marker list, Error.t) result io
    (** Collect delete marker entries across pages. *)
  end

  val put_string :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    string ->
    (Put_object.result, Error.t) result io
  (** Upload an in-memory string as a replayable request body. *)

  val put_bytes :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    bytes ->
    (Put_object.result, Error.t) result io
  (** Upload in-memory bytes as a replayable request body. *)

  val get_as_string :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Get_object.options ->
    unit ->
    (Get_object.result * string, Error.t) result io
  (** Buffer an object body into a string, failing with a body-limit error when
      the response exceeds [max_bytes]. *)

  val get_as_bytes :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Get_object.options ->
    unit ->
    (Get_object.result * bytes, Error.t) result io
  (** Buffer an object body into bytes, failing with a body-limit error when the
      response exceeds [max_bytes]. *)

  module Tagging : sig
    (** Object tag operations. *)
    val get :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Error.t) result io
    (** Fetch object tags. *)

    val put :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io
    (** Replace the object's full tag set. *)

    val delete :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Awskit.Response.t, Error.t) result io
    (** Remove all tags from the object. *)
  end
end
