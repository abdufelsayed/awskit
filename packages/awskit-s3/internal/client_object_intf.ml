open Common
open Client_data_intf

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
    ?options:Delete_objects.options ->
    unit ->
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
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Awskit.Response.t, Error.t) result io
  end
end
