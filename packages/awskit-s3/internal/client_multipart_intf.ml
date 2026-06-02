open Common
open Client_data_intf

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
    ?options:Complete_multipart_upload.options ->
    Multipart.Part.t list ->
    (Complete_multipart_upload.result, Error.t) result io

  val abort_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Abort_multipart_upload.options ->
    unit ->
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
end
