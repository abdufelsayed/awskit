open Common
open Client_data_intf

module type MULTIPART = sig
  type connection
  (** Runtime-backed S3 multipart upload operations. High-level file helpers in
      adapter packages build on this lower-level lifecycle. *)

  type +'a io
  type request_body

  val create_upload :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Create_multipart_upload.options ->
    unit ->
    (Create_multipart_upload.result, Error.t) result io
  (** Start a multipart upload and return an upload id. *)

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
  (** Upload one numbered part. The request body must have a known content
      length. *)

  val complete_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Complete_multipart_upload.options ->
    Multipart.Part.t list ->
    (Complete_multipart_upload.result, Error.t) result io
  (** Complete an upload using the parts returned by successful
      {!val:upload_part} calls. *)

  val abort_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Abort_multipart_upload.options ->
    unit ->
    (Abort_multipart_upload.result, Error.t) result io
  (** Abort a multipart upload and ask S3 to discard uploaded parts. *)

  val list_parts :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:List_parts.options ->
    unit ->
    (List_parts.page, Error.t) result io
  (** Return one [ListParts] page. *)

  module List_parts : sig
    (** Pagination helpers for [ListParts]. *)
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
    (** Fold part pages until S3 has no next marker, [max_pages] is reached, or
        [f] returns an error. *)

    val pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:List_parts.options ->
      ?max_pages:int ->
      unit ->
      (List_parts.page list, Error.t) result io
    (** Collect [ListParts] pages up to [max_pages]. *)

    val parts :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:List_parts.options ->
      ?max_pages:int ->
      unit ->
      (List_parts.part_info list, Error.t) result io
    (** Collect listed part metadata across pages. *)
  end
end
