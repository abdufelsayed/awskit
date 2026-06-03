open Common
open Client_data_intf

module type PRESIGNED = sig
  type connection
  (** Runtime-backed presigned URL helpers. The connection supplies region,
      credentials, current time, and S3 endpoint configuration. *)

  type +'a io

  val get_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Error.t) result io
  (** Generate a presigned [GET Object] URL. *)

  val put_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Error.t) result io
  (** Generate a presigned [PUT Object] URL. *)

  val head_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Error.t) result io
  (** Generate a presigned [HEAD Object] URL. *)

  val delete_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Error.t) result io
  (** Generate a presigned [DELETE Object] URL. *)

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Error.t) result io
  (** Generate a presigned [UploadPart] URL for one multipart part. *)
end
