open Awskit_s3_common
include Awskit_s3_data_intf

open struct
  module Provider = Awskit_s3_provider
  module Object = Awskit_s3_object
  module Bucket = Awskit_s3_bucket
  module Multipart = Awskit_s3_multipart
  module Policy = Awskit_s3_policy
  module Presigned = Awskit_s3_presigned
end

module type RUNTIME = sig
  include Awskit.Runtime.S

  val s3_provider : connection -> Provider.t
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
