module type RUNTIME = sig
  include Awskit.Runtime.S

  val s3_endpoint_config : connection -> Endpoint_resolver.t
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
    ?options:Object.Put.options ->
    body:request_body ->
    unit ->
    (Object.Put.result, Awskit.Error.t) result io

  val get :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Get.options ->
    consume:(response_body_reader -> ('a, Awskit.Error.t) result io) ->
    unit ->
    (Object.Get.result * 'a, Awskit.Error.t) result io

  val head :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Head.options ->
    unit ->
    (Object.Head.result, Awskit.Error.t) result io

  val exists :
    connection ->
    bucket:string ->
    key:string ->
    (bool, Awskit.Error.t) result io

  val delete :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Delete.options ->
    unit ->
    (Object.Delete.result, Awskit.Error.t) result io

  val delete_objects :
    connection ->
    bucket:string ->
    objects:Object.Delete_many.object_ list ->
    ?options:Object.Delete_many.options ->
    unit ->
    (Object.Delete_many.result, Awskit.Error.t) result io

  val copy :
    connection ->
    source_bucket:string ->
    source_key:string ->
    destination_bucket:string ->
    destination_key:string ->
    ?options:Object.Copy.options ->
    unit ->
    (Object.Copy.result, Awskit.Error.t) result io

  val list_versions :
    connection ->
    bucket:string ->
    ?options:Object.Versions.options ->
    unit ->
    (Object.Versions.page, Awskit.Error.t) result io

  val list :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (Object.List.page, Awskit.Error.t) result io

  val list_keys :
    connection ->
    bucket:string ->
    ?options:Object.List.options ->
    unit ->
    (string list, Awskit.Error.t) result io

  module List_objects_v2 : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.List.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.page list, Awskit.Error.t) result io

    val objects :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (Object.List.object_summary list, Awskit.Error.t) result io

    val keys :
      connection ->
      bucket:string ->
      ?options:Object.List.options ->
      ?max_pages:int ->
      unit ->
      (string list, Awskit.Error.t) result io
  end

  module List_object_versions : sig
    val fold_pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      init:'acc ->
      f:('acc -> Object.Versions.page -> ('acc, Awskit.Error.t) result io) ->
      unit ->
      ('acc, Awskit.Error.t) result io

    val pages :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.page list, Awskit.Error.t) result io

    val object_versions :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.object_version list, Awskit.Error.t) result io

    val delete_markers :
      connection ->
      bucket:string ->
      ?options:Object.Versions.options ->
      ?max_pages:int ->
      unit ->
      (Object.Versions.delete_marker list, Awskit.Error.t) result io
  end

  val put_string :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Put.options ->
    string ->
    (Object.Put.result, Awskit.Error.t) result io

  val put_bytes :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Object.Put.options ->
    bytes ->
    (Object.Put.result, Awskit.Error.t) result io

  val get_as_string :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Object.Get.options ->
    unit ->
    (Object.Get.result * string, Awskit.Error.t) result io

  val get_as_bytes :
    connection ->
    bucket:string ->
    key:string ->
    max_bytes:int64 ->
    ?options:Object.Get.options ->
    unit ->
    (Object.Get.result * bytes, Awskit.Error.t) result io

  module Tagging : sig
    val get :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Object.Tagging.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      Tag.t list ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      connection ->
      bucket:string ->
      key:string ->
      ?options:Object.Tagging.options ->
      unit ->
      (Awskit.Response.t, Awskit.Error.t) result io
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
    (Bucket.Create.result, Awskit.Error.t) result io

  val delete :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Bucket.Delete.result, Awskit.Error.t) result io

  val head :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Bucket.Head.result, Awskit.Error.t) result io

  val exists :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (bool, Awskit.Error.t) result io

  val list :
    connection -> (Bucket.List_buckets.result, Awskit.Error.t) result io

  val get_location :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Bucket.Get_location.result, Awskit.Error.t) result io

  module Policy : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Policy.t, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Policy.t ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Versioning : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Versioning.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Versioning.Status.t ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Tagging : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Tagging.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Tag.t list ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Encryption : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Encryption.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Encryption.config ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Cors : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Cors.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Cors.config ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Public_access_block : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Public_access_block.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Public_access_block.config ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
  end

  module Ownership_controls : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Ownership_controls.result, Awskit.Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Ownership_controls.config ->
      (Awskit.Response.t, Awskit.Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Awskit.Error.t) result io
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
    ?options:Multipart.Create.options ->
    unit ->
    (Multipart.Create.result, Awskit.Error.t) result io

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

  val complete_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.Complete.options ->
    Multipart.Part.t list ->
    (Multipart.Complete.result, Awskit.Error.t) result io

  val abort_upload :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.Abort.options ->
    unit ->
    (Multipart.Abort.result, Awskit.Error.t) result io

  val list_parts :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    ?options:Multipart.List_parts.options ->
    unit ->
    (Multipart.List_parts.page, Awskit.Error.t) result io

  module List_parts : sig
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

    val pages :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.page list, Awskit.Error.t) result io

    val parts :
      connection ->
      bucket:string ->
      key:string ->
      upload_id:Multipart.Upload_id.t ->
      ?options:Multipart.List_parts.options ->
      ?max_pages:int ->
      unit ->
      (Multipart.List_parts.part_info list, Awskit.Error.t) result io
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
    (Presigned.result, Awskit.Error.t) result io

  val put_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Put_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io

  val head_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Get_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io

  val delete_object :
    connection ->
    bucket:string ->
    key:string ->
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Awskit.Error.t) result io
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
    ?endpoint:Endpoint.t ->
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
