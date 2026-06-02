open Common
include Data_intf

open struct
  module Endpoint_resolver = Endpoint_resolver
  module Object = Object
  module Bucket = Bucket
  module Multipart = Multipart
  module Transfer = Transfer
  module Policy = Policy
  module Presigned = Presigned
end

include Operation_data

module type MULTIPART_DATA = sig
  module Upload_id : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
  end

  module Upload : sig
    type t = private { bucket : string; key : string; upload_id : Upload_id.t }

    val create :
      bucket:string ->
      key:string ->
      upload_id:Upload_id.t ->
      (t, Error.t) result

    val create_exn : bucket:string -> key:string -> upload_id:Upload_id.t -> t
  end

  module Part : sig
    type t = private {
      part_number : int;
      etag : Object.Etag.t;
      checksum : Object.Checksum.value option;
    }

    val create :
      ?checksum:Object.Checksum.value ->
      part_number:int ->
      etag:Object.Etag.t ->
      unit ->
      (t, Error.t) result

    val create_exn :
      ?checksum:Object.Checksum.value ->
      part_number:int ->
      etag:Object.Etag.t ->
      unit ->
      t
  end
end

module type TRANSFER_DATA = sig
  val min_part_size : int
  val default_part_size : int
  val default_multipart_threshold : int64
  val default_concurrency : int
  val max_parts : int

  type upload_options = {
    multipart_threshold : int64;
    part_size : int;
    concurrency : int;
    put_options : Put_object.options;
    create_options : Create_multipart_upload.options;
    upload_part_options : Upload_part.options;
    complete_options : Complete_multipart_upload.options;
    abort_options : Abort_multipart_upload.options;
    list_parts_options : List_parts.options;
  }

  type download_options = {
    multipart_threshold : int64;
    part_size : int;
    concurrency : int;
    get_options : Get_object.options;
  }

  type upload_strategy = [ `Put | `Multipart ]
  type download_strategy = [ `Get | `Ranged ]

  type multipart_upload_result = {
    upload : Multipart.Upload.t;
    parts : Multipart.Part.t list;
    complete : Complete_multipart_upload.result;
  }

  type upload_result =
    | Put of Put_object.result
    | Multipart of multipart_upload_result

  type download_result =
    | Get of Get_object.result
    | Ranged of { info : Head_object.result; parts : int }

  val upload_strategy : upload_result -> upload_strategy
  val download_strategy : download_result -> download_strategy
  val default_upload_options : upload_options
  val default_download_options : download_options
  val validate_upload_options : upload_options -> (unit, Error.t) Stdlib.result

  val validate_upload_multipart_selection :
    upload_options -> (unit, Error.t) Stdlib.result

  val validate_download_options :
    download_options -> (unit, Error.t) Stdlib.result

  val validate_multipart_part_count :
    content_length:int64 -> part_size:int -> (unit, Error.t) Stdlib.result
end

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type endpoint_config = Endpoint_resolver.t

let endpoint_config ?addressing_style ?endpoint_variant ?scheme ?endpoint () =
  Endpoint_resolver.create ?addressing_style ?endpoint_variant ?scheme ?endpoint
    ()

let default_endpoint_config = Endpoint_resolver.default

module type RUNTIME = sig
  include Awskit.Runtime.S

  val s3_endpoint_config : connection -> endpoint_config
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
    ?options:Create_bucket.options ->
    unit ->
    (Create_bucket.result, Error.t) result io

  val delete :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Delete_bucket.result, Error.t) result io

  val head :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Head_bucket.result, Error.t) result io

  val exists :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (bool, Error.t) result io

  val list : connection -> (List_buckets.result, Error.t) result io

  val get_location :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Get_bucket_location.result, Error.t) result io

  module Policy : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Policy.t, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Policy.t ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
  end

  module Versioning : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Versioning.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Versioning.Status.t ->
      (Awskit.Response.t, Error.t) result io
  end

  module Tagging : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Tagging.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
  end

  module Encryption : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Encryption.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Encryption.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
  end

  module Cors : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Cors.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Cors.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
  end

  module Public_access_block : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Public_access_block.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Public_access_block.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
  end

  module Ownership_controls : sig
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Ownership_controls.result, Error.t) result io

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Ownership_controls.config ->
      (Awskit.Response.t, Error.t) result io

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
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
    ?options:Presigned.Delete_object.options ->
    unit ->
    (Presigned.result, Error.t) result io

  val upload_part :
    connection ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Presigned.Upload_part.options ->
    unit ->
    (Presigned.result, Error.t) result io
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
