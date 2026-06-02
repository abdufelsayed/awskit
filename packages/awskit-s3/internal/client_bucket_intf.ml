open Common
open Client_data_intf

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
