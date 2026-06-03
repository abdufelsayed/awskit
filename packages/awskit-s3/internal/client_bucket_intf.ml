open Common
open Client_data_intf

module type BUCKET = sig
  type connection
  (** Runtime-backed S3 bucket operations and bucket configuration APIs. *)

  type +'a io

  val create :
    connection ->
    bucket:string ->
    ?options:Create_bucket.options ->
    unit ->
    (Create_bucket.result, Error.t) result io
  (** Create a bucket. Region-specific create options are carried in
      [Create_bucket.options]. *)

  val delete :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Delete_bucket.result, Error.t) result io
  (** Delete an empty bucket. *)

  val head :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Head_bucket.result, Error.t) result io
  (** Fetch bucket existence and region metadata. *)

  val exists :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (bool, Error.t) result io
  (** Return [Ok true] when the bucket exists, [Ok false] for not-found
      responses, and [Error] for other failures. *)

  val list : connection -> (List_buckets.result, Error.t) result io
  (** List buckets visible to the configured credentials. *)

  val get_location :
    ?expected_bucket_owner:string ->
    connection ->
    bucket:string ->
    (Get_bucket_location.result, Error.t) result io
  (** Fetch the bucket location constraint. *)

  module Policy : sig
    (** Bucket policy operations. Policy JSON is represented by opaque
        {!Awskit_s3.Policy.t}. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Policy.t, Error.t) result io
    (** Fetch the bucket policy document. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Policy.t ->
      (Awskit.Response.t, Error.t) result io
    (** Replace the bucket policy document. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete the bucket policy document. *)
  end

  module Versioning : sig
    (** Bucket versioning operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Versioning.result, Error.t) result io
    (** Fetch bucket versioning state. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Versioning.Status.t ->
      (Awskit.Response.t, Error.t) result io
    (** Set bucket versioning to [Enabled] or [Suspended]. *)
  end

  module Tagging : sig
    (** Bucket tag operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Tagging.result, Error.t) result io
    (** Fetch bucket tags. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Tag.t list ->
      (Awskit.Response.t, Error.t) result io
    (** Replace the bucket's full tag set. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete all bucket tags. *)
  end

  module Encryption : sig
    (** Bucket default-encryption operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Encryption.result, Error.t) result io
    (** Fetch default encryption configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Encryption.config ->
      (Awskit.Response.t, Error.t) result io
    (** Replace default encryption configuration. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete default encryption configuration. *)
  end

  module Cors : sig
    (** Bucket CORS operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Cors.result, Error.t) result io
    (** Fetch bucket CORS configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Cors.config ->
      (Awskit.Response.t, Error.t) result io
    (** Replace bucket CORS configuration. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete bucket CORS configuration. *)
  end

  module Public_access_block : sig
    (** Bucket public-access-block operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Public_access_block.result, Error.t) result io
    (** Fetch public-access-block configuration. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Public_access_block.config ->
      (Awskit.Response.t, Error.t) result io
    (** Replace public-access-block configuration. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete public-access-block configuration. *)
  end

  module Ownership_controls : sig
    (** Bucket ownership-controls operations. *)
    val get :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Bucket.Ownership_controls.result, Error.t) result io
    (** Fetch object ownership controls. *)

    val put :
      connection ->
      bucket:string ->
      ?expected_bucket_owner:string ->
      Bucket.Ownership_controls.config ->
      (Awskit.Response.t, Error.t) result io
    (** Replace object ownership controls. *)

    val delete :
      ?expected_bucket_owner:string ->
      connection ->
      bucket:string ->
      (Awskit.Response.t, Error.t) result io
    (** Delete object ownership controls. *)
  end
end
