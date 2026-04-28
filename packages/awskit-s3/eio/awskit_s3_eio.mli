(** Eio S3 adapter.

    Primitive operations are direct-style and stream through
    {!Awskit_eio.Runtime}. Local-path helpers live under {!Object.Transfer}. *)

type t

module Runtime : Awskit_s3.RUNTIME with type 'a t = 'a and type connection = t

val create :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  ?retry_policy:Awskit.Retry.t ->
  ?provider:Awskit_s3.Provider.t ->
  unit ->
  t
(** Create an Eio S3 client. [retry_policy] defaults to
    {!val:Awskit.Retry.default}. *)

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a
       and type upload_body := Runtime.upload_body
       and type download_reader := Runtime.download_reader

  module Transfer : sig
    val upload_from_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Object.Put.options ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Object.Put.result, Awskit_s3.Error.t) result

    val download_to_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Object.Get.options ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Object.Get.info, Awskit_s3.Error.t) result
  end
end

module Bucket : Awskit_s3.BUCKET with type connection := t and type 'a io := 'a

module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a
     and type upload_body := Runtime.upload_body

module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a
