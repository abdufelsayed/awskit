(** Ready-to-use Lwt + Unix S3 adapter.

    Primitive operations stream through {!Awskit_lwt_unix.Runtime}. Local-path
    helpers live under {!Object.Transfer}. *)

type t

module Runtime :
  Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t and type connection = t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?provider:Awskit_s3.Provider.t ->
  ?region:Awskit.Region.t ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  unit ->
  (t, Awskit_s3.Error.t) result

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type upload_body := Runtime.upload_body
       and type download_reader := Runtime.download_reader

  module Transfer : sig
    val upload_from_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Object.Put.options ->
      path:string ->
      unit ->
      (Awskit_s3.Object.Put.result, Awskit_s3.Error.t) result Lwt.t

    val download_to_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Object.Get.options ->
      path:string ->
      unit ->
      (Awskit_s3.Object.Get.info, Awskit_s3.Error.t) result Lwt.t
  end
end

module Bucket :
  Awskit_s3.BUCKET with type connection := t and type 'a io := 'a Lwt.t

module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a Lwt.t
     and type upload_body := Runtime.upload_body

module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
