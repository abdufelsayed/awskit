(** Ready-to-use Lwt + Unix S3 adapter.

    Primitive operations stream through {!Awskit_lwt_unix.Runtime}. Local-path
    helpers live under {!Object.Transfer}. *)

type t

module Runtime :
  Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t and type connection = t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?endpoint:Awskit.Endpoint.t ->
  ?addressing_style:Awskit_s3.addressing_style ->
  ?endpoint_variant:Awskit_s3.endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  ?region:Awskit.Region.t ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?imdsv1_fallback:Awskit_lwt_unix.Credentials.imdsv1_fallback ->
  unit ->
  (t, Awskit_s3.Error.t) result

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader

  module Transfer : sig
    val upload_from_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Put_object.options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Put_object.result, Awskit_s3.Error.t) result Lwt.t
    (** Stream a local file to S3. [on_progress], when provided, receives the
        cumulative number of bytes written to the request body. *)

    val upload_multipart_from_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Multipart.Managed.options ->
      ?concurrency:int ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Multipart.Managed.result, Awskit_s3.Error.t) result Lwt.t
    (** Upload a local file with S3 multipart upload. [concurrency] defaults to
        [4]. The helper aborts the multipart upload when a fresh upload fails.
    *)

    val resume_multipart_upload_from_path :
      t ->
      bucket:string ->
      key:string ->
      upload_id:Awskit_s3.Multipart.Upload_id.t ->
      ?options:Awskit_s3.Multipart.Managed.options ->
      ?concurrency:int ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Multipart.Managed.result, Awskit_s3.Error.t) result Lwt.t
    (** Resume an existing multipart upload by listing uploaded parts, skipping
        matching part numbers and sizes, uploading missing parts, and completing
        the upload. Existing uploads are not aborted on failure. *)

    val download_to_path :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Get_object.options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Get_object.result, Awskit_s3.Error.t) result Lwt.t
    (** Stream an S3 object to a local file. [on_progress], when provided,
        receives the cumulative number of bytes written to disk. *)
  end
end

module Bucket :
  Awskit_s3.BUCKET with type connection := t and type 'a io := 'a Lwt.t

module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a Lwt.t
     and type request_body := Runtime.request_body

module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
