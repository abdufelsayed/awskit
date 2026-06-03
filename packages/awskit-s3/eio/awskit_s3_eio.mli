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
  ?endpoint:Awskit.Endpoint.t ->
  ?addressing_style:Awskit_s3.addressing_style ->
  ?endpoint_variant:Awskit_s3.endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  unit ->
  t
(** Create an Eio S3 client.

    [region] and [credentials] are explicit. [endpoint] overrides the generated
    AWS S3 endpoint for local services or custom endpoints. [addressing_style],
    [endpoint_variant], and [scheme] configure S3 endpoint resolution when no
    explicit endpoint is supplied. [retry_policy] defaults to
    {!val:Awskit.Retry.default}. *)

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader

  module Transfer : sig
    val put_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Put_object.options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Put_object.result, Awskit_s3.Error.t) result
    (** Stream a local file to S3. [on_progress], when provided, receives the
        cumulative number of bytes written to the request body. *)

    val get_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Get_object.options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Get_object.result, Awskit_s3.Error.t) result
    (** Stream an S3 object to a local file. [on_progress], when provided,
        receives the cumulative number of bytes written to disk. *)

    val upload_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.upload_result, Awskit_s3.Error.t) result
    (** Upload a local file, using [PutObject] below the multipart threshold and
        multipart upload at or above it. *)

    val download_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.download_options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.download_result, Awskit_s3.Error.t) result
    (** Download an object to a local file, using ranged [GetObject] requests at
        or above the multipart threshold. *)

    val multipart_upload_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
    (** Upload a local file with S3 multipart upload. The helper aborts the
        multipart upload when a fresh upload fails. *)

    val resume_multipart_upload_file :
      t ->
      bucket:string ->
      key:string ->
      upload_id:Awskit_s3.Multipart.Upload_id.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
    (** Resume an existing multipart upload by listing uploaded parts, skipping
        matching part numbers and sizes, uploading missing parts, and completing
        the upload. Existing uploads are not aborted on failure. *)
  end
end

(** Bucket operations using direct-style Eio results. *)
module Bucket : Awskit_s3.BUCKET with type connection := t and type 'a io := 'a

(** Multipart operations using direct-style Eio results. *)
module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a
     and type request_body := Runtime.request_body

(** Presigned URL helpers using the client's region, credentials, clock, and
    endpoint configuration. *)
module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a
