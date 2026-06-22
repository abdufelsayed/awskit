(** Eio S3 adapter.

    Operations are direct-style and stream through {!Awskit_eio.Runtime}.
    Local-path body and reader helpers live under {!Body} and {!Reader}; managed
    upload/download helpers live under {!Object.Transfer}. *)

type t
(** Eio S3 client connection. Create with {!val:create}. *)

(** Direct-style S3 runtime used by [Awskit_s3.Make]. *)
module Runtime : Awskit_s3.RUNTIME with type 'a t = 'a and type connection = t

val create :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  https:'flow Awskit_eio.https ->
  region:string ->
  credentials:Awskit.Credentials.t ->
  ?retry_policy:Awskit.Retry.t ->
  ?endpoint_config:Awskit_s3.Endpoint_config.t ->
  unit ->
  (t, Awskit_s3.Error.t) result
(** Create an Eio S3 client.

    [https] is forwarded to [Awskit_eio.create]. Use [Awskit_eio.http_only] only
    for plain HTTP endpoints such as local tests; applications targeting HTTPS
    should pass a connector compatible with [Cohttp_eio.Client.make ~https].
    [region] and [credentials] are explicit. Use [endpoint_config] for AWS
    endpoint variants, local S3-compatible tests, or custom S3-compatible
    endpoints. [retry_policy] defaults to [Awskit.Retry.default]. *)

module Body : sig
  include Awskit_s3.BODY with type 'a io := 'a and type t = Runtime.request_body

  val of_flow :
    content_length:int64 ->
    ?on_progress:(int64 -> unit) ->
    'flow Eio.Flow.source ->
    t
  (** Build a non-replayable request body from an existing Eio source flow.
      [content_length] must match the produced bytes, and the flow must remain
      valid until the request finishes. *)

  val of_path :
    ?on_progress:(int64 -> unit) ->
    _ Eio.Path.t ->
    (t, Awskit_s3.Error.t) result
  (** Build a replayable request body from a regular file path. The file is
      reopened for each request attempt. *)
end

module Reader : sig
  include
    Awskit_s3.READER
      with type 'a io := 'a
       and type t = Runtime.response_body_reader

  val to_flow :
    ?on_progress:(int64 -> unit) ->
    'flow Eio.Flow.sink ->
    t ->
    (unit, Awskit_s3.Error.t) result
  (** Stream a response body into an existing Eio sink flow. *)

  val to_path :
    ?on_progress:(int64 -> unit) ->
    _ Eio.Path.t ->
    t ->
    (unit, Awskit_s3.Error.t) result
  (** Stream a response body into a private [0o600] file. *)
end

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a
       and type request_body := Body.t
       and type response_body_reader := Reader.t

  module Transfer : sig
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
     and type request_body := Body.t

(** Presigned request artifact helpers using the client's region, credentials,
    clock, and endpoint configuration. *)
module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a
