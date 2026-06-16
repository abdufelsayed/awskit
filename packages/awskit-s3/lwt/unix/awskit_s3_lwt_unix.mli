(** Ready-to-use Lwt + Unix S3 adapter.

    Operations stream through {!Awskit_lwt_unix.Runtime}. Local-path body and
    reader helpers live under {!Body} and {!Reader}; managed upload/download
    helpers live under {!Object.Transfer}. *)

type t
(** Ready-to-use Lwt + Unix S3 client connection. Create with {!val:create}. *)

(** Lwt + Unix S3 runtime used by [Awskit_s3.Make]. *)
module Runtime :
  Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t and type connection = t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?endpoint:string ->
  ?addressing_style:Awskit_s3.addressing_style ->
  ?endpoint_variant:Awskit_s3.endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  ?region:string ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?imdsv1_fallback:Awskit_lwt_unix.Credentials.imdsv1_fallback ->
  unit ->
  (t, Awskit_s3.Error.t) result
(** Create a ready-to-use Lwt + Unix S3 client.

    If [region] or [credentials] are omitted, the underlying [Awskit_lwt_unix]
    runtime resolves them from standard AWS environment and profile sources.
    [endpoint] is for local S3-compatible services or custom endpoints.
    [addressing_style], [endpoint_variant], and [scheme] configure S3 endpoint
    resolution when no explicit endpoint is supplied. [region] and [endpoint]
    are parsed and validated when the client is created. *)

module Body : sig
  include
    Awskit_s3.BODY with type 'a io := 'a Lwt.t and type t = Runtime.request_body

  val of_lwt_stream : content_length:int64 -> string Lwt_stream.t -> t
  (** Build a non-replayable request body from an existing Lwt stream.
      [content_length] must match the produced bytes, and the stream must remain
      valid until the request finishes. *)

  val of_channel :
    content_length:int64 ->
    ?on_progress:(int64 -> unit) ->
    Lwt_io.input_channel ->
    t
  (** Build a non-replayable request body from an existing input channel.
      [content_length] must match the produced bytes, and the channel must
      remain valid until the request finishes. *)

  val of_path :
    ?on_progress:(int64 -> unit) ->
    string ->
    (t, Awskit_s3.Error.t) result Lwt.t
  (** Build a replayable request body from a regular file path. The file is
      reopened for each request attempt. *)
end

module Reader : sig
  include
    Awskit_s3.READER
      with type 'a io := 'a Lwt.t
       and type t = Runtime.response_body_reader

  val to_channel :
    ?on_progress:(int64 -> unit) ->
    Lwt_io.output_channel ->
    t ->
    (unit, Awskit_s3.Error.t) result Lwt.t
  (** Stream a response body into an existing output channel. *)

  val to_path :
    ?on_progress:(int64 -> unit) ->
    string ->
    t ->
    (unit, Awskit_s3.Error.t) result Lwt.t
  (** Stream a response body into a private [0o600] file. *)
end

module Object : sig
  include
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type request_body := Body.t
       and type response_body_reader := Reader.t

  module Transfer : sig
    val upload_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.upload_result, Awskit_s3.Error.t) result Lwt.t
    (** Upload a local file, using [PutObject] below the multipart threshold and
        multipart upload at or above it. *)

    val download_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.download_options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.download_result, Awskit_s3.Error.t) result Lwt.t
    (** Download an object to a local file, using ranged [GetObject] requests at
        or above the multipart threshold. *)

    val multipart_upload_file :
      t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
      Lwt.t
    (** Upload a local file with S3 multipart upload. The helper aborts the
        multipart upload when a fresh upload fails. *)

    val resume_multipart_upload_file :
      t ->
      bucket:string ->
      key:string ->
      upload_id:Awskit_s3.Multipart.Upload_id.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(int64 -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
      Lwt.t
    (** Resume an existing multipart upload by listing uploaded parts, skipping
        matching part numbers and sizes, uploading missing parts, and completing
        the upload. Existing uploads are not aborted on failure. *)
  end
end

(** Bucket operations returning [Lwt.t]. *)
module Bucket :
  Awskit_s3.BUCKET with type connection := t and type 'a io := 'a Lwt.t

(** Multipart operations returning [Lwt.t]. *)
module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a Lwt.t
     and type request_body := Body.t

(** Presigned URL helpers using the client's resolved region, credentials,
    clock, and endpoint configuration. *)
module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
