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
  ?endpoint_config:Awskit_s3.Endpoint_config.t ->
  ?region:string ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?random_float:(upper_bound:float -> float) ->
  ?timeout_policy:Awskit.Timeout.policy ->
  ?max_response_drain_bytes:int ->
  ?imdsv1_fallback:Awskit_lwt_unix.Credentials.imdsv1_fallback ->
  unit ->
  (t, Awskit_s3.Error.t) result
(** Create a ready-to-use Lwt + Unix S3 client.

    If [region] or [credentials] are omitted, the underlying [Awskit_lwt_unix]
    runtime resolves them from standard AWS environment and profile sources. Use
    [endpoint_config] for AWS endpoint variants, local S3-compatible tests, or
    explicit endpoints that use S3-compatible signing/addressing rules.
    [max_response_drain_bytes] controls how much response body the runtime
    drains after successful consumers. *)

module Body : sig
  include
    Awskit_s3.BODY with type 'a io := 'a Lwt.t and type t = Runtime.request_body

  val of_lwt_stream :
    content_length:int64 -> string Lwt_stream.t -> (t, Awskit_s3.Error.t) result
  (** Build a non-replayable request body from an existing Lwt stream.
      [content_length] must match the produced bytes, and the stream must remain
      valid until the request finishes. *)

  val of_channel :
    content_length:int64 ->
    ?on_progress:(int64 -> unit) ->
    Lwt_io.input_channel ->
    (t, Awskit_s3.Error.t) result
  (** Build a non-replayable request body from an existing input channel.
      [content_length] must match the produced bytes, and the channel must
      remain valid until the request finishes. The caller owns and closes the
      channel. *)

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
  (** Stream a response body into an existing output channel. The caller owns
      and closes the channel. *)

  val to_path :
    ?on_progress:(int64 -> unit) ->
    string ->
    t ->
    (unit, Awskit_s3.Error.t) result Lwt.t
  (** Stream a response body into a private [0o600] temporary file, then publish
      it at the target path when the copy succeeds. *)
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
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.upload_result, Awskit_s3.Error.t) result Lwt.t
    (** Upload a local file, using [PutObject] below the multipart threshold and
        multipart upload at or above it. *)

    val download_file :
      t ->
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.download_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.download_result, Awskit_s3.Error.t) result Lwt.t
    (** Download an object to a local file, using ranged [GetObject] requests at
        or above the multipart threshold. The selected overwrite policy controls
        whether an existing target is replaced or rejected before transport. *)

    val multipart_upload_file :
      t ->
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
      Lwt.t
    (** Upload a local file with S3 multipart upload. The helper aborts the
        multipart upload when an Awskit-created upload fails before completion.
    *)

    val resume_multipart_upload_file :
      t ->
      upload:
        Awskit_s3.Multipart.Upload.caller_owned Awskit_s3.Multipart.Upload.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:string ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
      Lwt.t
    (** Resume a caller-owned multipart upload by verifying the upload with
        [ListParts], uploading every local part into that upload, and completing
        from the fresh [UploadPart] results. Caller-owned uploads are not
        aborted on failure. *)
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

(** Presigned request artifact helpers using the client's resolved region,
    credentials, clock, and endpoint configuration. *)
module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
