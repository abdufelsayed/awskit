(** Eio S3 adapter.

    Operations are direct-style and stream through {!Awskit_eio.Runtime}.
    Local-path body and reader helpers live under {!Body} and {!Reader}; managed
    upload/download helpers live under {!Object.Transfer}. *)

type t
(** Configured Eio S3 client. Create with {!val:create}. *)

val create :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  https:'flow Awskit_eio.https ->
  region:string ->
  credentials:Awskit.Credentials.t ->
  ?retry_policy:Awskit.Retry.t ->
  ?random_float:(upper_bound:float -> float) ->
  ?timeout_policy:Awskit.Timeout.policy ->
  ?endpoint_config:Awskit_s3.Endpoint_config.t ->
  ?max_response_drain_bytes:int ->
  unit ->
  (t, Awskit_s3.Error.t) result
(** Create an Eio S3 client.

    [https] is forwarded to [Awskit_eio.create]. Use [Awskit_eio.http_only] only
    for plain HTTP endpoints such as local tests; applications targeting HTTPS
    should pass a connector compatible with [Cohttp_eio.Client.make ~https].
    [region] and [credentials] are explicit. Use [endpoint_config] for AWS
    endpoint variants, local S3-compatible tests, or explicit endpoints that use
    S3-compatible signing/addressing rules. [retry_policy] defaults to
    [Awskit.Retry.default]. [max_response_drain_bytes] controls how much
    response body the runtime drains after successful consumers. *)

module Body : sig
  type t

  include
    Awskit_s3.Runtime_adapter.Request_body with type 'a io := 'a and type t := t

  val of_flow :
    content_length:int64 ->
    ?on_progress:(int64 -> unit) ->
    'flow Eio.Flow.source ->
    (t, Awskit_s3.Error.t) result
  (** Build a non-replayable request body from an existing Eio source flow.
      [content_length] must match the produced bytes, and the flow must remain
      valid until the request finishes. The caller owns and closes the flow. *)

  val of_path :
    ?on_progress:(int64 -> unit) ->
    _ Eio.Path.t ->
    (t, Awskit_s3.Error.t) result
  (** Build a replayable request body from a regular file path. The file is
      reopened for each request attempt. *)
end

module Reader : sig
  type t

  include
    Awskit_s3.Runtime_adapter.Response_reader
      with type 'a io := 'a
       and type t := t

  val to_flow :
    ?on_progress:(int64 -> unit) ->
    'flow Eio.Flow.sink ->
    t ->
    (unit, Awskit_s3.Error.t) result
  (** Stream a response body into an existing Eio sink flow. The caller owns and
      closes the flow. *)

  val to_path :
    ?on_progress:(int64 -> unit) ->
    _ Eio.Path.t ->
    t ->
    (unit, Awskit_s3.Error.t) result
  (** Stream a response body into a private [0o600] temporary file, then publish
      it at the target path when the copy succeeds. *)
end

module Object : sig
  include
    Awskit_s3.Runtime_adapter.Object_operations
      with type client := t
       and type 'a io := 'a
       and type request_body := Body.t
       and type response_body_reader := Reader.t

  module Transfer : sig
    val upload_file :
      t ->
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.upload_result, Awskit_s3.Error.t) result
    (** Upload a local file, using [PutObject] below the multipart threshold and
        multipart upload at or above it. *)

    val download_file :
      t ->
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.download_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.download_result, Awskit_s3.Error.t) result
    (** Download an object to a local file, using ranged [GetObject] requests at
        or above the multipart threshold. The selected overwrite policy controls
        whether an existing target is replaced or rejected before transport. *)

    val multipart_upload_file :
      t ->
      bucket:Awskit_s3.Bucket_name.t ->
      key:Awskit_s3.Object_key.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
    (** Upload a local file with S3 multipart upload. The helper aborts the
        multipart upload when an Awskit-created upload fails before completion.
    *)

    val resume_multipart_upload_file :
      t ->
      upload:
        Awskit_s3.Multipart.Upload.caller_owned Awskit_s3.Multipart.Upload.t ->
      ?options:Awskit_s3.Transfer.upload_options ->
      ?on_progress:(Awskit_s3.Transfer.progress -> unit) ->
      path:_ Eio.Path.t ->
      unit ->
      (Awskit_s3.Transfer.multipart_upload_result, Awskit_s3.Error.t) result
    (** Resume a caller-owned multipart upload by verifying the upload with
        [ListParts], uploading every local part into that upload, and completing
        from the fresh [UploadPart] results. Caller-owned uploads are not
        aborted on failure. *)
  end
end

(** Bucket operations using direct-style Eio results. *)
module Bucket :
  Awskit_s3.Runtime_adapter.Bucket_operations
    with type client := t
     and type 'a io := 'a

(** Multipart operations using direct-style Eio results. *)
module Multipart :
  Awskit_s3.Runtime_adapter.Multipart_operations
    with type client := t
     and type 'a io := 'a
     and type request_body := Body.t

(** Presigned request artifact helpers using the client's region, credentials,
    clock, and endpoint configuration. *)
module Presigned :
  Awskit_s3.Runtime_adapter.Presigned_operations
    with type client := t
     and type 'a io := 'a
