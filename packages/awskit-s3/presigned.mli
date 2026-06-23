(** Standalone S3 presigned request generation. *)

type addressing_style = Endpoint_config.addressing_style
(** S3 bucket addressing style used when building the presigned request. *)

type endpoint_variant = Endpoint_config.endpoint_variant
(** AWS S3 endpoint variant. *)

type method_ = [ `GET | `PUT | `HEAD | `DELETE ]
(** HTTP method a caller must use with the generated request. *)

type result
(** Opaque generated presigned request artifact.

    Presigned URLs are bearer tokens. The raw URL is intentionally hidden behind
    {!val:reveal_url}; use {!val:safe_uri}, {!val:method_},
    {!val:signed_headers}, and the expiry accessors for logs, diagnostics, and
    user-facing output. Consumers that execute the request must use
    {!val:reveal_url}, {!val:method_}, and all {!val:signed_headers} exactly as
    returned. *)

val method_ : result -> method_
(** HTTP method the caller must use. *)

val safe_uri : result -> Uri.t
(** Documentation/log-safe URI with SigV4 bearer query parameters removed.

    Operation query parameters such as response overrides, [versionId],
    [partNumber], and [uploadId] are preserved. *)

val signed_headers : result -> (string * string) list
(** Headers that were part of the signature and must be sent with exactly the
    same names/values. Do not log header values unless the caller has made an
    explicit application-level decision that they are safe. *)

val requested_expires_in : result -> Ptime.Span.t
(** Lifetime requested by the caller, or the default when omitted. *)

val effective_expires_in : result -> Ptime.Span.t
(** Lifetime actually signed into the bearer URL.

    This is capped by temporary credential expiration when credentials expire
    before the requested lifetime. *)

val expires_at : result -> Ptime.t option
(** Absolute expiration timestamp for the effective lifetime. *)

val reveal_url : result -> string
(** Return the fully signed bearer URL.

    This includes SigV4 credential, signature, and session-token material and
    should only be handed to the component that will execute the presigned
    request. Do not print or log it by default. *)

val pp : Format.formatter -> result -> unit
(** Safe pretty-printer that omits the raw bearer URL, SigV4 credential,
    signature, session token, and signed header values. *)

module Put_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
        (** URL lifetime. AWS S3 accepts at most seven days for SigV4 query
            authentication. *)
    content_type : Content_type.t option;
        (** Optional [Content-Type] header to sign. The uploader must send the
            same value. *)
    checksum : Object.Checksum.value option;
        (** Optional checksum header to sign. *)
    server_side_encryption : Object.Encryption.request option;
        (** Optional server-side encryption headers to sign. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner] header to sign. *)
    extra_signed_headers : (string * string) list;
        (** Additional headers to include in the signature. Callers must send
            them with the eventual request. *)
  }
  (** Presigned [PUT Object] options. *)

  val default_options : options
end

module Get_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
        (** URL lifetime. AWS S3 accepts at most seven days for SigV4 query
            authentication. *)
    response_content_type : Content_type.t option;
        (** Optional [response-content-type] query override. *)
    response_content_disposition : Header_value.t option;
        (** Optional [response-content-disposition] query override. *)
    version_id : Object.Version_id.t option;  (** Object version to presign. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner] header to sign. *)
    extra_signed_headers : (string * string) list;
        (** Additional headers to include in the signature. *)
  }
  (** Presigned [GET Object] and [HEAD Object] options. *)

  val default_options : options
end

module Upload_part : sig
  type options = {
    expires_in : Ptime.Span.t option;
        (** URL lifetime. AWS S3 accepts at most seven days for SigV4 query
            authentication. *)
    checksum : Object.Checksum.value option;
        (** Optional checksum header to sign for the part body. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner] header to sign. *)
    extra_signed_headers : (string * string) list;
        (** Additional headers to include in the signature. *)
  }
  (** Presigned [UploadPart] options. *)

  val default_options : options
end

module Delete_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
        (** URL lifetime. AWS S3 accepts at most seven days for SigV4 query
            authentication. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner] header to sign. *)
    extra_signed_headers : (string * string) list;
        (** Additional headers to include in the signature. *)
  }
  (** Presigned [DELETE Object] options. *)

  val default_options : options
end

type endpoint_config = Endpoint_resolver.t
(** Reusable endpoint configuration for callers generating many URLs. *)

val endpoint_config :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  unit ->
  endpoint_config
(** Build AWS endpoint configuration for presigning. Use
    {!Awskit_s3.Endpoint_config} for local or S3-compatible endpoints. *)

val get_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [GET Object] request artifact. *)

val put_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Put_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [PUT Object] request artifact. Headers represented by
    [options], such as content type or checksum, must be sent by the eventual
    uploader. *)

val head_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [HEAD Object] request artifact. *)

val delete_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Delete_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [DELETE Object] request artifact. *)

val upload_part :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  upload_id:Multipart.Upload_id.t ->
  part_number:int ->
  ?options:Upload_part.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [UploadPart] request artifact for one multipart part
    number. *)

val get_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:get_object}, using a prebuilt endpoint configuration. *)

val put_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Put_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:put_object}, using a prebuilt endpoint configuration. *)

val head_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:head_object}, using a prebuilt endpoint configuration. *)

val delete_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  ?options:Delete_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:delete_object}, using a prebuilt endpoint configuration. *)

val upload_part_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  upload_id:Multipart.Upload_id.t ->
  part_number:int ->
  ?options:Upload_part.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:upload_part}, using a prebuilt endpoint configuration. *)
