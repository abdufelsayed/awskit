(** Standalone S3 presigned URL generation. *)

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
(** S3 bucket addressing style used when building the presigned URL. *)

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]
(** AWS S3 endpoint variant. Ignored when an explicit endpoint is supplied. *)

type method_ = [ `GET | `PUT | `HEAD | `DELETE ]
(** HTTP method a caller must use with the generated URL. *)

type result = {
  url : string;
      (** Fully signed URL, including query-string authentication parameters. *)
  method_ : method_;  (** HTTP method the caller must use. *)
  signed_headers : (string * string) list;
      (** Headers that were part of the signature and must be sent with exactly
          the same names/values. *)
  expires_at : Ptime.t option;
      (** Absolute expiration time when [expires_in] was supplied. *)
}
(** Generated presigned request. Consumers must send [method_] and all
    [signed_headers] exactly as returned. *)

module Put_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
        (** URL lifetime. AWS S3 accepts at most seven days for SigV4 query
            authentication. *)
    content_type : string option;
        (** Optional [Content-Type] header to sign. The uploader must send the
            same value. *)
    checksum : Object.Checksum.value option;
        (** Optional checksum header to sign. *)
    server_side_encryption : Object.Encryption.request option;
        (** Optional server-side encryption headers to sign. *)
    expected_bucket_owner : string option;
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
    response_content_type : string option;
        (** Optional [response-content-type] query override. *)
    response_content_disposition : string option;
        (** Optional [response-content-disposition] query override. *)
    version_id : Object.Version_id.t option;  (** Object version to presign. *)
    expected_bucket_owner : string option;
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
    expected_bucket_owner : string option;
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
    expected_bucket_owner : string option;
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
  ?scheme:Awskit.Endpoint.Scheme.t ->
  ?endpoint:string ->
  unit ->
  endpoint_config
(** Build endpoint configuration for presigning.

    [endpoint] is used for custom S3-compatible services or local tests. When
    omitted, [endpoint_variant] and [scheme] select the generated AWS endpoint.
*)

val get_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:string ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  bucket:string ->
  key:string ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [GET Object] URL. *)

val put_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:string ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  bucket:string ->
  key:string ->
  ?options:Put_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [PUT Object] URL. Headers represented by [options],
    such as content type or checksum, must be sent by the eventual uploader. *)

val head_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:string ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  bucket:string ->
  key:string ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [HEAD Object] URL. *)

val delete_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:string ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  bucket:string ->
  key:string ->
  ?options:Delete_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [DELETE Object] URL. *)

val upload_part :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:string ->
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  bucket:string ->
  key:string ->
  upload_id:Multipart.Upload_id.t ->
  part_number:int ->
  ?options:Upload_part.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Generate a presigned [UploadPart] URL for one multipart part number. *)

val get_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:string ->
  key:string ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:get_object}, using a prebuilt endpoint configuration. *)

val put_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:string ->
  key:string ->
  ?options:Put_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:put_object}, using a prebuilt endpoint configuration. *)

val head_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:string ->
  key:string ->
  ?options:Get_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:head_object}, using a prebuilt endpoint configuration. *)

val delete_object_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:string ->
  key:string ->
  ?options:Delete_object.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:delete_object}, using a prebuilt endpoint configuration. *)

val upload_part_with_endpoint_config :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint_config:endpoint_config ->
  bucket:string ->
  key:string ->
  upload_id:Multipart.Upload_id.t ->
  part_number:int ->
  ?options:Upload_part.options ->
  unit ->
  (result, Awskit.Error.t) Stdlib.result
(** Like {!val:upload_part}, using a prebuilt endpoint configuration. *)
