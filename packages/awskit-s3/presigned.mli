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
  method_ : method_;
  signed_headers : (string * string) list;
  expires_at : Ptime.t option;
}
(** Generated presigned request. Consumers must send [method_] and all
    [signed_headers] exactly as returned. *)

module Put_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
    content_type : string option;
    checksum : Object.Checksum.value option;
    server_side_encryption : Object.Encryption.request option;
    expected_bucket_owner : string option;
    extra_signed_headers : (string * string) list;
  }
  (** Presigned [PUT Object] options. *)

  val default_options : options
end

module Get_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
    response_content_type : string option;
    response_content_disposition : string option;
    version_id : Object.Version_id.t option;
    expected_bucket_owner : string option;
    extra_signed_headers : (string * string) list;
  }
  (** Presigned [GET Object] and [HEAD Object] options. *)

  val default_options : options
end

module Upload_part : sig
  type options = {
    expires_in : Ptime.Span.t option;
    checksum : Object.Checksum.value option;
    expected_bucket_owner : string option;
    extra_signed_headers : (string * string) list;
  }
  (** Presigned [UploadPart] options. *)

  val default_options : options
end

module Delete_object : sig
  type options = {
    expires_in : Ptime.Span.t option;
    expected_bucket_owner : string option;
    extra_signed_headers : (string * string) list;
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
  ?endpoint:Awskit.Endpoint.t ->
  unit ->
  endpoint_config
(** Build endpoint configuration for presigning. *)

val get_object :
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:Awskit.Endpoint.t ->
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
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:Awskit.Endpoint.t ->
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
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:Awskit.Endpoint.t ->
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
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:Awskit.Endpoint.t ->
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
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  ?endpoint:Awskit.Endpoint.t ->
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
