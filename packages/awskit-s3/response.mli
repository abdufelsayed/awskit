(** Low-level S3 response-header and response-payload decoders.

    These helpers are used by request builders to turn raw
    {!module:Awskit.Response} metadata and small XML payloads into public S3
    result records. *)

val parse_bool : string -> bool option
(** Parse S3 boolean text. *)

val response_etag :
  Awskit.Response.t -> (Object.Etag.t option, Awskit.Error.t) result
(** Parse the [ETag] response header. *)

val response_version :
  Awskit.Response.t -> (Object.Version_id.t option, Awskit.Error.t) result
(** Parse the [x-amz-version-id] response header. *)

val response_checksum : Awskit.Response.t -> Object.Checksum.response
(** Parse modeled checksum response headers. *)

val response_encryption : Awskit.Response.t -> Object.Encryption.response option
(** Parse server-side encryption response headers. *)

val object_info :
  Awskit.Response.t -> (Object.Get.result, Awskit.Error.t) result
(** Build object metadata from [GetObject] or [HeadObject] response headers. *)

val put_result : Awskit.Response.t -> (Object.Put.result, Awskit.Error.t) result
(** Build [PutObject] result metadata from response headers. *)

val delete_result :
  Awskit.Response.t -> (Object.Delete.result, Awskit.Error.t) result
(** Build [DeleteObject] result metadata from response headers. *)

val embedded_service_error : Awskit.Response.t -> string -> Awskit.Error.t
(** Build a service error from an operation that returned an embedded error
    payload in an otherwise transport-successful response. *)

val copy_result :
  Awskit.Response.t -> string -> (Object.Copy.result, Awskit.Error.t) result
(** Decode a [CopyObject] XML response body and response headers. *)
