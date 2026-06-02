val parse_bool : string -> bool option

val response_etag :
  Awskit.Response.t -> (Object.Etag.t option, Awskit.Error.t) result

val response_version :
  Awskit.Response.t -> (Object.Version_id.t option, Awskit.Error.t) result

val response_checksum : Awskit.Response.t -> Object.Checksum.response
val response_encryption : Awskit.Response.t -> Object.Encryption.response option

val object_info :
  Awskit.Response.t -> (Object.Get.result, Awskit.Error.t) result

val put_result : Awskit.Response.t -> (Object.Put.result, Awskit.Error.t) result

val delete_result :
  Awskit.Response.t -> (Object.Delete.result, Awskit.Error.t) result

val embedded_service_error : Awskit.Response.t -> string -> Awskit.Error.t

val copy_result :
  Awskit.Response.t -> string -> (Object.Copy.result, Awskit.Error.t) result
