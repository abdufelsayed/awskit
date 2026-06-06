open Awskit_s3

val etag : string -> Object.Etag.t
val empty_checksum : Object.Checksum.response

val checksum_response :
  ?checksum_type:Object.Checksum.Type.t ->
  Object.Checksum.value list ->
  Object.Checksum.response

val checksum_for_value :
  Object.Checksum.value option -> Object.Checksum.response

val checksum_for_algorithm :
  body:string -> Object.Checksum.Algorithm.t option -> Object.Checksum.response

val checksum_summary : Object.Checksum.response -> Object.Checksum.summary

val checksum_response_headers :
  Object.Checksum.response -> (string * string) list
