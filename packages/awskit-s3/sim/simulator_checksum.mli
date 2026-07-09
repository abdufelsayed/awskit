(** Internal checksum helpers for simulator responses. *)

val etag : string -> Awskit_s3.Object.Etag.t
val multipart_etag : string list -> Awskit_s3.Object.Etag.t
val empty_checksum : Awskit_s3.Object.Checksum.response

val checksum_response :
  ?checksum_type:Awskit_s3.Object.Checksum.Type.observed ->
  Awskit_s3.Object.Checksum.observed_value list ->
  Awskit_s3.Object.Checksum.response

val validate_supported_algorithm :
  Awskit_s3.Object.Checksum.Algorithm.t -> (unit, Awskit.Error.t) result

val checksum_for_value :
  body:string ->
  Awskit_s3.Object.Checksum.value option ->
  (Awskit_s3.Object.Checksum.response, Awskit.Error.t) result

val checksum_for_algorithm :
  body:string ->
  Awskit_s3.Object.Checksum.Algorithm.t option ->
  (Awskit_s3.Object.Checksum.response, Awskit.Error.t) result

val checksum_summary :
  Awskit_s3.Object.Checksum.response -> Awskit_s3.Object.Checksum.summary

val checksum_response_headers :
  Awskit_s3.Object.Checksum.response -> (string * string) list
