(** S3 multipart XML decoder helpers. *)

val checksum_xml_name : Object.Checksum.Algorithm.t -> string option
(** Return the XML element name for a checksum algorithm. *)

val checksum_response_from_xml : Ezxmlm.nodes -> Object.Checksum.response
(** Decode checksum fields from an XML node list. *)

val complete_result :
  Awskit.Response.t ->
  string ->
  (Multipart.Complete.result, Awskit.Error.t) result
(** Decode a [CompleteMultipartUpload] response body and response headers. *)
