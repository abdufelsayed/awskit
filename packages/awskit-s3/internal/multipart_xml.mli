val checksum_xml_name : Object.Checksum.Algorithm.t -> string option
val checksum_response_from_xml : Ezxmlm.nodes -> Object.Checksum.response

val complete_result :
  Awskit.Response.t ->
  string ->
  (Multipart.Complete.result, Awskit.Error.t) result
