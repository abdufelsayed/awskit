module Xml = S3_xml
open Response

let checksum_xml_name = function
  | Object.Checksum.Algorithm.Crc32 -> "ChecksumCRC32"
  | Crc32c -> "ChecksumCRC32C"
  | Crc64nvme -> "ChecksumCRC64NVME"
  | Md5 -> "ChecksumMD5"
  | Sha1 -> "ChecksumSHA1"
  | Sha256 -> "ChecksumSHA256"
  | Sha512 -> "ChecksumSHA512"
  | Xxhash64 -> "ChecksumXXHASH64"
  | Xxhash3 -> "ChecksumXXHASH3"
  | Xxhash128 -> "ChecksumXXHASH128"

let checksum_values_from_xml nodes =
  let find algorithm name =
    Option.map
      (fun value ->
        {
          Object.Checksum.algorithm = Object.Checksum.Algorithm.Known algorithm;
          value;
        })
      (Xml.child_text name nodes)
  in
  [
    find Crc32 "ChecksumCRC32";
    find Crc32c "ChecksumCRC32C";
    find Crc64nvme "ChecksumCRC64NVME";
    find Md5 "ChecksumMD5";
    find Sha1 "ChecksumSHA1";
    find Sha256 "ChecksumSHA256";
    find Sha512 "ChecksumSHA512";
    find Xxhash64 "ChecksumXXHASH64";
    find Xxhash3 "ChecksumXXHASH3";
    find Xxhash128 "ChecksumXXHASH128";
  ]
  |> List.filter_map Fun.id

let checksum_response_from_xml nodes =
  {
    Object.Checksum.values = checksum_values_from_xml nodes;
    checksum_type =
      Option.map Object.Checksum.Type.observed_of_string
        (Xml.child_text "ChecksumType" nodes);
  }

let merge_checksum_response first second =
  {
    Object.Checksum.values =
      first.Object.Checksum.values @ second.Object.Checksum.values;
    checksum_type =
      (match first.checksum_type with
      | Some _ as value -> value
      | None -> second.checksum_type);
  }

let complete_result response body =
  match Xml.root body with
  | Error _ as error -> error
  | Ok ("Error", _) -> Error (embedded_service_error response body)
  | Ok ("CompleteMultipartUploadResult", nodes) -> (
      let etag =
        Xml.optional_child_result ~path:"CompleteMultipartUploadResult" "ETag"
          Object.Etag.of_string nodes
      in
      let version_id = response_version response in
      match (etag, version_id) with
      | Error error, _ | _, Error error -> Error error
      | Ok etag, Ok version_id ->
          let xml_checksum = checksum_response_from_xml nodes in
          Ok
            {
              Multipart.Complete.etag;
              version_id;
              checksum =
                merge_checksum_response xml_checksum
                  (response_checksum response);
              response;
            })
  | Ok (actual, _) ->
      Error
        (Xml.decode_with_context ~what:"CompleteMultipartUploadResult XML"
           (Fmt.str "expected CompleteMultipartUploadResult XML, got %s" actual))
