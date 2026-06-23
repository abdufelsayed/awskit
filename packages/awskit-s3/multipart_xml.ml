open Common
open Response

let checksum_xml_name = function
  | Object.Checksum.Algorithm.Crc32 -> Some "ChecksumCRC32"
  | Crc32c -> Some "ChecksumCRC32C"
  | Crc64nvme -> Some "ChecksumCRC64NVME"
  | Md5 -> Some "ChecksumMD5"
  | Sha1 -> Some "ChecksumSHA1"
  | Sha256 -> Some "ChecksumSHA256"
  | Sha512 -> Some "ChecksumSHA512"
  | Xxhash64 -> Some "ChecksumXXHASH64"
  | Xxhash3 -> Some "ChecksumXXHASH3"
  | Xxhash128 -> Some "ChecksumXXHASH128"
  | Unknown _ -> None

let checksum_values_from_xml nodes =
  let find algorithm name =
    Option.map
      (fun value -> { Object.Checksum.algorithm; value })
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
      Option.map Object.Checksum.Type.of_string
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
      let etag = Xml.child_text "ETag" nodes in
      let etag = option_map_result Object.Etag.of_string etag in
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
        (Awskit.Error.Producer.decode
           (Fmt.str "expected CompleteMultipartUploadResult XML, got %s" actual))
