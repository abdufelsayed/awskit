open Awskit_s3_common

open struct
  module Object = Awskit_s3_object
end

let parse_bool = function
  | "true" | "True" | "TRUE" -> Some true
  | "false" | "False" | "FALSE" -> Some false
  | _ -> None

let response_etag response =
  option_map_result Object.Etag.of_string
    (Awskit.Response.header response "etag")

let response_version response =
  option_map_result Object.Version_id.of_string
    (Awskit.Response.header response "x-amz-version-id")

let response_checksum response =
  let find alg header =
    match Awskit.Response.header response header with
    | None -> None
    | Some value -> Some { Object.Checksum.algorithm = alg; value }
  in
  match find `CRC32 "x-amz-checksum-crc32" with
  | Some _ as value -> value
  | None -> (
      match find `CRC32C "x-amz-checksum-crc32c" with
      | Some _ as value -> value
      | None -> (
          match find `CRC64NVME "x-amz-checksum-crc64nvme" with
          | Some _ as value -> value
          | None -> (
              match find `SHA1 "x-amz-checksum-sha1" with
              | Some _ as value -> value
              | None -> find `SHA256 "x-amz-checksum-sha256")))

let response_encryption response =
  match Awskit.Response.header response "x-amz-server-side-encryption" with
  | None -> None
  | Some "AES256" -> Some `AES256
  | Some "aws:kms" ->
      Some
        (`Aws_kms
           {
             Object.Encryption.key_id =
               Awskit.Response.header response
                 "x-amz-server-side-encryption-aws-kms-key-id";
             bucket_key_enabled =
               Option.bind
                 (Awskit.Response.header response
                    "x-amz-server-side-encryption-bucket-key-enabled")
                 parse_bool;
           })
  | Some value -> Some (`Unknown value)

let storage_class response =
  match Awskit.Response.header response "x-amz-storage-class" with
  | None -> Ok None
  | Some value -> (
      match Storage_class.of_string value with
      | Some value -> Ok (Some value)
      | None ->
          Error (Awskit.Error.decode (Fmt.str "invalid storage class %s" value))
      )

let object_info response =
  let* etag = response_etag response in
  let* storage_class = storage_class response in
  let* content_length = Awskit.Response.header_int response "content-length" in
  let* version_id = response_version response in
  Ok
    {
      Object.Get.etag;
      content_type = Awskit.Response.header response "content-type";
      content_length = Option.map Int64.of_int content_length;
      last_modified =
        Option.bind
          (Awskit.Response.header response "last-modified")
          ptime_of_string;
      metadata = Metadata_headers.of_headers (Awskit.Response.headers response);
      storage_class;
      version_id;
      checksum = response_checksum response;
      server_side_encryption = response_encryption response;
      request = response;
    }

let put_result response =
  let* etag = response_etag response in
  let* version_id = response_version response in
  Ok
    {
      Object.Put.etag;
      version_id;
      checksum = response_checksum response;
      request = response;
    }

let delete_result response =
  let* version_id = response_version response in
  Ok
    {
      Object.Delete.delete_marker =
        Option.bind
          (Awskit.Response.header response "x-amz-delete-marker")
          parse_bool;
      version_id;
      request = response;
    }

let copy_result response body =
  let etag, last_modified =
    match Xml.decode_root body ~name:"CopyObjectResult" with
    | Error _ -> (None, None)
    | Ok nodes ->
        ( Xml.child_text "ETag" nodes,
          Option.bind (Xml.child_text "LastModified" nodes) ptime_of_string )
  in
  let* etag = option_map_result Object.Etag.of_string etag in
  let* version_id = response_version response in
  let* copy_source_version_id =
    option_map_result Object.Version_id.of_string
      (Awskit.Response.header response "x-amz-copy-source-version-id")
  in
  Ok
    {
      Object.Copy.etag;
      last_modified;
      version_id;
      copy_source_version_id;
      request = response;
    }
