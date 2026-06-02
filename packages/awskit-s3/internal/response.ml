open Common
open Operation_data

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
  let find algorithm header =
    match Awskit.Response.header response header with
    | None -> None
    | Some value -> Some { Object.Checksum.algorithm; value }
  in
  let values =
    [
      find Object.Checksum.Algorithm.Crc32 "x-amz-checksum-crc32";
      find Crc32c "x-amz-checksum-crc32c";
      find Crc64nvme "x-amz-checksum-crc64nvme";
      find Md5 "x-amz-checksum-md5";
      find Sha1 "x-amz-checksum-sha1";
      find Sha256 "x-amz-checksum-sha256";
      find Sha512 "x-amz-checksum-sha512";
      find Xxhash64 "x-amz-checksum-xxhash64";
      find Xxhash3 "x-amz-checksum-xxhash3";
      find Xxhash128 "x-amz-checksum-xxhash128";
    ]
    |> List.filter_map Fun.id
  in
  {
    Object.Checksum.values;
    checksum_type =
      Option.map Object.Checksum.Type.of_string
        (Awskit.Response.header response "x-amz-checksum-type");
  }

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
      Get_object.etag;
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
      response;
    }

let put_result response =
  let* etag = response_etag response in
  let* version_id = response_version response in
  Ok
    {
      Put_object.etag;
      version_id;
      checksum = response_checksum response;
      response;
    }

let delete_result response =
  let* version_id = response_version response in
  Ok
    {
      Delete_object.delete_marker =
        Option.bind
          (Awskit.Response.header response "x-amz-delete-marker")
          parse_bool;
      version_id;
      response;
    }

let embedded_service_error response body =
  Awskit.Error.service
    {
      status = Awskit.Response.status response;
      code = Xml.service_code body;
      message = Xml.service_message body;
      request_id = Awskit.Response.request_id response;
      host_id = Awskit.Response.host_id response;
      headers = Awskit.Response.headers response;
      body = Some body;
    }

let copy_result response body =
  match Xml.root body with
  | Error _ as error -> error
  | Ok ("Error", _) -> Error (embedded_service_error response body)
  | Ok ("CopyObjectResult", nodes) ->
      let etag = Xml.child_text "ETag" nodes in
      let last_modified =
        Option.bind (Xml.child_text "LastModified" nodes) ptime_of_string
      in
      let* etag = option_map_result Object.Etag.of_string etag in
      let* version_id = response_version response in
      let* copy_source_version_id =
        option_map_result Object.Version_id.of_string
          (Awskit.Response.header response "x-amz-copy-source-version-id")
      in
      Ok
        {
          Copy_object.etag;
          last_modified;
          version_id;
          copy_source_version_id;
          response;
        }
  | Ok (actual, _) ->
      Error
        (Awskit.Error.decode
           (Fmt.str "expected CopyObjectResult XML, got %s" actual))
