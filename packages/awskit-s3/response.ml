open Common
module Put_object = Object.Put
module Get_object = Object.Get
module Delete_object = Object.Delete
module Copy_object = Object.Copy

let parse_bool = function
  | "true" | "True" | "TRUE" -> Some true
  | "false" | "False" | "FALSE" -> Some false
  | _ -> None

let response_bool_header response name =
  match Awskit.Response.header response name with
  | None -> Ok None
  | Some value -> (
      match parse_bool value with
      | Some value -> Ok (Some value)
      | None ->
          Error
            (decode_with_context
               ~what:(Fmt.str "%s response header" name)
               (Fmt.str "invalid boolean value %S" value)))

let response_etag response =
  match Awskit.Response.header response "etag" with
  | None -> Ok None
  | Some value -> (
      match Object.Etag.of_string value with
      | Ok etag -> Ok (Some etag)
      | Error error ->
          Error
            (decode_with_context ~what:"etag response header"
               (Awskit.Error.to_string_hum error)))

let response_version response =
  match Awskit.Response.header response "x-amz-version-id" with
  | None -> Ok None
  | Some value -> (
      match Object.Version_id.of_string value with
      | Ok version -> Ok (Some version)
      | Error error ->
          Error
            (decode_with_context ~what:"x-amz-version-id response header"
               (Awskit.Error.to_string_hum error)))

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
  | None -> Ok None
  | Some "AES256" -> Ok (Some `AES256)
  | Some "aws:kms" ->
      let* bucket_key_enabled =
        response_bool_header response
          "x-amz-server-side-encryption-bucket-key-enabled"
      in
      Ok
        (Some
           (`Aws_kms
              {
                Object.Encryption.key_id =
                  Awskit.Response.header response
                    "x-amz-server-side-encryption-aws-kms-key-id";
                bucket_key_enabled;
              }))
  | Some value -> Ok (Some (`Unknown value))

let storage_class response =
  match Awskit.Response.header response "x-amz-storage-class" with
  | None -> Ok None
  | Some "" ->
      Error
        (decode_with_context ~what:"x-amz-storage-class response header"
           "storage class must be non-empty")
  | Some value -> Ok (Some (Storage_class.of_string value))

let response_content_type response =
  match Awskit.Response.header response "content-type" with
  | None -> Ok None
  | Some value -> (
      match Content_type.of_string value with
      | Ok content_type -> Ok (Some content_type)
      | Error error ->
          Error
            (decode_with_context ~what:"Content-Type response header"
               (Awskit.Error.to_string_hum error)))

let response_content_range response =
  option_map_result Range.Content_range.of_header
    (Awskit.Response.header response "content-range")

let response_time_header response name =
  match Awskit.Response.header response name with
  | None -> Ok None
  | Some value -> (
      match ptime_of_string value with
      | Some time -> Ok (Some time)
      | None ->
          Error
            (decode_with_context
               ~what:(Fmt.str "%s response header" name)
               (Fmt.str "invalid timestamp value %S" value)))

let object_info response =
  let* etag = response_etag response in
  let* storage_class = storage_class response in
  let* content_length =
    Awskit.Response.header_int64 response "content-length"
  in
  let* version_id = response_version response in
  let* content_type = response_content_type response in
  let* content_range = response_content_range response in
  let* server_side_encryption = response_encryption response in
  let* last_modified = response_time_header response "last-modified" in
  let* metadata =
    Metadata_headers.of_headers (Awskit.Response.headers response)
  in
  Ok
    {
      Get_object.etag;
      content_type;
      content_length;
      content_range;
      last_modified;
      metadata;
      storage_class;
      version_id;
      checksum = response_checksum response;
      server_side_encryption;
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
  let* delete_marker = response_bool_header response "x-amz-delete-marker" in
  Ok { Delete_object.delete_marker; version_id; response }

let first_some first second =
  match first with Some _ -> first | None -> second

let embedded_service_error response body =
  let error = Xml.service_error body in
  Awskit.Error.Producer.service
    ~status:(Awskit.Response.status response)
    ?code:error.code ?message:error.message
    ?request_id:
      (first_some (Awskit.Response.request_id response) error.request_id)
    ?host_id:(first_some (Awskit.Response.host_id response) error.host_id)
    ~headers:(Awskit.Response.headers response)
    ~body ()

let copy_result response body =
  match Xml.root body with
  | Error _ as error -> error
  | Ok ("Error", _) -> Error (embedded_service_error response body)
  | Ok ("CopyObjectResult", nodes) ->
      let* etag =
        Xml.optional_child_result ~path:"CopyObjectResult" "ETag"
          Object.Etag.of_string nodes
      in
      let* last_modified =
        Xml.optional_child_parse ~path:"CopyObjectResult" "LastModified"
          ptime_of_string nodes
      in
      let* version_id = response_version response in
      let* copy_source_version_id =
        match
          Awskit.Response.header response "x-amz-copy-source-version-id"
        with
        | None -> Ok None
        | Some value -> (
            match Object.Version_id.of_string value with
            | Ok version -> Ok (Some version)
            | Error error ->
                Error
                  (decode_with_context
                     ~what:"x-amz-copy-source-version-id response header"
                     (Awskit.Error.to_string_hum error)))
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
        (Awskit.Error.Producer.decode
           (Fmt.str "expected CopyObjectResult XML, got %s" actual))
