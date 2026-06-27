module Xml = S3_xml
module Metadata_headers = S3_metadata_headers

let ( let* ) = S3_result.( let* )

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
            (S3_error_context.decode_with_context
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
            (S3_error_context.decode_with_context ~what:"etag response header"
               (Awskit.Error.to_string_hum error)))

let response_version response =
  match Awskit.Response.header response "x-amz-version-id" with
  | None -> Ok None
  | Some value -> (
      match Object.Version_id.of_string value with
      | Ok version -> Ok (Some version)
      | Error error ->
          Error
            (S3_error_context.decode_with_context
               ~what:"x-amz-version-id response header"
               (Awskit.Error.to_string_hum error)))

let response_checksum response =
  let checksum_headers =
    [
      (Object.Checksum.Algorithm.Crc32, "x-amz-checksum-crc32");
      (Crc32c, "x-amz-checksum-crc32c");
      (Crc64nvme, "x-amz-checksum-crc64nvme");
      (Md5, "x-amz-checksum-md5");
      (Sha1, "x-amz-checksum-sha1");
      (Sha256, "x-amz-checksum-sha256");
      (Sha512, "x-amz-checksum-sha512");
      (Xxhash64, "x-amz-checksum-xxhash64");
      (Xxhash3, "x-amz-checksum-xxhash3");
      (Xxhash128, "x-amz-checksum-xxhash128");
    ]
  in
  let find (algorithm, header) =
    match Awskit.Response.header response header with
    | None -> None
    | Some value -> Some (Object.Checksum.response_value ~algorithm ~value)
  in
  let prefix = "x-amz-checksum-" in
  let prefix_length = String.length prefix in
  let known_header name =
    List.exists (fun (_, header) -> String.equal name header) checksum_headers
    || String.equal name "x-amz-checksum-type"
  in
  let unknown_values =
    Awskit.Response.headers response
    |> List.filter_map (fun (name, value) ->
        let name = String.lowercase_ascii name in
        if S3_string.is_prefix ~prefix name && not (known_header name) then
          let algorithm =
            String.sub name prefix_length (String.length name - prefix_length)
            |> String.uppercase_ascii
          in
          if String.equal algorithm "" then None
          else
            Some
              (Object.Checksum.response_value ~algorithm:(Unknown algorithm)
                 ~value)
        else None)
  in
  let values = List.filter_map find checksum_headers @ unknown_values in
  {
    Object.Checksum.values;
    checksum_type =
      Option.map Object.Checksum.Type.of_string
        (Awskit.Response.header response "x-amz-checksum-type");
  }

let response_encryption response =
  let kms () =
    let* bucket_key_enabled =
      response_bool_header response
        "x-amz-server-side-encryption-bucket-key-enabled"
    in
    Encryption.Kms.create
      ?key_id:
        (Awskit.Response.header response
           "x-amz-server-side-encryption-aws-kms-key-id")
      ?bucket_key_enabled ()
  in
  match
    Awskit.Response.header response
      "x-amz-server-side-encryption-customer-algorithm"
  with
  | Some "AES256" -> Ok (Some Encryption.Observed.Sse_c)
  | Some value -> Ok (Some (Encryption.Observed.Unknown value))
  | None -> (
      match Awskit.Response.header response "x-amz-server-side-encryption" with
      | None -> Ok None
      | Some "AES256" -> Ok (Some Encryption.Observed.Sse_s3)
      | Some "aws:kms" ->
          let* kms = kms () in
          Ok (Some (Encryption.Observed.Sse_kms kms))
      | Some "aws:kms:dsse" ->
          let* kms = kms () in
          Ok (Some (Encryption.Observed.Dsse_kms kms))
      | Some "aws:fsx" -> Ok (Some Encryption.Observed.Aws_fsx)
      | Some value -> Ok (Some (Encryption.Observed.Unknown value)))

let storage_class response =
  match Awskit.Response.header response "x-amz-storage-class" with
  | None -> Ok None
  | Some "" ->
      Error
        (S3_error_context.decode_with_context
           ~what:"x-amz-storage-class response header"
           "storage class must be non-empty")
  | Some value -> (
      match Storage_class.of_string value with
      | Ok storage_class -> Ok (Some storage_class)
      | Error error ->
          Error
            (S3_error_context.decode_with_context
               ~what:"x-amz-storage-class response header"
               (Awskit.Error.to_string_hum error)))

let response_content_type response =
  match Awskit.Response.header response "content-type" with
  | None -> Ok None
  | Some value -> (
      match Content_type.of_string value with
      | Ok content_type -> Ok (Some content_type)
      | Error error ->
          Error
            (S3_error_context.decode_with_context
               ~what:"Content-Type response header"
               (Awskit.Error.to_string_hum error)))

let response_content_range response =
  match Awskit.Response.header response "content-range" with
  | None -> Ok None
  | Some value -> (
      match Range.Content_range.of_header value with
      | Ok range -> Ok (Some range)
      | Error error ->
          Error
            (Awskit.Error.Producer.with_context
               "decoding Content-Range response header" error))

let response_time_header response name =
  match Awskit.Response.header response name with
  | None -> Ok None
  | Some value -> (
      match S3_time.of_string value with
      | Some time -> Ok (Some time)
      | None ->
          Error
            (S3_error_context.decode_with_context
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
  let* encryption = response_encryption response in
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
      encryption;
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
          S3_time.of_string nodes
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
                  (S3_error_context.decode_with_context
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
        (Xml.decode_with_context ~what:"CopyObjectResult XML"
           (Fmt.str "expected CopyObjectResult XML, got %s" actual))
