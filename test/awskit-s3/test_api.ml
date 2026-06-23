open Awskit_s3
open Awskit_s3_test

let test_public_operation_aliases () =
  let owner = Account_id.of_string_exn "123456789012" in
  let content_type = Content_type.of_string_exn "text/plain" in
  let metadata = Metadata.of_list_exn [ ("source", "api") ] in
  let tags =
    Tag.Set.of_list_exn [ Tag.create_exn ~key:"project" ~value:"awskit" ]
  in
  ignore (Put_object.default_options : Put_object.options);
  ignore
    (Put_object.options ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : (Put_object.options, Error.t) result);
  ignore
    (Put_object.options_exn ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : Put_object.options);
  ignore (Get_object.default_options : Get_object.options);
  ignore
    (Get_object.options ~expected_bucket_owner:owner ()
      : (Get_object.options, Error.t) result);
  ignore
    (Get_object.options_exn ~expected_bucket_owner:owner ()
      : Get_object.options);
  ignore (Head_object.default_options : Head_object.options);
  ignore
    (Head_object.options ~expected_bucket_owner:owner ()
      : (Head_object.options, Error.t) result);
  ignore
    (Head_object.options_exn ~expected_bucket_owner:owner ()
      : Head_object.options);
  ignore (Delete_object.default_options : Delete_object.options);
  ignore
    (Delete_object.options ~expected_bucket_owner:owner ()
      : (Delete_object.options, Error.t) result);
  ignore
    (Delete_object.options_exn ~expected_bucket_owner:owner ()
      : Delete_object.options);
  let delete_object : Delete_objects.object_ =
    Delete_objects.object_ ~key:(object_key "file.txt") ()
  in
  ignore (delete_object : Delete_objects.object_);
  ignore (Delete_objects.max_objects : int);
  ignore
    (Delete_objects.options ~expected_bucket_owner:owner ()
      : (Delete_objects.options, Error.t) result);
  ignore
    (Delete_objects.options_exn ~expected_bucket_owner:owner ()
      : Delete_objects.options);
  ignore (None : string Get_object.result option);
  ignore (None : Get_object.info option);
  ignore (Copy_object.default_options : Copy_object.options);
  ignore
    (Copy_object.options ~expected_bucket_owner:owner
       ~source_expected_bucket_owner:owner ()
      : (Copy_object.options, Error.t) result);
  ignore
    (Copy_object.options_exn ~expected_bucket_owner:owner
       ~source_expected_bucket_owner:owner ()
      : Copy_object.options);
  ignore
    (Object.Tagging.options ~expected_bucket_owner:owner ()
      : (Object.Tagging.options, Error.t) result);
  ignore
    (Object.Tagging.options_exn ~expected_bucket_owner:owner ()
      : Object.Tagging.options);
  ignore (List_objects_v2.default_options : List_objects_v2.options);
  ignore
    (List_objects_v2.options
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:List_objects_v2.Delimiter.slash
       ~start_after:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : (List_objects_v2.options, Error.t) result);
  ignore
    (List_objects_v2.options_exn
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:List_objects_v2.Delimiter.slash
       ~start_after:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : List_objects_v2.options);
  let listed_object : List_objects_v2.object_summary =
    {
      key = object_key "file.txt";
      size = Some 1L;
      etag = None;
      last_modified = None;
      storage_class = None;
      checksum = Object.Checksum.empty_summary;
    }
  in
  ignore (listed_object : List_objects_v2.object_summary);
  ignore (List_object_versions.default_options : List_object_versions.options);
  ignore
    (List_object_versions.options
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:List_object_versions.Delimiter.slash
       ~key_marker:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : (List_object_versions.options, Error.t) result);
  ignore
    (List_object_versions.options_exn
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:List_object_versions.Delimiter.slash
       ~key_marker:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : List_object_versions.options);
  ignore (Create_bucket.default_options : Create_bucket.options);
  ignore
    ({ Create_bucket.region = Some (Awskit.Region.of_string_exn "us-west-2") }
      : Create_bucket.options);
  ignore
    (Create_bucket.options ~region:(Awskit.Region.of_string_exn "us-west-2") ()
      : (Create_bucket.options, Error.t) result);
  ignore
    (Create_bucket.options_exn
       ~region:(Awskit.Region.of_string_exn "us-west-2")
       ()
      : Create_bucket.options);
  ignore (Endpoint_config.aws () : endpoint_config);
  let local_endpoint =
    Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9000 ()
  in
  let signing_region = Awskit.Region.of_string_exn "us-east-1" in
  let local =
    Endpoint_config.local_plaintext ~endpoint:local_endpoint ~signing_region
      ~addressing_style:`Path ()
    |> ok_or_fail "local endpoint config"
  in
  let unsafe =
    Endpoint_config.unsafe_plaintext ~endpoint:local_endpoint ~signing_region
      ~addressing_style:`Path ()
  in
  ignore (local : endpoint_config);
  ignore (unsafe : endpoint_config);
  ignore (Delete_bucket.default_options : Delete_bucket.options);
  ignore
    (Delete_bucket.options ~expected_bucket_owner:owner ()
      : (Delete_bucket.options, Error.t) result);
  ignore
    (Delete_bucket.options_exn ~expected_bucket_owner:owner ()
      : Delete_bucket.options);
  ignore (Head_bucket.default_options : Head_bucket.options);
  ignore
    (Head_bucket.options ~expected_bucket_owner:owner ()
      : (Head_bucket.options, Error.t) result);
  ignore
    (Head_bucket.options_exn ~expected_bucket_owner:owner ()
      : Head_bucket.options);
  ignore (Bucket.Policy.default_options : Bucket.Policy.options);
  ignore
    (Bucket.Policy.options ~expected_bucket_owner:owner ()
      : (Bucket.Policy.options, Error.t) result);
  ignore
    (Bucket.Policy.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Policy.options);
  ignore (Bucket.Versioning.default_options : Bucket.Versioning.options);
  ignore
    (Bucket.Versioning.options ~expected_bucket_owner:owner ()
      : (Bucket.Versioning.options, Error.t) result);
  ignore
    (Bucket.Versioning.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Versioning.options);
  ignore (Bucket.Tagging.default_options : Bucket.Tagging.options);
  ignore
    (Bucket.Tagging.options ~expected_bucket_owner:owner ()
      : (Bucket.Tagging.options, Error.t) result);
  ignore
    (Bucket.Tagging.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Tagging.options);
  ignore (Bucket.Encryption.default_options : Bucket.Encryption.options);
  ignore
    (Bucket.Encryption.options ~expected_bucket_owner:owner ()
      : (Bucket.Encryption.options, Error.t) result);
  ignore
    (Bucket.Encryption.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Encryption.options);
  ignore (Bucket.Cors.default_options : Bucket.Cors.options);
  ignore
    (Bucket.Cors.options ~expected_bucket_owner:owner ()
      : (Bucket.Cors.options, Error.t) result);
  ignore
    (Bucket.Cors.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Cors.options);
  ignore
    (Bucket.Public_access_block.default_options
      : Bucket.Public_access_block.options);
  ignore
    (Bucket.Public_access_block.options ~expected_bucket_owner:owner ()
      : (Bucket.Public_access_block.options, Error.t) result);
  ignore
    (Bucket.Public_access_block.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Public_access_block.options);
  ignore
    (Bucket.Ownership_controls.default_options
      : Bucket.Ownership_controls.options);
  ignore
    (Bucket.Ownership_controls.options ~expected_bucket_owner:owner ()
      : (Bucket.Ownership_controls.options, Error.t) result);
  ignore
    (Bucket.Ownership_controls.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Ownership_controls.options);
  ignore (None : Delete_bucket.result option);
  ignore (None : Head_bucket.result option);
  ignore (None : List_buckets.result option);
  ignore (Get_bucket_location.default_options : Get_bucket_location.options);
  ignore
    (Get_bucket_location.options ~expected_bucket_owner:owner ()
      : (Get_bucket_location.options, Error.t) result);
  ignore
    (Get_bucket_location.options_exn ~expected_bucket_owner:owner ()
      : Get_bucket_location.options);
  ignore (None : Get_bucket_location.result option);
  ignore
    (Create_multipart_upload.default_options : Create_multipart_upload.options);
  ignore (Upload_part.default_options : Upload_part.options);
  ignore
    (Transfer.default_upload_options.create_options
      : Create_multipart_upload.options);
  ignore
    (Transfer.default_upload_options.upload_part_options : Upload_part.options);
  ignore
    (Transfer.default_upload_options.list_parts_options : List_parts.options);
  ignore (Transfer.default_download_options.get_options : Get_object.options);
  ignore (None : Transfer.upload_result option);
  ignore (None : Transfer.download_result option);
  Alcotest.(check int64)
    "default upload threshold"
    (Int64.of_int Transfer.default_part_size)
    Transfer.default_multipart_threshold;
  Alcotest.(check bool)
    "put strategy" true
    (Transfer.upload_strategy
       (Transfer.Put
          {
            etag = None;
            version_id = None;
            checksum = { Object.Checksum.values = []; checksum_type = None };
            response = Awskit.Response.create_exn ~status:200 ();
          })
    = `Put);
  ignore (None : Complete_multipart_upload.result option);
  ignore (None : Abort_multipart_upload.result option);
  ignore (List_parts.default_options : List_parts.options)

let test_native_body_api_compiles () =
  let module S3 = Recording_s3 in
  let _empty : S3.Body.t = S3.Body.empty in
  let _string_body : S3.Body.t = S3.Body.of_string "hello" in
  let _bytes_body : S3.Body.t = S3.Body.of_bytes (Bytes.of_string "hello") in
  let _stream_body : S3.Body.t =
    S3.Body.of_stream ~content_length:11L ~write:(fun writer ->
        match S3.Body.Writer.write_string writer "hello " with
        | Error _ as error -> error
        | Ok () -> S3.Body.Writer.write_bytes writer (Bytes.of_string "world"))
  in
  let consume (reader : S3.Reader.t) =
    S3.Reader.to_bytes ~max_bytes:1_048_576L reader
  in
  ignore (consume : S3.Reader.t -> (bytes, Error.t) result);
  ignore
    (S3.Object.find_metadata
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Head_object.options ->
        unit ->
        (Head_object.result option, Error.t) result);
  ignore
    (S3.Object.find
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Get_object.options ->
        consume:(S3.Reader.t -> ('a, Error.t) result) ->
        unit ->
        ('a Get_object.result option, Error.t) result);
  ignore
    (S3.Object.Tagging.put
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Tagging.options ->
        tags:Tag.Set.t ->
        unit ->
        (Awskit.Response.t, Error.t) result);
  ignore
    (S3.Bucket.head
      : S3.connection ->
        bucket:Bucket_name.t ->
        ?options:Head_bucket.options ->
        unit ->
        (Head_bucket.result, Error.t) result);
  ignore
    (S3.Bucket.Policy.put
      : S3.connection ->
        bucket:Bucket_name.t ->
        ?options:Bucket.Policy.options ->
        policy:Policy.t ->
        unit ->
        (Awskit.Response.t, Error.t) result);
  ignore
    (S3.Bucket.Versioning.put
      : S3.connection ->
        bucket:Bucket_name.t ->
        ?options:Bucket.Versioning.options ->
        status:Bucket.Versioning.Status.t ->
        unit ->
        (Awskit.Response.t, Error.t) result);
  ignore
    (S3.Bucket.Encryption.put
      : S3.connection ->
        bucket:Bucket_name.t ->
        ?options:Bucket.Encryption.options ->
        config:Bucket.Encryption.config ->
        unit ->
        (Awskit.Response.t, Error.t) result);
  ()

let service_error ?code ?message status =
  Awskit.Error.Internal.service ~status ?code ?message ~headers:[] ()

let test_error_classifiers () =
  let precondition = service_error ~code:"PreconditionFailed" 412 in
  let conditional_conflict =
    service_error ~code:"ConditionalRequestConflict" 409
  in
  let generic_conflict = service_error 409 in
  Alcotest.(check bool)
    "precondition failed" true
    (Error.is_precondition_failed precondition);
  Alcotest.(check bool)
    "conditional request conflict by code" true
    (Error.is_conditional_request_conflict conditional_conflict);
  Alcotest.(check bool)
    "generic 409 is not conditional conflict" false
    (Error.is_conditional_request_conflict generic_conflict);
  Alcotest.(check bool)
    "conditional failure includes precondition" true
    (Error.is_conditional_failure precondition);
  Alcotest.(check bool)
    "conditional failure includes conflict" true
    (Error.is_conditional_failure conditional_conflict)

let suite =
  [
    ( "api",
      [
        Alcotest.test_case "public operation aliases" `Quick
          test_public_operation_aliases;
        Alcotest.test_case "native body api compiles" `Quick
          test_native_body_api_compiles;
        Alcotest.test_case "error classifiers" `Quick test_error_classifiers;
      ] );
  ]
