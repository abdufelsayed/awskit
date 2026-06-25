open Awskit_s3
open Awskit_s3_test

let test_public_operation_modules () =
  let owner = Account_id.of_string_exn "123456789012" in
  let content_type = Content_type.of_string_exn "text/plain" in
  let metadata = Metadata.of_list_exn [ ("source", "api") ] in
  let tags =
    Tag.Set.of_list_exn [ Tag.create_exn ~key:"project" ~value:"awskit" ]
  in
  ignore (Object.Put.default_options : Object.Put.options);
  ignore
    (Object.Put.options ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : (Object.Put.options, Error.t) result);
  ignore
    (Object.Put.options_exn ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : Object.Put.options);
  ignore (Object.Get.default_options : Object.Get.options);
  ignore
    (Object.Get.options ~expected_bucket_owner:owner ()
      : (Object.Get.options, Error.t) result);
  ignore
    (Object.Get.options_exn ~expected_bucket_owner:owner ()
      : Object.Get.options);
  ignore (Object.Head.default_options : Object.Head.options);
  ignore
    (Object.Head.options ~expected_bucket_owner:owner ()
      : (Object.Head.options, Error.t) result);
  ignore
    (Object.Head.options_exn ~expected_bucket_owner:owner ()
      : Object.Head.options);
  ignore (Object.Delete.default_options : Object.Delete.options);
  ignore
    (Object.Delete.options ~expected_bucket_owner:owner ()
      : (Object.Delete.options, Error.t) result);
  ignore
    (Object.Delete.options_exn ~expected_bucket_owner:owner ()
      : Object.Delete.options);
  let delete_object : Object.Delete_many.object_ =
    Object.Delete_many.object_ ~key:(object_key "file.txt") ()
  in
  ignore (delete_object : Object.Delete_many.object_);
  ignore (Object.Delete_many.max_objects : int);
  ignore
    (Object.Delete_many.options ~expected_bucket_owner:owner ()
      : (Object.Delete_many.options, Error.t) result);
  ignore
    (Object.Delete_many.options_exn ~expected_bucket_owner:owner ()
      : Object.Delete_many.options);
  ignore (None : string Object.Get.result option);
  ignore (None : Object.Get.info option);
  ignore (Object.Copy.default_options : Object.Copy.options);
  ignore
    (Object.Copy.options ~expected_bucket_owner:owner
       ~source_expected_bucket_owner:owner ()
      : (Object.Copy.options, Error.t) result);
  ignore
    (Object.Copy.options_exn ~expected_bucket_owner:owner
       ~source_expected_bucket_owner:owner ()
      : Object.Copy.options);
  ignore
    (Object.Tagging.options ~expected_bucket_owner:owner ()
      : (Object.Tagging.options, Error.t) result);
  ignore
    (Object.Tagging.options_exn ~expected_bucket_owner:owner ()
      : Object.Tagging.options);
  ignore (Object.List.default_options : Object.List.options);
  ignore
    (Object.List.options
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:Object.List.Delimiter.slash
       ~start_after:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : (Object.List.options, Error.t) result);
  ignore
    (Object.List.options_exn
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:Object.List.Delimiter.slash
       ~start_after:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : Object.List.options);
  let listed_object : Object.List.object_summary =
    {
      key = object_key "file.txt";
      size = Some 1L;
      etag = None;
      last_modified = None;
      storage_class = None;
      checksum = Object.Checksum.empty_summary;
    }
  in
  ignore (listed_object : Object.List.object_summary);
  ignore (Object.Versions.default_options : Object.Versions.options);
  ignore
    (Object.Versions.options
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:Object.Versions.Delimiter.slash
       ~key_marker:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : (Object.Versions.options, Error.t) result);
  ignore
    (Object.Versions.options_exn
       ~prefix:(Object_key.Prefix.of_string_exn "logs/")
       ~delimiter:Object.Versions.Delimiter.slash
       ~key_marker:(object_key "logs/0001.txt")
       ~max_keys:100 ~expected_bucket_owner:owner ()
      : Object.Versions.options);
  ignore (Bucket.Create.default_options : Bucket.Create.options);
  ignore
    ({ Bucket.Create.region = Some (Awskit.Region.of_string_exn "us-west-2") }
      : Bucket.Create.options);
  ignore
    (Bucket.Create.options ~region:(Awskit.Region.of_string_exn "us-west-2") ()
      : (Bucket.Create.options, Error.t) result);
  ignore
    (Bucket.Create.options_exn
       ~region:(Awskit.Region.of_string_exn "us-west-2")
       ()
      : Bucket.Create.options);
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
  ignore (Bucket.Delete.default_options : Bucket.Delete.options);
  ignore
    (Bucket.Delete.options ~expected_bucket_owner:owner ()
      : (Bucket.Delete.options, Error.t) result);
  ignore
    (Bucket.Delete.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Delete.options);
  ignore (Bucket.Head.default_options : Bucket.Head.options);
  ignore
    (Bucket.Head.options ~expected_bucket_owner:owner ()
      : (Bucket.Head.options, Error.t) result);
  ignore
    (Bucket.Head.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Head.options);
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
  ignore (None : Bucket.Delete.result option);
  ignore (None : Bucket.Head.result option);
  ignore (None : Bucket.List_buckets.result option);
  ignore (Bucket.Get_location.default_options : Bucket.Get_location.options);
  ignore
    (Bucket.Get_location.options ~expected_bucket_owner:owner ()
      : (Bucket.Get_location.options, Error.t) result);
  ignore
    (Bucket.Get_location.options_exn ~expected_bucket_owner:owner ()
      : Bucket.Get_location.options);
  ignore (None : Bucket.Get_location.result option);
  ignore (Multipart.Create.default_options : Multipart.Create.options);
  ignore
    (Multipart.Create.options ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : (Multipart.Create.options, Error.t) result);
  ignore
    (Multipart.Create.options_exn ~content_type ~metadata ~tags
       ~expected_bucket_owner:owner ()
      : Multipart.Create.options);
  ignore (Multipart.Upload_part.default_options : Multipart.Upload_part.options);
  let upload_checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"part-sha256"
  in
  ignore
    (Multipart.Upload_part.options ~checksum:upload_checksum
       ~expected_bucket_owner:owner ()
      : (Multipart.Upload_part.options, Error.t) result);
  ignore
    (Multipart.Upload_part.options_exn ~checksum:upload_checksum
       ~expected_bucket_owner:owner ()
      : Multipart.Upload_part.options);
  let multipart_part_number = Multipart.Part_number.of_int_exn 1 in
  let multipart_part =
    Multipart.Part.create_exn ~part_number:multipart_part_number
      ~etag:(Object.Etag.of_string_exn "\"etag-1\"")
      ~size:5_242_880L ()
  in
  ignore (multipart_part : Multipart.Part.t);
  let multipart_upload =
    Multipart.Upload.resume
      ~bucket:(bucket_name "multipart-api-bucket")
      ~key:(object_key "large.bin")
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")
  in
  ignore (multipart_upload : Multipart.Upload.caller_owned Multipart.Upload.t);
  ignore
    (Multipart.Complete.options
       ~checksum:
         (Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
            ~value:"full-sha256")
       ~checksum_type:Object.Checksum.Type.Composite
       ~multipart_object_size:5_242_880L ~expected_bucket_owner:owner ()
      : (Multipart.Complete.options, Error.t) result);
  ignore
    (Multipart.Complete.options_exn
       ~checksum:
         (Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
            ~value:"full-sha256")
       ~checksum_type:Object.Checksum.Type.Composite
       ~multipart_object_size:5_242_880L ~expected_bucket_owner:owner ()
      : Multipart.Complete.options);
  ignore
    (Multipart.Abort.options ~expected_bucket_owner:owner ()
      : (Multipart.Abort.options, Error.t) result);
  ignore
    (Multipart.Abort.options_exn ~expected_bucket_owner:owner ()
      : Multipart.Abort.options);
  let abort_result : Multipart.Abort.result =
    { response = Awskit.Response.create_exn ~status:204 () }
  in
  ignore (abort_result.response : Awskit.Response.t);
  ignore
    (Multipart.List_parts.options ~max_parts:1000
       ~part_number_marker:(Multipart.Part_number_marker.of_int_exn 2)
       ~expected_bucket_owner:owner ()
      : (Multipart.List_parts.options, Error.t) result);
  ignore
    (Multipart.List_parts.options_exn ~max_parts:1000
       ~part_number_marker:(Multipart.Part_number_marker.of_int_exn 2)
       ~expected_bucket_owner:owner ()
      : Multipart.List_parts.options);
  ignore
    (Transfer.default_upload_options.create_options : Multipart.Create.options);
  ignore
    (Transfer.default_upload_options.upload_part_options
      : Multipart.Upload_part.options);
  ignore
    (Transfer.default_upload_options.list_parts_options
      : Multipart.List_parts.options);
  ignore (Transfer.default_download_options.get_options : Object.Get.options);
  let transfer_upload_options =
    Transfer.upload_options_exn
      ~multipart_threshold:Transfer.default_multipart_threshold
      ~part_size:Transfer.default_part_size
      ~concurrency:Transfer.default_concurrency ()
  in
  ignore (transfer_upload_options : Transfer.upload_options);
  ignore (Transfer.upload_multipart_threshold transfer_upload_options : int64);
  ignore (Transfer.upload_part_size transfer_upload_options : int);
  ignore (Transfer.upload_concurrency transfer_upload_options : int);
  ignore
    (Transfer.upload_put_options transfer_upload_options : Object.Put.options);
  ignore
    (Transfer.upload_create_options transfer_upload_options
      : Multipart.Create.options);
  ignore
    (Transfer.upload_part_options transfer_upload_options
      : Multipart.Upload_part.options);
  ignore
    (Transfer.upload_complete_options transfer_upload_options
      : Multipart.Complete.options);
  ignore
    (Transfer.upload_abort_options transfer_upload_options
      : Multipart.Abort.options);
  ignore
    (Transfer.upload_list_parts_options transfer_upload_options
      : Multipart.List_parts.options);
  ignore
    (Transfer.upload_options ~part_size:1 ()
      : (Transfer.upload_options, Error.t) result);
  let transfer_download_options =
    Transfer.download_options_exn
      ~multipart_threshold:Transfer.default_multipart_threshold ~part_size:1
      ~concurrency:Transfer.default_concurrency
      ~overwrite:Transfer.Error_if_exists ()
  in
  ignore (transfer_download_options : Transfer.download_options);
  ignore
    (Transfer.download_multipart_threshold transfer_download_options : int64);
  ignore (Transfer.download_part_size transfer_download_options : int);
  ignore (Transfer.download_concurrency transfer_download_options : int);
  ignore
    (Transfer.download_overwrite transfer_download_options : Transfer.overwrite);
  ignore
    (Transfer.download_get_options transfer_download_options
      : Object.Get.options);
  ignore
    (Transfer.download_options ~part_size:0 ()
      : (Transfer.download_options, Error.t) result);
  let progress =
    Transfer.progress ~direction:Transfer.Upload ~phase:Transfer.Part
      ~transferred:1L ~total:2L ~part_number:multipart_part_number ()
  in
  ignore (progress.direction : Transfer.direction);
  ignore (progress.phase : Transfer.phase);
  ignore (progress.transferred : int64);
  ignore (progress.total : int64 option);
  ignore (progress.part_number : Multipart.Part_number.t option);
  ignore (None : Transfer.upload_result option);
  ignore (None : Transfer.download_result option);
  Alcotest.(check int64)
    "default upload threshold"
    (Int64.of_int Transfer.default_part_size)
    Transfer.default_multipart_threshold;
  let put_result : Object.Put.result =
    {
      etag = None;
      version_id = None;
      checksum = { Object.Checksum.values = []; checksum_type = None };
      response = Awskit.Response.create_exn ~status:200 ();
    }
  in
  let transfer_put_result : Transfer.put_upload_result =
    { put = put_result; bytes_transferred = 5L }
  in
  ignore (transfer_put_result.put : Object.Put.result);
  ignore (transfer_put_result.bytes_transferred : int64);
  let transfer_upload_result = Transfer.Put transfer_put_result in
  Alcotest.(check bool)
    "put strategy" true
    (Transfer.upload_strategy transfer_upload_result = `Put);
  Alcotest.(check int64)
    "put bytes" 5L
    (Transfer.upload_bytes_transferred transfer_upload_result);
  let transfer_multipart_result : Transfer.multipart_upload_result =
    {
      upload = multipart_upload;
      parts = [];
      complete =
        {
          etag = None;
          version_id = None;
          checksum = { Object.Checksum.values = []; checksum_type = None };
          response = Awskit.Response.create_exn ~status:200 ();
        };
      bytes_transferred = 8L;
    }
  in
  ignore
    (transfer_multipart_result.upload
      : Multipart.Upload.caller_owned Multipart.Upload.t);
  ignore (transfer_multipart_result.parts : Multipart.Part.t list);
  ignore (transfer_multipart_result.complete : Multipart.Complete.result);
  ignore (transfer_multipart_result.bytes_transferred : int64);
  ignore
    (Transfer.upload_bytes_transferred
       (Transfer.Multipart transfer_multipart_result)
      : int64);
  let transfer_get_result : Transfer.get_download_result =
    {
      info =
        {
          etag = None;
          content_type = None;
          content_length = Some 5L;
          content_range = None;
          last_modified = None;
          metadata = Metadata.empty;
          storage_class = None;
          version_id = None;
          checksum = { Object.Checksum.values = []; checksum_type = None };
          encryption = None;
          response = Awskit.Response.create_exn ~status:200 ();
        };
      bytes_transferred = 5L;
    }
  in
  ignore (transfer_get_result.info : Object.Get.info);
  ignore (transfer_get_result.bytes_transferred : int64);
  let transfer_download_result = Transfer.Get transfer_get_result in
  Alcotest.(check bool)
    "get strategy" true
    (Transfer.download_strategy transfer_download_result = `Get);
  Alcotest.(check int64)
    "get bytes" 5L
    (Transfer.download_bytes_transferred transfer_download_result);
  let transfer_ranged_result : Transfer.ranged_download_result =
    {
      info =
        {
          etag = None;
          content_type = None;
          content_length = Some 8L;
          content_range = None;
          last_modified = None;
          metadata = Metadata.empty;
          storage_class = None;
          version_id = None;
          checksum = { Object.Checksum.values = []; checksum_type = None };
          encryption = None;
          response = Awskit.Response.create_exn ~status:200 ();
        };
      parts = 2;
      bytes_transferred = 8L;
    }
  in
  ignore (transfer_ranged_result.info : Object.Head.result);
  ignore (transfer_ranged_result.parts : int);
  ignore (transfer_ranged_result.bytes_transferred : int64);
  ignore
    (Transfer.download_bytes_transferred
       (Transfer.Ranged transfer_ranged_result)
      : int64);
  ignore (None : Multipart.Complete.result option);
  ignore (None : Multipart.Abort.result option);
  ignore (Multipart.List_parts.default_options : Multipart.List_parts.options)

let test_native_body_api_compiles () =
  let module S3 = Recording_s3 in
  let _empty : S3.Body.t = S3.Body.empty in
  let _string_body : S3.Body.t = S3.Body.of_string "hello" in
  let _bytes_body : S3.Body.t = S3.Body.of_bytes (Bytes.of_string "hello") in
  let _stream_body : S3.Body.t =
    S3.Body.of_stream ~content_length:11L ~replayable:false
      ~write:(fun writer ->
        match S3.Body.Writer.write_string writer "hello " with
        | Error _ as error -> error
        | Ok () -> S3.Body.Writer.write_bytes writer (Bytes.of_string "world"))
    |> ok_or_fail "stream body"
  in
  ignore (S3.Body.content_length _stream_body : int64 option);
  ignore (S3.Body.replayable _stream_body : bool);
  let consume (reader : S3.Reader.t) =
    S3.Reader.to_bytes ~max_bytes:1_048_576L reader
  in
  ignore (consume : S3.Reader.t -> (bytes, Error.t) result);
  ignore
    (S3.Object.find_metadata
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Head.options ->
        unit ->
        (Object.Head.result option, Error.t) result);
  ignore
    (S3.Object.find
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Get.options ->
        consume:(S3.Reader.t -> ('a, Error.t) result) ->
        unit ->
        ('a Object.Get.result option, Error.t) result);
  ignore
    (S3.Object.put_string
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Put.options ->
        contents:string ->
        unit ->
        (Object.Put.result, Error.t) result);
  ignore
    (S3.Object.put_bytes
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Put.options ->
        contents:bytes ->
        unit ->
        (Object.Put.result, Error.t) result);
  ignore
    (S3.Object.get_string
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Get.options ->
        max_bytes:int64 ->
        unit ->
        (string Object.Get.result, Error.t) result);
  ignore
    (S3.Object.get_bytes
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Get.options ->
        max_bytes:int64 ->
        unit ->
        (bytes Object.Get.result, Error.t) result);
  ignore
    (S3.Object.find_string
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Get.options ->
        max_bytes:int64 ->
        unit ->
        (string Object.Get.result option, Error.t) result);
  ignore
    (S3.Object.find_bytes
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Object.Get.options ->
        max_bytes:int64 ->
        unit ->
        (bytes Object.Get.result option, Error.t) result);
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
        ?options:Bucket.Head.options ->
        unit ->
        (Bucket.Head.result, Error.t) result);
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

let test_presigned_api_compiles () =
  let module S3 = Recording_s3 in
  ignore (Presigned.method_ : Presigned.result -> Presigned.method_);
  ignore (Presigned.safe_uri : Presigned.result -> Uri.t);
  ignore (Presigned.signed_headers : Presigned.result -> (string * string) list);
  ignore
    (Presigned.request_headers : Presigned.result -> (string * string) list);
  ignore (Presigned.reveal_url : Presigned.result -> string);
  ignore
    (S3.Presigned.get_object
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Presigned.Get_object.options ->
        unit ->
        (Presigned.result, Error.t) result);
  ignore
    (S3.Presigned.put_object
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Presigned.Put_object.options ->
        unit ->
        (Presigned.result, Error.t) result);
  ignore
    (S3.Presigned.head_object
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Presigned.Head_object.options ->
        unit ->
        (Presigned.result, Error.t) result);
  ignore
    (S3.Presigned.delete_object
      : S3.connection ->
        bucket:Bucket_name.t ->
        key:Object_key.t ->
        ?options:Presigned.Delete_object.options ->
        unit ->
        (Presigned.result, Error.t) result);
  ignore
    (S3.Presigned.upload_part
      : S3.connection ->
        upload:_ Multipart.Upload.t ->
        part_number:Multipart.Part_number.t ->
        ?options:Presigned.Upload_part.options ->
        unit ->
        (Presigned.result, Error.t) result);
  ()

let service_error ?code ?message status =
  Awskit.Error.Producer.service ~status ?code ?message ~headers:[] ()

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
        Alcotest.test_case "public operation modules" `Quick
          test_public_operation_modules;
        Alcotest.test_case "native body api compiles" `Quick
          test_native_body_api_compiles;
        Alcotest.test_case "presigned api compiles" `Quick
          test_presigned_api_compiles;
        Alcotest.test_case "error classifiers" `Quick test_error_classifiers;
      ] );
  ]
