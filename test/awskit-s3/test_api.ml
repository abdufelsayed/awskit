open Awskit_s3
open Awskit_s3_test

let test_operation_data_module_names () =
  ignore (Put_object.default_options : Put_object.options);
  ignore (Get_object.default_options : Get_object.options);
  ignore (Head_object.default_options : Head_object.options);
  ignore (Delete_object.default_options : Delete_object.options);
  let delete_object : Delete_objects.object_ =
    { key = "file.txt"; version_id = None; etag = None }
  in
  ignore (delete_object : Delete_objects.object_);
  ignore (None : Get_object.result option);
  ignore (Copy_object.default_options : Copy_object.options);
  ignore (List_objects_v2.default_options : List_objects_v2.options);
  let listed_object : List_objects_v2.object_summary =
    {
      key = "file.txt";
      size = Some 1L;
      etag = None;
      last_modified = None;
      storage_class = None;
      checksum = { Object.Checksum.algorithms = []; checksum_type = None };
    }
  in
  ignore (listed_object : List_objects_v2.object_summary);
  ignore (List_object_versions.default_options : List_object_versions.options);
  ignore (Create_bucket.default_options : Create_bucket.options);
  ignore (None : Delete_bucket.result option);
  ignore (None : Head_bucket.result option);
  ignore (None : List_buckets.result option);
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

let service_error ?code ?message status =
  Awskit.Error.service
    {
      status;
      code;
      message;
      request_id = None;
      host_id = None;
      headers = [];
      body = None;
    }

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
        Alcotest.test_case "operation data module names" `Quick
          test_operation_data_module_names;
        Alcotest.test_case "error classifiers" `Quick test_error_classifiers;
      ] );
  ]
