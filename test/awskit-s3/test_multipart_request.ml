open Awskit_s3
open Awskit_s3_test

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let expect_validation_field label field = function
  | Error error when is_validation_field field error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation field %s" label field

let upload_handle () =
  Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
    ~key:(object_key "large.bin")
    ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")

let complete_part ?size part_number =
  Multipart.Part.create_exn
    ~part_number:(Multipart.Part_number.of_int_exn part_number)
    ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" part_number))
    ?size ()

let check_no_requests label conn =
  Alcotest.(check int) label 0 (List.length conn.Recording_runtime.calls)

let test_multipart_upload_part_checksum_headers () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("etag", "\"etag-1\"");
              ("x-amz-checksum-sha256", "provided-sha256");
            ]
          "";
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  let checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"provided-sha256"
  in
  let options =
    { Multipart.Upload_part.default_options with checksum = Some checksum }
  in
  let part =
    Recording_s3.Multipart.upload_part conn ~upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Recording_runtime.string_request_body "hello")
      ~options ()
    |> ok_or_fail "upload part checksum"
  in
  check_checksum "part response checksum" Object.Checksum.Algorithm.Sha256
    "provided-sha256" part.checksum;
  let call = Recording_runtime.last_call conn in
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" call.request.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha256")
    (header "x-amz-checksum-sha256" call.request.headers);
  Alcotest.(check (option (list string)))
    "part number" (Some [ "1" ])
    (List.assoc_opt "partNumber" call.request.target.query)

let test_multipart_unknown_storage_class_rejected () =
  let unknown_storage = Storage_class.Unknown "FUTURE_CLASS" in
  expect_validation_field "create builder storage" "storage_class"
    (Multipart.Create.options ~storage_class:unknown_storage ());
  let options =
    {
      Multipart.Create.default_options with
      storage_class = Some unknown_storage;
    }
  in
  let conn = Recording_runtime.connect [] in
  expect_validation_field "create operation storage" "storage_class"
    (Recording_s3.Multipart.create_upload conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "large.bin") ~options ());
  check_no_requests "create not sent" conn

let test_multipart_checksum_and_expected_owner_headers () =
  let expected_owner = account_id "123456789012" in
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<InitiateMultipartUploadResult><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>";
        response 200
          ~headers:
            [
              ("etag", "\"etag-1\"");
              ("x-amz-checksum-sha256", "provided-sha256");
            ]
          "";
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag><ChecksumSHA256>complete-sha256</ChecksumSHA256><ChecksumType>COMPOSITE</ChecksumType></CompleteMultipartUploadResult>|};
      ]
  in
  let create_options =
    {
      Multipart.Create.default_options with
      checksum_algorithm = Some Object.Checksum.Algorithm.Sha256;
      checksum_type = Some Object.Checksum.Type.Composite;
      expected_bucket_owner = Some expected_owner;
    }
  in
  let upload =
    Recording_s3.Multipart.create_upload conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~options:create_options ()
    |> ok_or_fail "create multipart checksum"
  in
  let part_checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"provided-sha256"
  in
  let upload_options =
    {
      Multipart.Upload_part.checksum = Some part_checksum;
      expected_bucket_owner = Some expected_owner;
    }
  in
  let part =
    Recording_s3.Multipart.upload_part conn ~upload:upload.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Recording_runtime.string_request_body "hello")
      ~options:upload_options ()
    |> ok_or_fail "upload part checksum"
  in
  (match Multipart.Part.checksum part.part with
  | Some checksum ->
      Alcotest.(check bool)
        "part carries response checksum algorithm" true
        (checksum.algorithm = Object.Checksum.Algorithm.Sha256);
      Alcotest.(check string)
        "part carries response checksum value" "provided-sha256" checksum.value
  | None -> Alcotest.fail "expected part checksum");
  let complete_checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"top-sha256"
  in
  let complete_options =
    {
      Multipart.Complete.expected_bucket_owner = Some expected_owner;
      checksum = Some complete_checksum;
      checksum_type = Some Object.Checksum.Type.Composite;
      multipart_object_size = Some 5L;
    }
  in
  let complete =
    Recording_s3.Multipart.complete_upload conn ~upload:upload.upload
      ~options:complete_options ~parts:[ part.part ] ()
    |> ok_or_fail "complete multipart checksum"
  in
  check_checksum "complete xml checksum" Object.Checksum.Algorithm.Sha256
    "complete-sha256" complete.checksum;
  Alcotest.(check bool)
    "complete checksum type" true
    (complete.checksum.checksum_type = Some Object.Checksum.Type.Composite);
  match List.rev conn.calls with
  | [ create; upload_part; complete ] ->
      Alcotest.(check (option string))
        "create checksum algorithm" (Some "SHA256")
        (header "x-amz-checksum-algorithm" create.request.headers);
      Alcotest.(check (option string))
        "create checksum type" (Some "COMPOSITE")
        (header "x-amz-checksum-type" create.request.headers);
      Alcotest.(check (option string))
        "upload part checksum value" (Some "provided-sha256")
        (header "x-amz-checksum-sha256" upload_part.request.headers);
      Alcotest.(check (option string))
        "upload part no algorithm" None
        (header "x-amz-checksum-algorithm" upload_part.request.headers);
      Alcotest.(check (option string))
        "complete checksum value" (Some "top-sha256")
        (header "x-amz-checksum-sha256" complete.request.headers);
      Alcotest.(check (option string))
        "complete checksum type" (Some "COMPOSITE")
        (header "x-amz-checksum-type" complete.request.headers);
      Alcotest.(check (option string))
        "complete object size" (Some "5")
        (header "x-amz-mp-object-size" complete.request.headers);
      List.iter
        (fun (label, (call : Recording_runtime.call)) ->
          Alcotest.(check (option string))
            (label ^ " expected owner")
            (Some (Account_id.to_string expected_owner))
            (header "x-amz-expected-bucket-owner" call.request.headers))
        [
          ("create", create);
          ("upload part", upload_part);
          ("complete", complete);
        ];
      Alcotest.(check bool)
        "completion xml part checksum" true
        (string_contains
           ~substring:"<ChecksumSHA256>provided-sha256</ChecksumSHA256>"
           complete.body)
  | _ -> Alcotest.fail "expected three multipart calls"

let test_complete_multipart_retryable_embedded_error_retries_then_succeeds () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 slow_down;
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  let part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~etag:(Object.Etag.of_string_exn "\"part-1\"")
      ()
  in
  ignore
    (Recording_s3.Multipart.complete_upload conn ~upload ~parts:[ part ] ()
    |> ok_or_fail "complete after embedded SlowDown");
  Alcotest.(check int) "attempts" 2 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 1 (List.length conn.sleeps)

let test_complete_multipart_non_retryable_embedded_error_is_final () =
  let invalid_request =
    {|<Error><Code>InvalidRequest</Code><Message>invalid completion</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 invalid_request;
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  let upload = upload_handle () in
  let part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~etag:(Object.Etag.of_string_exn "\"part-1\"")
      ()
  in
  (match
     Recording_s3.Multipart.complete_upload conn ~upload ~parts:[ part ] ()
   with
  | Error error when Error.service_code error = Some "InvalidRequest" -> ()
  | Error error -> Alcotest.failf "unexpected complete error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded complete error");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_complete_multipart_wrong_root_mentions_result_context () =
  let conn =
    Recording_runtime.connect [ response 200 "<NotCompleteMultipartUpload/>" ]
  in
  let upload = upload_handle () in
  let part = complete_part 1 in
  match
    Recording_s3.Multipart.complete_upload conn ~upload ~parts:[ part ] ()
  with
  | Error error -> (
      let text = Awskit.Error.to_string_hum error in
      match Awskit.Error.kind error with
      | Decode _ ->
          Alcotest.(check bool)
            "mentions CompleteMultipartUploadResult XML" true
            (string_contains text ~substring:"CompleteMultipartUploadResult XML")
      | _ -> Alcotest.failf "unexpected complete error: %a" Error.pp error)
  | Ok _ -> Alcotest.fail "expected wrong-root complete result decode error"

let test_abort_multipart_result_and_absent_error () =
  let absent_body =
    {|<Error><Code>NoSuchUpload</Code><Message>upload not found</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 204 ""; response 404 absent_body ]
  in
  let upload = upload_handle () in
  let aborted =
    Recording_s3.Multipart.abort_upload conn ~upload ()
    |> ok_or_fail "abort multipart"
  in
  Alcotest.(check int)
    "abort response status" 204
    (Awskit.Response.status aborted.response);
  (match Recording_s3.Multipart.abort_upload conn ~upload () with
  | Error error when Error.service_code error = Some "NoSuchUpload" -> ()
  | Error error ->
      Alcotest.failf "unexpected repeated abort error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected repeated abort service error");
  match List.rev conn.calls with
  | [ first; second ] ->
      List.iter
        (fun (label, (call : Recording_runtime.call)) ->
          check_method label "DELETE" call.request;
          Alcotest.(check (option (list string)))
            (label ^ " upload id") (Some [ "upload-1" ])
            (List.assoc_opt "uploadId" call.request.target.query))
        [ ("first abort", first); ("second abort", second) ]
  | _ -> Alcotest.fail "expected two abort calls"

let test_complete_multipart_revalidates_options_record () =
  let conn = Recording_runtime.connect [] in
  let options =
    {
      Multipart.Complete.default_options with
      multipart_object_size = Some (-1L);
    }
  in
  expect_validation_field "negative multipart object size"
    "multipart_object_size"
    (Recording_s3.Multipart.complete_upload conn ~upload:(upload_handle ())
       ~options
       ~parts:[ complete_part ~size:1L 1 ]
       ());
  check_no_requests "negative option did not send request" conn

let test_complete_multipart_rejects_known_undersized_non_final_part () =
  let conn = Recording_runtime.connect [] in
  expect_validation_field "undersized non-final part" "parts"
    (Recording_s3.Multipart.complete_upload conn ~upload:(upload_handle ())
       ~parts:[ complete_part ~size:1L 1; complete_part ~size:1L 2 ]
       ());
  check_no_requests "undersized parts did not send request" conn

let test_complete_multipart_rejects_known_object_size_mismatch () =
  let conn = Recording_runtime.connect [] in
  let first_size = Int64.of_int Transfer.min_part_size in
  let options =
    {
      Multipart.Complete.default_options with
      multipart_object_size = Some (Int64.add first_size 2L);
    }
  in
  expect_validation_field "multipart object size mismatch"
    "multipart_object_size"
    (Recording_s3.Multipart.complete_upload conn ~upload:(upload_handle ())
       ~options
       ~parts:[ complete_part ~size:first_size 1; complete_part ~size:1L 2 ]
       ());
  check_no_requests "size mismatch did not send request" conn

let test_complete_multipart_allows_unknown_part_sizes () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  let options =
    { Multipart.Complete.default_options with multipart_object_size = Some 1L }
  in
  ignore
    (Recording_s3.Multipart.complete_upload conn ~upload:(upload_handle ())
       ~options
       ~parts:[ complete_part 1; complete_part 2 ]
       ()
    |> ok_or_fail "complete unknown sizes");
  Alcotest.(check int)
    "unknown sizes sent request" 1
    (List.length conn.Recording_runtime.calls)

let suite =
  [
    ( "multipart request",
      [
        Alcotest.test_case "multipart upload part checksum headers" `Quick
          test_multipart_upload_part_checksum_headers;
        Alcotest.test_case "multipart unknown storage class rejected" `Quick
          test_multipart_unknown_storage_class_rejected;
        Alcotest.test_case "multipart checksum and expected owner headers"
          `Quick test_multipart_checksum_and_expected_owner_headers;
        Alcotest.test_case "complete multipart retryable embedded error" `Quick
          test_complete_multipart_retryable_embedded_error_retries_then_succeeds;
        Alcotest.test_case "complete multipart non-retryable embedded error"
          `Quick test_complete_multipart_non_retryable_embedded_error_is_final;
        Alcotest.test_case "complete multipart wrong root mentions context"
          `Quick test_complete_multipart_wrong_root_mentions_result_context;
        Alcotest.test_case "abort multipart result and absent upload error"
          `Quick test_abort_multipart_result_and_absent_error;
        Alcotest.test_case "complete revalidates public option record" `Quick
          test_complete_multipart_revalidates_options_record;
        Alcotest.test_case "complete rejects undersized known non-final part"
          `Quick test_complete_multipart_rejects_known_undersized_non_final_part;
        Alcotest.test_case "complete rejects known object size mismatch" `Quick
          test_complete_multipart_rejects_known_object_size_mismatch;
        Alcotest.test_case "complete allows unknown part sizes" `Quick
          test_complete_multipart_allows_unknown_part_sizes;
      ] );
  ]
