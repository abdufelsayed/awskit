open Awskit_s3
open Awskit_s3_test

let test_multipart_upload_part_checksum_headers () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [ ("etag", "\"etag-1\""); ("x-amz-checksum-sha1", "provided-sha1") ]
          "";
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha1;
      value = "provided-sha1";
    }
  in
  let options = { Upload_part.default_options with checksum = Some checksum } in
  let part =
    Recording_s3.Multipart.upload_part conn ~bucket:"my-bucket" ~key:"large.bin"
      ~upload_id ~part_number:1
      ~body:(Recording_runtime.string_request_body "hello")
      ~options ()
    |> ok_or_fail "upload part checksum"
  in
  check_checksum "part response checksum" Object.Checksum.Algorithm.Sha1
    "provided-sha1" part.checksum;
  let call = Recording_runtime.last_call conn in
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" call.request.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha1")
    (header "x-amz-checksum-sha1" call.request.headers);
  Alcotest.(check (option (list string)))
    "part number" (Some [ "1" ])
    (List.assoc_opt "partNumber" call.request.target.query)

let test_multipart_checksum_and_expected_owner_headers () =
  let expected_owner = "123456789012" in
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<InitiateMultipartUploadResult><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>";
        response 200
          ~headers:
            [ ("etag", "\"etag-1\""); ("x-amz-checksum-sha1", "provided-sha1") ]
          "";
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag><ChecksumSHA256>complete-sha256</ChecksumSHA256><ChecksumType>COMPOSITE</ChecksumType></CompleteMultipartUploadResult>|};
      ]
  in
  let create_options =
    {
      Create_multipart_upload.default_options with
      checksum_algorithm = Some Object.Checksum.Algorithm.Sha256;
      checksum_type = Some Object.Checksum.Type.Composite;
      expected_bucket_owner = Some expected_owner;
    }
  in
  let upload =
    Recording_s3.Multipart.create_upload conn ~bucket:"my-bucket"
      ~key:"large.bin" ~options:create_options ()
    |> ok_or_fail "create multipart checksum"
  in
  let upload_id = upload.upload.upload_id in
  let part_checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha1;
      value = "provided-sha1";
    }
  in
  let upload_options =
    {
      Upload_part.checksum = Some part_checksum;
      expected_bucket_owner = Some expected_owner;
    }
  in
  let part =
    Recording_s3.Multipart.upload_part conn ~bucket:"my-bucket" ~key:"large.bin"
      ~upload_id ~part_number:1
      ~body:(Recording_runtime.string_request_body "hello")
      ~options:upload_options ()
    |> ok_or_fail "upload part checksum"
  in
  let complete_checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
      value = "top-sha256";
    }
  in
  let complete_options =
    {
      Complete_multipart_upload.expected_bucket_owner = Some expected_owner;
      checksum = Some complete_checksum;
      checksum_type = Some Object.Checksum.Type.Composite;
      multipart_object_size = Some 5L;
    }
  in
  let complete =
    Recording_s3.Multipart.complete_upload conn ~bucket:"my-bucket"
      ~key:"large.bin" ~upload_id ~options:complete_options [ part.part ]
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
        "upload part checksum value" (Some "provided-sha1")
        (header "x-amz-checksum-sha1" upload_part.request.headers);
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
            (Some expected_owner)
            (header "x-amz-expected-bucket-owner" call.request.headers))
        [
          ("create", create);
          ("upload part", upload_part);
          ("complete", complete);
        ];
      Alcotest.(check bool)
        "completion xml part checksum" true
        (string_contains ~substring:"<ChecksumSHA1>provided-sha1</ChecksumSHA1>"
           complete.body)
  | _ -> Alcotest.fail "expected three multipart calls"

let test_complete_multipart_embedded_error () =
  let body =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let part =
    Multipart.Part.create_exn ~part_number:1
      ~etag:(Object.Etag.of_string_exn "\"part-1\"")
      ()
  in
  match
    Recording_s3.Multipart.complete_upload conn ~bucket:"my-bucket"
      ~key:"large.bin" ~upload_id [ part ]
  with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected complete error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded complete error"

let suite =
  [
    ( "multipart request",
      [
        Alcotest.test_case "multipart upload part checksum headers" `Quick
          test_multipart_upload_part_checksum_headers;
        Alcotest.test_case "multipart checksum and expected owner headers"
          `Quick test_multipart_checksum_and_expected_owner_headers;
        Alcotest.test_case "complete multipart embedded error" `Quick
          test_complete_multipart_embedded_error;
      ] );
  ]
