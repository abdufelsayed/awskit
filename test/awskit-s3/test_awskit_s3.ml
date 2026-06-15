open Awskit_s3_test

let test_bounded_response_body_error_mentions_context () =
  let oversized_body = String.make 1_048_577 'x' in
  let conn = Recording_runtime.connect [ response 500 oversized_body ] in
  match Recording_s3.Object.head conn ~bucket:"my-bucket" ~key:"file" () with
  | Ok _ -> Alcotest.fail "expected bounded response body error"
  | Error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions bounded response body" true
        (string_contains ~substring:"reading bounded response body" text)

let () =
  Alcotest.run "awskit-s3"
    (List.concat
       [
         [
           ( "awskit s3",
             [
               Alcotest.test_case "bounded response body error mentions context"
                 `Quick test_bounded_response_body_error_mentions_context;
             ] );
         ];
         Test_api.suite;
         Test_endpoint.suite;
         Test_presigned.suite;
         Test_bucket_request.suite;
         Test_bucket_xml.suite;
         Test_object_request.suite;
         Test_retry.suite;
         Test_paginator.suite;
         Test_multipart_request.suite;
       ])
