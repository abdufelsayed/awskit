open Awskit_s3
open Awskit_s3_test

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let test_presigned_result () =
  let result =
    Presigned.get_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt" ()
    |> ok_or_fail "presigned get"
  in
  Alcotest.(check string)
    "method" "GET"
    (Awskit.Request.Method.to_string
       (result.method_ :> Awskit.Request.Method.t));
  Alcotest.(check bool)
    "has signature" true
    (String.contains result.url '?'
    && String.contains result.url '='
    && String.contains result.url '&');
  Alcotest.(check bool)
    "virtual-hosted URL" true
    (String.starts_with ~prefix:"https://bucket.s3.us-east-1.amazonaws.com/"
       result.url)

let test_presigned_put_checksum_headers () =
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha1;
      value = "provided-sha1";
    }
  in
  let options =
    { Presigned.Put_object.default_options with checksum = Some checksum }
  in
  let result =
    Presigned.put_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options ()
    |> ok_or_fail "presigned put"
  in
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha1")
    (header "x-amz-checksum-sha1" result.signed_headers);
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" result.signed_headers);
  let signed_headers = signed_headers_or_fail result.url in
  Alcotest.(check bool)
    "signed checksum value" true
    (List.mem "x-amz-checksum-sha1" signed_headers)

let test_presigned_expected_bucket_owner_headers () =
  let owner = "123456789012" in
  let get_options =
    {
      Presigned.Get_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let get =
    Presigned.get_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options:get_options ()
    |> ok_or_fail "presigned get expected owner"
  in
  Alcotest.(check (option string))
    "get expected owner header" (Some owner)
    (header "x-amz-expected-bucket-owner" get.signed_headers);
  Alcotest.(check bool)
    "get signed expected owner" true
    (List.mem "x-amz-expected-bucket-owner" (signed_headers_or_fail get.url));
  let head =
    Presigned.head_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options:get_options ()
    |> ok_or_fail "presigned head expected owner"
  in
  Alcotest.(check (option string))
    "head expected owner header" (Some owner)
    (header "x-amz-expected-bucket-owner" head.signed_headers);
  Alcotest.(check bool)
    "head signed expected owner" true
    (List.mem "x-amz-expected-bucket-owner" (signed_headers_or_fail head.url));
  let put_options =
    {
      Presigned.Put_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let put =
    Presigned.put_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options:put_options ()
    |> ok_or_fail "presigned put expected owner"
  in
  Alcotest.(check (option string))
    "put expected owner header" (Some owner)
    (header "x-amz-expected-bucket-owner" put.signed_headers);
  Alcotest.(check bool)
    "put signed expected owner" true
    (List.mem "x-amz-expected-bucket-owner" (signed_headers_or_fail put.url));
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload_part_options =
    {
      Presigned.Upload_part.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let upload_part =
    Presigned.upload_part
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"large.bin"
      ~upload_id ~part_number:1 ~options:upload_part_options ()
    |> ok_or_fail "presigned upload-part expected owner"
  in
  Alcotest.(check (option string))
    "upload-part expected owner header" (Some owner)
    (header "x-amz-expected-bucket-owner" upload_part.signed_headers);
  Alcotest.(check bool)
    "upload-part signed expected owner" true
    (List.mem "x-amz-expected-bucket-owner"
       (signed_headers_or_fail upload_part.url));
  let delete_options =
    {
      Presigned.Delete_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let delete =
    Presigned.delete_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options:delete_options ()
    |> ok_or_fail "presigned delete expected owner"
  in
  Alcotest.(check (option string))
    "delete expected owner header" (Some owner)
    (header "x-amz-expected-bucket-owner" delete.signed_headers);
  Alcotest.(check bool)
    "delete signed expected owner" true
    (List.mem "x-amz-expected-bucket-owner" (signed_headers_or_fail delete.url))

let test_presigned_upload_part () =
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
      value = "provided-sha256";
    }
  in
  let options =
    { Presigned.Upload_part.default_options with checksum = Some checksum }
  in
  let result =
    Presigned.upload_part
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"large.bin"
      ~upload_id ~part_number:7 ~options ()
    |> ok_or_fail "presigned upload part"
  in
  Alcotest.(check string)
    "method" "PUT"
    (Awskit.Request.Method.to_string
       (result.method_ :> Awskit.Request.Method.t));
  Alcotest.(check (option (list string)))
    "part number" (Some [ "7" ])
    (query_param "partNumber" result.url);
  Alcotest.(check (option (list string)))
    "upload id" (Some [ "upload-1" ])
    (query_param "uploadId" result.url);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha256")
    (header "x-amz-checksum-sha256" result.signed_headers);
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" result.signed_headers);
  let signed_headers = signed_headers_or_fail result.url in
  Alcotest.(check bool)
    "signed checksum value" true
    (List.mem "x-amz-checksum-sha256" signed_headers);
  match
    Presigned.upload_part
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"large.bin"
      ~upload_id ~part_number:0 ()
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected invalid part number"

let test_presigned_rejects_header_newline () =
  let options =
    {
      Presigned.Put_object.default_options with
      extra_signed_headers = [ ("x-test", "ok\r\nInjected: yes") ];
    }
  in
  match
    Presigned.put_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options ()
  with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected header validation error"

let test_presigned_rejects_unknown_checksum () =
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Unknown "FUTURE";
      value = "value";
    }
  in
  let put_options =
    { Presigned.Put_object.default_options with checksum = Some checksum }
  in
  (match
     Presigned.put_object
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
       ~options:put_options ()
   with
  | Error error when is_validation_field "checksum_algorithm" error -> ()
  | Error error -> Alcotest.failf "unexpected put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected presigned put checksum validation");
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload_part_options =
    { Presigned.Upload_part.default_options with checksum = Some checksum }
  in
  match
    Presigned.upload_part
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"large.bin"
      ~upload_id ~part_number:1 ~options:upload_part_options ()
  with
  | Error error when is_validation_field "checksum_algorithm" error -> ()
  | Error error ->
      Alcotest.failf "unexpected upload part error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected presigned upload-part checksum validation"

let suite =
  [
    ( "presigned",
      [
        Alcotest.test_case "presigned result" `Quick test_presigned_result;
        Alcotest.test_case "presigned put checksum headers" `Quick
          test_presigned_put_checksum_headers;
        Alcotest.test_case "presigned expected owner headers" `Quick
          test_presigned_expected_bucket_owner_headers;
        Alcotest.test_case "presigned multipart upload part" `Quick
          test_presigned_upload_part;
        Alcotest.test_case "presigned rejects header newline" `Quick
          test_presigned_rejects_header_newline;
        Alcotest.test_case "presigned rejects unknown checksum" `Quick
          test_presigned_rejects_unknown_checksum;
      ] );
  ]
