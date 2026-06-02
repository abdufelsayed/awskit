open Awskit_s3
open Awskit_s3_test

let test_bucket_head_request () =
  let conn =
    Recording_runtime.connect
      [ response 200 ~headers:[ ("x-amz-bucket-region", "us-west-2") ] "" ]
  in
  let info =
    Recording_s3.Bucket.head conn ~bucket:"my-bucket"
    |> ok_or_fail "bucket head"
  in
  Alcotest.(check (option string))
    "region" (Some "us-west-2")
    (Option.map Awskit.Region.to_string info.region);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string)
    "host" "my-bucket.s3.us-east-1.amazonaws.com" call.request.target.host;
  Alcotest.(check string) "path" "/" call.request.target.path

let test_bucket_list_parse () =
  let body =
    {|<ListAllMyBucketsResult><Buckets><Bucket><Name>alpha</Name><CreationDate>2026-04-08T12:00:00Z</CreationDate></Bucket><Bucket><Name>zeta</Name></Bucket></Buckets></ListAllMyBucketsResult>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let list_result = Recording_s3.Bucket.list conn |> ok_or_fail "bucket list" in
  Alcotest.(check (list string))
    "bucket names" [ "alpha"; "zeta" ]
    (List.map (fun (bucket : Bucket.info) -> bucket.name) list_result.buckets);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string)
    "root host" "s3.us-east-1.amazonaws.com" call.request.target.host

let expected_owner = "123456789012"

let check_expected_owner_header label (call : Recording_runtime.call) =
  Alcotest.(check (option string))
    label (Some expected_owner)
    (header "x-amz-expected-bucket-owner" call.request.headers)

let check_no_expected_owner_header label (call : Recording_runtime.call) =
  Alcotest.(check (option string))
    label None
    (header "x-amz-expected-bucket-owner" call.request.headers)

let test_bucket_expected_owner_headers () =
  let conn =
    Recording_runtime.connect
      [ response 200 ~headers:[ ("x-amz-bucket-region", "us-west-2") ] "" ]
  in
  ignore
    (Recording_s3.Bucket.head conn ~bucket:"my-bucket"
       ~expected_bucket_owner:expected_owner
    |> ok_or_fail "head expected owner");
  check_expected_owner_header "head expected owner"
    (Recording_runtime.last_call conn);
  let conn =
    Recording_runtime.connect
      [ response 200 "<LocationConstraint>us-west-2</LocationConstraint>" ]
  in
  ignore
    (Recording_s3.Bucket.get_location conn ~bucket:"my-bucket"
       ~expected_bucket_owner:expected_owner
    |> ok_or_fail "location expected owner");
  check_expected_owner_header "location expected owner"
    (Recording_runtime.last_call conn);
  let conn =
    Recording_runtime.connect
      [
        response 200
          {|<VersioningConfiguration><Status>Enabled</Status></VersioningConfiguration>|};
      ]
  in
  ignore
    (Recording_s3.Bucket.Versioning.get conn ~bucket:"my-bucket"
       ~expected_bucket_owner:expected_owner
    |> ok_or_fail "xml get expected owner");
  check_expected_owner_header "xml get expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.Versioning.put conn ~bucket:"my-bucket"
       ~expected_bucket_owner:expected_owner Bucket.Versioning.Status.Enabled
    |> ok_or_fail "xml put expected owner");
  check_expected_owner_header "xml put expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 204 "" ] in
  ignore
    (Recording_s3.Bucket.Tagging.delete conn ~bucket:"my-bucket"
       ~expected_bucket_owner:expected_owner
    |> ok_or_fail "xml delete expected owner");
  check_expected_owner_header "xml delete expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.create conn ~bucket:"new-bucket" ()
    |> ok_or_fail "create no expected owner");
  check_no_expected_owner_header "create no expected owner"
    (Recording_runtime.last_call conn);
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<ListAllMyBucketsResult><Buckets/></ListAllMyBucketsResult>";
      ]
  in
  ignore (Recording_s3.Bucket.list conn |> ok_or_fail "list no expected owner");
  check_no_expected_owner_header "list no expected owner"
    (Recording_runtime.last_call conn)

let suite =
  [
    ( "bucket request",
      [
        Alcotest.test_case "bucket head request" `Quick test_bucket_head_request;
        Alcotest.test_case "bucket list parse" `Quick test_bucket_list_parse;
        Alcotest.test_case "bucket expected owner headers" `Quick
          test_bucket_expected_owner_headers;
      ] );
  ]
