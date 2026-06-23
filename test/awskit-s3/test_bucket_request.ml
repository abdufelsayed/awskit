open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let test_bucket_head_request () =
  let conn =
    Recording_runtime.connect
      [ response 200 ~headers:[ ("x-amz-bucket-region", "us-west-2") ] "" ]
  in
  let info =
    Recording_s3.Bucket.head conn ~bucket:(bucket_name "my-bucket") ()
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
    (List.map
       (fun (bucket : Bucket.info) -> Bucket_name.to_string bucket.name)
       list_result.buckets);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string)
    "root host" "s3.us-east-1.amazonaws.com" call.request.target.host

let test_bucket_list_rejects_invalid_bucket_names () =
  let body =
    {|<ListAllMyBucketsResult><Buckets><Bucket><Name>Bad_Bucket</Name></Bucket></Buckets></ListAllMyBucketsResult>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match Recording_s3.Bucket.list conn with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "unexpected list decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid bucket name decode error"

let test_bucket_create_accepts_typed_region () =
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let options =
    { Create_bucket.region = Some (Awskit.Region.of_string_exn "eu-west-1") }
  in
  ignore
    (Recording_s3.Bucket.create conn ~bucket:(bucket_name "new-bucket") ~options
       ()
    |> ok_or_fail "create bucket typed region");
  let call = Recording_runtime.last_call conn in
  Alcotest.(check bool)
    "location constraint" true
    (string_contains call.body
       ~substring:"<LocationConstraint>eu-west-1</LocationConstraint>")

let expected_owner_string = "123456789012"
let expected_owner = account_id expected_owner_string

let check_expected_owner_header label (call : Recording_runtime.call) =
  Alcotest.(check (option string))
    label (Some expected_owner_string)
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
    (Recording_s3.Bucket.head conn ~bucket:(bucket_name "my-bucket")
       ~options:
         (Bucket.Head.options_exn ~expected_bucket_owner:expected_owner ())
       ()
    |> ok_or_fail "head expected owner");
  check_expected_owner_header "head expected owner"
    (Recording_runtime.last_call conn);
  let conn =
    Recording_runtime.connect
      [ response 200 "<LocationConstraint>us-west-2</LocationConstraint>" ]
  in
  ignore
    (Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket")
       ~options:
         (Bucket.Get_location.options_exn ~expected_bucket_owner:expected_owner
            ())
       ()
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
    (Recording_s3.Bucket.Versioning.get conn ~bucket:(bucket_name "my-bucket")
       ~options:
         (Bucket.Versioning.options_exn ~expected_bucket_owner:expected_owner ())
       ()
    |> ok_or_fail "xml get expected owner");
  check_expected_owner_header "xml get expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.Versioning.put conn ~bucket:(bucket_name "my-bucket")
       ~options:
         (Bucket.Versioning.options_exn ~expected_bucket_owner:expected_owner ())
       ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "xml put expected owner");
  check_expected_owner_header "xml put expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 204 "" ] in
  ignore
    (Recording_s3.Bucket.Tagging.delete conn ~bucket:(bucket_name "my-bucket")
       ~options:
         (Bucket.Tagging.options_exn ~expected_bucket_owner:expected_owner ())
       ()
    |> ok_or_fail "xml delete expected owner");
  check_expected_owner_header "xml delete expected owner"
    (Recording_runtime.last_call conn);
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.create conn ~bucket:(bucket_name "new-bucket") ()
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
        Alcotest.test_case "bucket list rejects invalid bucket names" `Quick
          test_bucket_list_rejects_invalid_bucket_names;
        Alcotest.test_case "bucket create accepts typed region" `Quick
          test_bucket_create_accepts_typed_region;
        Alcotest.test_case "bucket expected owner headers" `Quick
          test_bucket_expected_owner_headers;
      ] );
  ]
