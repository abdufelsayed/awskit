open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let service_headers error =
  match Awskit.Error.kind error with
  | Service service -> service.headers
  | _ -> Alcotest.failf "expected service error: %a" Error.pp error

let service_details error =
  match Awskit.Error.kind error with
  | Service service -> service
  | _ -> Alcotest.failf "expected service error: %a" Error.pp error

let check_query label expected (call : Recording_runtime.call) =
  Alcotest.(check (list (pair string (list string))))
    label expected call.request.target.query

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
  check_method "method" "HEAD" call.request;
  Alcotest.(check string)
    "host" "my-bucket.s3.us-east-1.amazonaws.com" call.request.target.host;
  Alcotest.(check string) "path" "/" call.request.target.path

let test_bucket_exists_uses_head () =
  let conn = Recording_runtime.connect [ response 404 "" ] in
  let exists =
    Recording_s3.Bucket.exists conn ~bucket:(bucket_name "missing-bucket") ()
    |> ok_or_fail "bucket exists"
  in
  Alcotest.(check bool) "exists" false exists;
  let call = Recording_runtime.last_call conn in
  check_method "exists method" "HEAD" call.request

let test_bucket_head_error_preserves_region_hints () =
  List.iter
    (fun status ->
      let conn =
        Recording_runtime.connect
          [
            response status
              ~headers:[ ("x-amz-bucket-region", "eu-central-1") ]
              "";
          ]
      in
      match
        Recording_s3.Bucket.head conn ~bucket:(bucket_name "my-bucket") ()
      with
      | Error error when Awskit.Error.service_status error = Some status ->
          let call = Recording_runtime.last_call conn in
          check_method (Fmt.str "status %d method" status) "HEAD" call.request;
          Alcotest.(check int)
            (Fmt.str "status %d attempts" status)
            1 (List.length conn.calls);
          Alcotest.(check (option string))
            (Fmt.str "status %d region" status)
            (Some "eu-central-1")
            (header "x-amz-bucket-region" (service_headers error))
      | Error error ->
          Alcotest.failf "unexpected status %d error: %a" status Error.pp error
      | Ok _ -> Alcotest.failf "expected status %d service error" status)
    [ 301; 307; 400; 403; 404 ]

let test_bucket_head_rejects_malformed_region_hint () =
  let conn =
    Recording_runtime.connect
      [ response 200 ~headers:[ ("x-amz-bucket-region", "") ] "" ]
  in
  match Recording_s3.Bucket.head conn ~bucket:(bucket_name "my-bucket") () with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions region header" true
        (string_contains text ~substring:"x-amz-bucket-region")
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed region decode error"

let test_service_error_extracts_body_fields () =
  let body =
    {|<Error><Code> SlowDown </Code><Message> reduce request rate </Message><RequestId>req-body</RequestId><HostId>host-body</HostId></Error>|}
  in
  let cases =
    [
      ("body ids", [], Some "req-body", Some "host-body");
      ( "header ids",
        [ ("x-amz-request-id", "req-header"); ("x-amz-id-2", "host-header") ],
        Some "req-header",
        Some "host-header" );
    ]
  in
  List.iter
    (fun (label, headers, request_id, host_id) ->
      let conn =
        Recording_runtime.connect ~retry_policy:Awskit.Retry.disabled
          [ response 503 ~headers body ]
      in
      match
        Recording_s3.Bucket.delete conn ~bucket:(bucket_name "my-bucket") ()
      with
      | Error error ->
          let service = service_details error in
          Alcotest.(check int) (label ^ " status") 503 service.status;
          Alcotest.(check (option string))
            (label ^ " code") (Some "SlowDown") service.code;
          Alcotest.(check (option string))
            (label ^ " message") (Some "reduce request rate") service.message;
          Alcotest.(check (option string))
            (label ^ " request id") request_id service.request_id;
          Alcotest.(check (option string))
            (label ^ " host id") host_id service.host_id
      | Ok _ -> Alcotest.failf "%s: expected service error" label)
    cases

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

let test_bucket_list_rejects_malformed_creation_date () =
  let body =
    {|<ListAllMyBucketsResult><Buckets><Bucket><Name>alpha</Name><CreationDate>not-a-time</CreationDate></Bucket></Buckets></ListAllMyBucketsResult>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match Recording_s3.Bucket.list conn with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions creation date" true
        (string_contains text ~substring:"CreationDate")
  | Error error ->
      Alcotest.failf "unexpected list decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed CreationDate decode error"

let test_bucket_create_accepts_typed_region () =
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let options =
    { Bucket.Create.region = Some (Awskit.Region.of_string_exn "eu-west-1") }
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

let test_bucket_fundamental_wire () =
  let list_body =
    "<ListAllMyBucketsResult><Buckets><Bucket><Name>new-bucket</Name></Bucket></Buckets></ListAllMyBucketsResult>"
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 "";
        response 200 "";
        response 204 "";
        response 200 "<LocationConstraint>eu-west-1</LocationConstraint>";
        response 200 list_body;
      ]
  in
  ignore
    (Recording_s3.Bucket.create conn ~bucket:(bucket_name "new-bucket") ()
    |> ok_or_fail "create default bucket");
  let create_options =
    Bucket.Create.options_exn
      ~region:(Awskit.Region.of_string_exn "eu-west-1")
      ()
  in
  ignore
    (Recording_s3.Bucket.create conn ~bucket:(bucket_name "new-bucket")
       ~options:create_options ()
    |> ok_or_fail "create regional bucket");
  let delete_options =
    Bucket.Delete.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Bucket.delete conn ~bucket:(bucket_name "new-bucket")
       ~options:delete_options ()
    |> ok_or_fail "delete bucket");
  let location_options =
    Bucket.Get_location.options_exn ~expected_bucket_owner:expected_owner ()
  in
  let location =
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "new-bucket")
      ~options:location_options ()
    |> ok_or_fail "get bucket location"
  in
  Alcotest.(check string)
    "location region" "eu-west-1"
    (Awskit.Region.to_string location.region);
  ignore (Recording_s3.Bucket.list conn |> ok_or_fail "list buckets");
  match List.rev conn.calls with
  | [ create_default; create_regional; delete; location; list ] ->
      List.iter
        (fun (label, expected_method, (call : Recording_runtime.call)) ->
          check_method (label ^ " method") expected_method call.request;
          Alcotest.(check string) (label ^ " path") "/" call.request.target.path)
        [
          ("create default", "PUT", create_default);
          ("create regional", "PUT", create_regional);
          ("delete", "DELETE", delete);
          ("location", "GET", location);
          ("list", "GET", list);
        ];
      check_query "create default query" [] create_default;
      check_query "create regional query" [] create_regional;
      check_query "delete query" [] delete;
      check_query "location query" [ ("location", []) ] location;
      check_query "list query" [] list;
      Alcotest.(check string) "create default body" "" create_default.body;
      Alcotest.(check (option string))
        "create default content type" None
        (header "content-type" create_default.request.headers);
      Alcotest.(check bool)
        "create regional location constraint" true
        (string_contains create_regional.body
           ~substring:"<LocationConstraint>eu-west-1</LocationConstraint>");
      Alcotest.(check (option string))
        "create regional content type" (Some "application/xml")
        (header "content-type" create_regional.request.headers);
      check_expected_owner_header "delete expected owner" delete;
      check_expected_owner_header "location expected owner" location;
      Alcotest.(check string)
        "list host" "s3.us-east-1.amazonaws.com" list.request.target.host
  | _ -> Alcotest.fail "expected bucket fundamental calls"

let test_bucket_location_default_and_legacy_region () =
  let conn =
    Recording_runtime.connect
      [
        response 200 "<LocationConstraint></LocationConstraint>";
        response 200 "<LocationConstraint>EU</LocationConstraint>";
      ]
  in
  let default_region =
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "default location"
  in
  Alcotest.(check string)
    "default region" "us-east-1"
    (Awskit.Region.to_string default_region.region);
  let eu =
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "legacy EU location"
  in
  Alcotest.(check string)
    "legacy EU region" "eu-west-1"
    (Awskit.Region.to_string eu.region)

let test_bucket_location_rejects_invalid_region () =
  let conn =
    Recording_runtime.connect
      [ response 200 "<LocationConstraint>us&#x0A;west</LocationConstraint>" ]
  in
  match
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions location constraint" true
        (string_contains text ~substring:"LocationConstraint")
  | Error error -> Alcotest.failf "unexpected location error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid location decode error"

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
  check_method "head expected owner method" "HEAD"
    (Recording_runtime.last_call conn).request;
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
        Alcotest.test_case "bucket exists uses head" `Quick
          test_bucket_exists_uses_head;
        Alcotest.test_case "bucket head errors preserve region hints" `Quick
          test_bucket_head_error_preserves_region_hints;
        Alcotest.test_case "bucket head rejects malformed region hint" `Quick
          test_bucket_head_rejects_malformed_region_hint;
        Alcotest.test_case "service error extracts body fields" `Quick
          test_service_error_extracts_body_fields;
        Alcotest.test_case "bucket list parse" `Quick test_bucket_list_parse;
        Alcotest.test_case "bucket list rejects invalid bucket names" `Quick
          test_bucket_list_rejects_invalid_bucket_names;
        Alcotest.test_case "bucket list rejects malformed creation date" `Quick
          test_bucket_list_rejects_malformed_creation_date;
        Alcotest.test_case "bucket create accepts typed region" `Quick
          test_bucket_create_accepts_typed_region;
        Alcotest.test_case "bucket fundamental wire" `Quick
          test_bucket_fundamental_wire;
        Alcotest.test_case "bucket location default and legacy region" `Quick
          test_bucket_location_default_and_legacy_region;
        Alcotest.test_case "bucket location rejects invalid region" `Quick
          test_bucket_location_rejects_invalid_region;
        Alcotest.test_case "bucket expected owner headers" `Quick
          test_bucket_expected_owner_headers;
      ] );
  ]
