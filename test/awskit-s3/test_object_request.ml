open Awskit_s3
open Awskit_s3_test

let test_object_checksum_headers_and_response () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("etag", "\"etag\"");
              ("x-amz-checksum-sha1", "provided-sha1");
              ("x-amz-checksum-sha256", "provided-sha256");
              ("x-amz-checksum-type", "COMPOSITE");
            ]
          "";
      ]
  in
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
      value = "provided-sha256";
    }
  in
  let options =
    {
      Put_object.default_options with
      checksum = Some checksum;
      expected_bucket_owner = Some "123456789012";
    }
  in
  let put =
    Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file" ~options
      ~body:(Recording_s3.Body.of_string "hello")
      ()
    |> ok_or_fail "put checksum"
  in
  check_checksum "put response sha1" Object.Checksum.Algorithm.Sha1
    "provided-sha1" put.checksum;
  check_checksum "put response sha256" Object.Checksum.Algorithm.Sha256
    "provided-sha256" put.checksum;
  Alcotest.(check bool)
    "checksum type" true
    (put.checksum.checksum_type = Some Object.Checksum.Type.Composite);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string) "body" "hello" call.body;
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" call.request.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha256")
    (header "x-amz-checksum-sha256" call.request.headers);
  Alcotest.(check (option string))
    "expected owner header" (Some "123456789012")
    (header "x-amz-expected-bucket-owner" call.request.headers)

let test_object_precondition_headers () =
  let time = Ptime.to_rfc3339 test_time in
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("etag", "\"put\"") ] "";
        response 200
          ~headers:[ ("etag", "\"get\""); ("content-length", "0") ]
          "";
        response 200
          ~headers:[ ("etag", "\"head\""); ("content-length", "0") ]
          "";
        response 204 "";
        response 200
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
      ]
  in
  let etag = Object.Etag.of_string_exn "\"etag\"" in
  let write_preconditions =
    {
      Object.Preconditions.Write.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
    }
  in
  let put_options =
    { Put_object.default_options with preconditions = write_preconditions }
  in
  ignore
    (Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
       ~options:put_options
       ~body:(Recording_s3.Body.of_string "body")
       ()
    |> ok_or_fail "put preconditions");
  let read_preconditions =
    {
      Object.Preconditions.Read.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
      if_modified_since = Some test_time;
      if_unmodified_since = Some test_time;
    }
  in
  let get_options =
    { Get_object.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.get conn ~bucket:"my-bucket" ~key:"file"
       ~options:get_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get preconditions");
  let head_options =
    { Head_object.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:"my-bucket" ~key:"file"
       ~options:head_options ()
    |> ok_or_fail "head preconditions");
  let delete_preconditions =
    {
      Object.Preconditions.Delete.if_match =
        Some (Object.Etag_condition.etag etag);
    }
  in
  let delete_options =
    { Delete_object.default_options with preconditions = delete_preconditions }
  in
  ignore
    (Recording_s3.Object.delete conn ~bucket:"my-bucket" ~key:"file"
       ~options:delete_options ()
    |> ok_or_fail "delete preconditions");
  let copy_preconditions =
    {
      Object.Preconditions.Copy_source.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
      if_modified_since = Some test_time;
      if_unmodified_since = Some test_time;
    }
  in
  let copy_options =
    {
      Copy_object.default_options with
      source_preconditions = copy_preconditions;
    }
  in
  ignore
    (Recording_s3.Object.copy conn ~source_bucket:"my-bucket" ~source_key:"file"
       ~destination_bucket:"my-bucket" ~destination_key:"copy"
       ~options:copy_options ()
    |> ok_or_fail "copy preconditions");
  match List.rev conn.calls with
  | [ put; get; head; delete; copy ] ->
      Alcotest.(check (option string))
        "put if-match" (Some "\"etag\"")
        (header "if-match" put.request.headers);
      Alcotest.(check (option string))
        "put if-none-match" (Some "*")
        (header "if-none-match" put.request.headers);
      List.iter
        (fun (call : Recording_runtime.call) ->
          Alcotest.(check (option string))
            "read if-match" (Some "\"etag\"")
            (header "if-match" call.request.headers);
          Alcotest.(check (option string))
            "read if-none-match" (Some "*")
            (header "if-none-match" call.request.headers);
          Alcotest.(check (option string))
            "read if-modified-since" (Some time)
            (header "if-modified-since" call.request.headers);
          Alcotest.(check (option string))
            "read if-unmodified-since" (Some time)
            (header "if-unmodified-since" call.request.headers))
        [ get; head ];
      Alcotest.(check (option string))
        "delete if-match" (Some "\"etag\"")
        (header "if-match" delete.request.headers);
      Alcotest.(check (option string))
        "delete last modified removed" None
        (header "x-amz-if-match-last-modified-time" delete.request.headers);
      Alcotest.(check (option string))
        "delete size removed" None
        (header "x-amz-if-match-size" delete.request.headers);
      Alcotest.(check (option string))
        "copy source if-match" (Some "\"etag\"")
        (header "x-amz-copy-source-if-match" copy.request.headers);
      Alcotest.(check (option string))
        "copy source if-none-match" (Some "*")
        (header "x-amz-copy-source-if-none-match" copy.request.headers);
      Alcotest.(check (option string))
        "copy source modified since" (Some time)
        (header "x-amz-copy-source-if-modified-since" copy.request.headers);
      Alcotest.(check (option string))
        "copy source unmodified since" (Some time)
        (header "x-amz-copy-source-if-unmodified-since" copy.request.headers)
  | _ -> Alcotest.fail "expected five recorded calls"

let test_delete_objects_request_body () =
  let conn = Recording_runtime.connect [ response 200 "<DeleteResult/>" ] in
  let version_id = Object.Version_id.of_string_exn "version-1" in
  let etag = Object.Etag.of_string_exn "\"etag\"" in
  let objects =
    [
      { Delete_objects.key = "key-only.txt"; version_id = None; etag = None };
      {
        Delete_objects.key = "versioned.txt";
        version_id = Some version_id;
        etag = None;
      };
      { Delete_objects.key = "etag.txt"; version_id = None; etag = Some etag };
    ]
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:"my-bucket" ~objects ()
    |> ok_or_fail "delete objects request body");
  let body = (Recording_runtime.last_call conn).body in
  let check_contains label substring =
    Alcotest.(check bool) label true (string_contains ~substring body)
  in
  let check_absent label substring =
    Alcotest.(check bool) label false (string_contains ~substring body)
  in
  check_contains "key-only key" "<Key>key-only.txt</Key>";
  check_contains "versioned key" "<Key>versioned.txt</Key>";
  check_contains "version id" "<VersionId>version-1</VersionId>";
  check_contains "etag key" "<Key>etag.txt</Key>";
  check_contains "etag" "<ETag>&quot;etag&quot;</ETag>";
  check_absent "last modified omitted" "LastModifiedTime";
  check_absent "size omitted" "<Size>"

let test_object_versioning_requests_and_parse () =
  let version_id = Object.Version_id.of_string_exn "version-1" in
  let next_version_id = Object.Version_id.of_string_exn "version-2" in
  let versions_body =
    {|<ListVersionsResult><Name>my-bucket</Name><Prefix>logs/</Prefix><KeyMarker>logs/a.txt</KeyMarker><VersionIdMarker>version-1</VersionIdMarker><NextKeyMarker>logs/b.txt</NextKeyMarker><NextVersionIdMarker>version-2</NextVersionIdMarker><IsTruncated>true</IsTruncated><Version><Key>logs/a.txt</Key><VersionId>version-1</VersionId><IsLatest>false</IsLatest><LastModified>2026-04-08T12:00:00Z</LastModified><ETag>"etag"</ETag><Size>3</Size><StorageClass>STANDARD</StorageClass></Version><DeleteMarker><Key>logs/a.txt</Key><VersionId>marker-1</VersionId><IsLatest>true</IsLatest><LastModified>2026-04-08T12:00:00Z</LastModified></DeleteMarker></ListVersionsResult>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("x-amz-version-id", "copy-version");
              ("x-amz-copy-source-version-id", "version-1");
            ]
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
        response 200 versions_body;
      ]
  in
  let copy_options =
    { Copy_object.default_options with source_version_id = Some version_id }
  in
  let copy =
    Recording_s3.Object.copy conn ~source_bucket:"my-bucket" ~source_key:"file"
      ~destination_bucket:"my-bucket" ~destination_key:"copy"
      ~options:copy_options ()
    |> ok_or_fail "copy source version"
  in
  Alcotest.(check (option string))
    "copy result source version" (Some "version-1")
    (version_string copy.copy_source_version_id);
  let list_options =
    {
      List_object_versions.default_options with
      prefix = Some "logs/";
      max_keys = Some 10;
      key_marker = Some "logs/a.txt";
      version_id_marker = Some version_id;
    }
  in
  let page =
    Recording_s3.Object.list_versions conn ~bucket:"my-bucket"
      ~options:list_options ()
    |> ok_or_fail "list versions"
  in
  Alcotest.(check bool) "versions truncated" true page.is_truncated;
  Alcotest.(check int) "version count" 1 (List.length page.versions);
  Alcotest.(check int) "delete marker count" 1 (List.length page.delete_markers);
  Alcotest.(check (option string))
    "next key marker" (Some "logs/b.txt") page.next_key_marker;
  Alcotest.(check (option string))
    "next version marker"
    (Some (Object.Version_id.to_string next_version_id))
    (version_string page.next_version_id_marker);
  match List.rev conn.calls with
  | [ copy_call; versions_call ] ->
      Alcotest.(check (option string))
        "copy source header" (Some "/my-bucket/file?versionId=version-1")
        (header "x-amz-copy-source" copy_call.request.headers);
      Alcotest.(check (option string))
        "copy tagging header omitted" None
        (header "x-amz-tagging" copy_call.request.headers);
      Alcotest.(check (option (list string)))
        "versions query" (Some [])
        (List.assoc_opt "versions" versions_call.request.target.query);
      Alcotest.(check (option (list string)))
        "prefix query" (Some [ "logs/" ])
        (List.assoc_opt "prefix" versions_call.request.target.query);
      Alcotest.(check (option (list string)))
        "key marker query" (Some [ "logs/a.txt" ])
        (List.assoc_opt "key-marker" versions_call.request.target.query);
      Alcotest.(check (option (list string)))
        "version marker query" (Some [ "version-1" ])
        (List.assoc_opt "version-id-marker" versions_call.request.target.query)
  | _ -> Alcotest.fail "expected copy and version listing calls"

let test_malformed_xml_responses () =
  let conn =
    Recording_runtime.connect
      [
        response 200 "<ListAllMyBucketsResult><Buckets>";
        response 200 "<ListBucketResult><Contents>";
        response 200 "<ListVersionsResult><Version>";
        response 200 "<ListPartsResult><Part>";
      ]
  in
  (match Recording_s3.Bucket.list conn with
  | Error (Awskit.Error.Decode _) -> ()
  | Error error ->
      Alcotest.failf "unexpected bucket decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected bucket list decode error");
  (match Recording_s3.Object.list conn ~bucket:"my-bucket" () with
  | Error (Awskit.Error.Decode _) -> ()
  | Error error ->
      Alcotest.failf "unexpected object decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected object list decode error");
  (match Recording_s3.Object.list_versions conn ~bucket:"my-bucket" () with
  | Error (Awskit.Error.Decode _) -> ()
  | Error error ->
      Alcotest.failf "unexpected version decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected version list decode error");
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  match
    Recording_s3.Multipart.list_parts conn ~bucket:"my-bucket" ~key:"large.bin"
      ~upload_id ()
  with
  | Error (Awskit.Error.Decode _) -> ()
  | Error error ->
      Alcotest.failf "unexpected multipart decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected list parts decode error"

let test_copy_object_embedded_error () =
  let body =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.copy conn ~source_bucket:"my-bucket" ~source_key:"file"
      ~destination_bucket:"my-bucket" ~destination_key:"copy" ()
  with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected copy error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded copy error"

let test_object_checksum_mode_and_expected_owner_headers () =
  let expected_owner = "123456789012" in
  let copy_body =
    {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("content-length", "5") ] "hello";
        response 200 ~headers:[ ("content-length", "0") ] "";
        response 204 "";
        response 200 "<DeleteResult/>";
        response 200 copy_body;
        response 200 (list_page ~truncated:false [ "a.txt" ]);
        response 200
          {|<ListVersionsResult><IsTruncated>false</IsTruncated></ListVersionsResult>|};
      ]
  in
  let read_options =
    {
      Get_object.default_options with
      checksum_mode = Some Object.Checksum.Mode.Enabled;
      expected_bucket_owner = Some expected_owner;
    }
  in
  ignore
    (Recording_s3.Object.get conn ~bucket:"my-bucket" ~key:"file"
       ~options:read_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get checksum mode");
  let head_options =
    {
      Head_object.default_options with
      checksum_mode = Some Object.Checksum.Mode.Enabled;
      expected_bucket_owner = Some expected_owner;
    }
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:"my-bucket" ~key:"file"
       ~options:head_options ()
    |> ok_or_fail "head checksum mode");
  let delete_options =
    {
      Delete_object.default_options with
      expected_bucket_owner = Some expected_owner;
    }
  in
  ignore
    (Recording_s3.Object.delete conn ~bucket:"my-bucket" ~key:"file"
       ~options:delete_options ()
    |> ok_or_fail "delete expected owner");
  let delete_many_options =
    { Delete_objects.expected_bucket_owner = Some expected_owner }
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:"my-bucket"
       ~objects:
         [ { Delete_objects.key = "file"; version_id = None; etag = None } ]
       ~options:delete_many_options ()
    |> ok_or_fail "delete many expected owner");
  let copy_options =
    {
      Copy_object.default_options with
      checksum_algorithm = Some Object.Checksum.Algorithm.Sha256;
      expected_bucket_owner = Some expected_owner;
      source_expected_bucket_owner = Some "210987654321";
    }
  in
  ignore
    (Recording_s3.Object.copy conn ~source_bucket:"source" ~source_key:"file"
       ~destination_bucket:"my-bucket" ~destination_key:"copy"
       ~options:copy_options ()
    |> ok_or_fail "copy expected owner");
  let list_options =
    {
      List_objects_v2.default_options with
      expected_bucket_owner = Some expected_owner;
    }
  in
  ignore
    (Recording_s3.Object.list conn ~bucket:"my-bucket" ~options:list_options ()
    |> ok_or_fail "list expected owner");
  let version_options =
    {
      List_object_versions.default_options with
      expected_bucket_owner = Some expected_owner;
    }
  in
  ignore
    (Recording_s3.Object.list_versions conn ~bucket:"my-bucket"
       ~options:version_options ()
    |> ok_or_fail "list versions expected owner");
  match List.rev conn.calls with
  | [ get; head; delete; delete_many; copy; list; versions ] ->
      List.iter
        (fun (label, (call : Recording_runtime.call)) ->
          Alcotest.(check (option string))
            (label ^ " expected owner")
            (Some expected_owner)
            (header "x-amz-expected-bucket-owner" call.request.headers))
        [
          ("get", get);
          ("head", head);
          ("delete", delete);
          ("delete many", delete_many);
          ("copy", copy);
          ("list", list);
          ("versions", versions);
        ];
      List.iter
        (fun (label, (call : Recording_runtime.call)) ->
          Alcotest.(check (option string))
            (label ^ " checksum mode") (Some "ENABLED")
            (header "x-amz-checksum-mode" call.request.headers))
        [ ("get", get); ("head", head) ];
      Alcotest.(check (option string))
        "copy checksum algorithm" (Some "SHA256")
        (header "x-amz-checksum-algorithm" copy.request.headers);
      Alcotest.(check (option string))
        "copy source expected owner" (Some "210987654321")
        (header "x-amz-source-expected-bucket-owner" copy.request.headers)
  | _ -> Alcotest.fail "expected seven object calls"

let suite =
  [
    ( "object request",
      [
        Alcotest.test_case "object checksum headers and response" `Quick
          test_object_checksum_headers_and_response;
        Alcotest.test_case "object precondition headers" `Quick
          test_object_precondition_headers;
        Alcotest.test_case "delete objects request body" `Quick
          test_delete_objects_request_body;
        Alcotest.test_case "object versioning requests and parse" `Quick
          test_object_versioning_requests_and_parse;
        Alcotest.test_case "malformed xml responses" `Quick
          test_malformed_xml_responses;
        Alcotest.test_case "copy object embedded error" `Quick
          test_copy_object_embedded_error;
        Alcotest.test_case "object checksum mode and expected owner headers"
          `Quick test_object_checksum_mode_and_expected_owner_headers;
      ] );
  ]
