open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  let open Awskit.Error in
  match kind error with Decode _ -> true | _ -> false

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let service_error ?code ?message status =
  Awskit.Error.Internal.service ~status ?code ?message ~headers:[] ()

let no_such_key_body =
  {|<Error><Code>NoSuchKey</Code><Message>not found</Message></Error>|}

let no_such_bucket_body =
  {|<Error><Code>NoSuchBucket</Code><Message>bucket not found</Message></Error>|}

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
      expected_bucket_owner = Some (account_id "123456789012");
    }
  in
  let put =
    Recording_s3.Object.put conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ~options
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
    (Recording_s3.Object.put conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:put_options
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
    (Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:get_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get preconditions");
  let head_options =
    { Head_object.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:head_options ()
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
    (Recording_s3.Object.delete conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:delete_options ()
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
    (Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
       ~source_key:(object_key "file")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ~options:copy_options ()
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
      Delete_objects.object_ ~key:(object_key "key-only.txt") ();
      Delete_objects.object_ ~key:(object_key "versioned.txt") ~version_id ();
      Delete_objects.object_ ~key:(object_key "etag.txt") ~etag ();
    ]
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
       ~objects ()
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

let test_delete_objects_rejects_invalid_count () =
  let conn = Recording_runtime.connect [] in
  let object_ = Delete_objects.object_ ~key:(object_key "file") () in
  let result =
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects:[] ()
  in
  expect_validation "delete objects empty" result;
  let objects = List.init (Delete_objects.max_objects + 1) (fun _ -> object_) in
  let result =
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects ()
  in
  expect_validation "delete objects too many" result

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
    Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
      ~source_key:(object_key "file")
      ~destination_bucket:(bucket_name "my-bucket")
      ~destination_key:(object_key "copy") ~options:copy_options ()
    |> ok_or_fail "copy source version"
  in
  Alcotest.(check (option string))
    "copy result source version" (Some "version-1")
    (version_string copy.copy_source_version_id);
  let list_options =
    List_object_versions.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~max_keys:10 ~key_marker:(object_key "logs/a.txt")
      ~version_id_marker:version_id ()
  in
  let page =
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket")
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

let test_object_versioning_empty_markers_are_absent () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          {|<ListVersionsResult><Name>my-bucket</Name><KeyMarker></KeyMarker><VersionIdMarker></VersionIdMarker><NextKeyMarker></NextKeyMarker><NextVersionIdMarker></NextVersionIdMarker><IsTruncated>false</IsTruncated></ListVersionsResult>|};
      ]
  in
  let page =
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "list versions"
  in
  Alcotest.(check (option string)) "key marker" None page.key_marker;
  Alcotest.(check (option string))
    "version marker" None
    (version_string page.version_id_marker);
  Alcotest.(check (option string)) "next key marker" None page.next_key_marker;
  Alcotest.(check (option string))
    "next version marker" None
    (version_string page.next_version_id_marker)

let test_version_paginator_rejects_invalid_next_key_marker () =
  let invalid_marker = String.make 1025 'a' in
  let body =
    Fmt.str
      "<ListVersionsResult><IsTruncated>true</IsTruncated><NextKeyMarker>%s</NextKeyMarker></ListVersionsResult>"
      invalid_marker
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.List_object_versions.pages conn
      ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_validation_field "key" error ->
      Alcotest.(check int)
        "stops before second request" 1 (List.length conn.calls)
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid next key marker"

let test_find_metadata_missing_object_returns_none () =
  let conn = Recording_runtime.connect [ response 404 no_such_key_body ] in
  match
    Recording_s3.Object.find_metadata conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "missing") ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected None for missing object"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error

let test_find_metadata_bare_head_404_returns_none () =
  let conn = Recording_runtime.connect [ response 404 "" ] in
  match
    Recording_s3.Object.find_metadata conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "missing") ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected None for bare HEAD 404"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error

let test_find_metadata_missing_bucket_returns_error () =
  let conn = Recording_runtime.connect [ response 404 no_such_bucket_body ] in
  match
    Recording_s3.Object.find_metadata conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when Error.is_no_such_bucket error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok None -> Alcotest.fail "expected missing bucket error, got None"
  | Ok (Some _) -> Alcotest.fail "expected missing bucket error"

let test_find_success_returns_some () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "5") ]
          "hello";
      ]
  in
  match
    Recording_s3.Object.find conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file")
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Ok (Some result) ->
      let body = result.Get_object.value in
      Alcotest.(check string) "body" "hello" body;
      Alcotest.(check (option string))
        "etag" (Some "\"etag\"")
        (Option.map Object.Etag.to_string result.etag)
  | Ok None -> Alcotest.fail "expected present object"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error

let test_find_missing_object_returns_none () =
  let conn = Recording_runtime.connect [ response 404 no_such_key_body ] in
  match
    Recording_s3.Object.find conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "missing")
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected None for missing object"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error

let test_find_missing_bucket_returns_error () =
  let conn = Recording_runtime.connect [ response 404 no_such_bucket_body ] in
  match
    Recording_s3.Object.find conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file")
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Error error when Error.is_no_such_bucket error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok None -> Alcotest.fail "expected missing bucket error, got None"
  | Ok (Some _) -> Alcotest.fail "expected missing bucket error"

let test_find_preserves_consumer_not_found_error () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "4") ]
          "body";
      ]
  in
  let consumer_error =
    service_error ~code:"NoSuchKey" ~message:"consumer-owned missing resource"
      404
  in
  match
    Recording_s3.Object.find conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file")
      ~consume:(fun _reader -> Error consumer_error)
      ()
  with
  | Error error when Error.is_not_found error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok None -> Alcotest.fail "expected consumer error, got None"
  | Ok (Some _) -> Alcotest.fail "expected consumer error"

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
  let check_context label error ~operation ?resource () =
    let text = Awskit.Error.to_string_hum error in
    Alcotest.(check bool)
      (label ^ " operation") true
      (string_contains text ~substring:operation);
    match resource with
    | None -> ()
    | Some resource ->
        Alcotest.(check bool)
          (label ^ " resource") true
          (string_contains text ~substring:resource)
  in
  (match Recording_s3.Bucket.list conn with
  | Error error when is_decode_error error ->
      check_context "bucket list" error ~operation:"ListBuckets" ()
  | Error error ->
      Alcotest.failf "unexpected bucket decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected bucket list decode error");
  (match Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") () with
  | Error error when is_decode_error error ->
      check_context "object list" error ~operation:"ListObjectsV2"
        ~resource:"s3://my-bucket" ()
  | Error error ->
      Alcotest.failf "unexpected object decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected object list decode error");
  (match
     Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
   with
  | Error error when is_decode_error error ->
      check_context "version list" error ~operation:"ListObjectVersions"
        ~resource:"s3://my-bucket" ()
  | Error error ->
      Alcotest.failf "unexpected version decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected version list decode error");
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  match
    Recording_s3.Multipart.list_parts conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id ()
  with
  | Error error when is_decode_error error ->
      check_context "multipart list" error ~operation:"ListParts"
        ~resource:"s3://my-bucket/large.bin" ()
  | Error error ->
      Alcotest.failf "unexpected multipart decode error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected list parts decode error"

let test_object_list_rejects_malformed_known_fields () =
  let bad_size =
    "<ListBucketResult><Contents><Key>a.txt</Key><Size>not-int</Size></Contents></ListBucketResult>"
  in
  let conn = Recording_runtime.connect [ response 200 bad_size ] in
  match Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") () with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions size" true
        (string_contains text ~substring:"Size")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed Size decode error"

let test_object_list_rejects_negative_numeric_fields () =
  let cases =
    [
      ( "Size",
        "<ListBucketResult><Contents><Key>a.txt</Key><Size>-1</Size></Contents></ListBucketResult>"
      );
      ( "KeyCount",
        "<ListBucketResult><KeyCount>-1</KeyCount></ListBucketResult>" );
    ]
  in
  List.iter
    (fun (field, body) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match
        Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            ("mentions " ^ field) true
            (string_contains text ~substring:field)
      | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
      | Ok _ -> Alcotest.failf "expected negative %s decode error" field)
    cases

let test_object_versions_rejects_malformed_known_fields () =
  let body =
    "<ListVersionsResult><Version><Key>a.txt</Key><IsLatest>maybe</IsLatest></Version></ListVersionsResult>"
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions IsLatest" true
        (string_contains text ~substring:"IsLatest")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed IsLatest decode error"

let test_object_list_allows_unknown_extra_elements () =
  let body =
    "<ListBucketResult><FutureField>ok</FutureField><Contents><Key>a.txt</Key><Size>1</Size><FutureObjectField>ok</FutureObjectField></Contents></ListBucketResult>"
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let result =
    Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "list with unknown fields"
  in
  Alcotest.(check int) "object count" 1 (List.length result.objects)

let test_object_tagging_rejects_incomplete_tag_xml () =
  let body = "<Tagging><TagSet><Tag><Key>env</Key></Tag></TagSet></Tagging>" in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.Tagging.get conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions Value" true
        (string_contains text ~substring:"Value")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected incomplete tag decode error"

let test_head_rejects_duplicate_metadata_headers_as_decode_error () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("content-length", "0");
              ("x-amz-meta-source", "one");
              ("X-Amz-Meta-Source", "two");
            ]
          "";
      ]
  in
  match
    Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions metadata headers" true
        (string_contains text ~substring:"S3 metadata headers")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected duplicate metadata decode error"

let test_copy_object_embedded_error () =
  let body =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
      ~source_key:(object_key "file")
      ~destination_bucket:(bucket_name "my-bucket")
      ~destination_key:(object_key "copy") ()
  with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected copy error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded copy error"

let test_object_checksum_mode_and_expected_owner_headers () =
  let expected_owner = account_id "123456789012" in
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
    Get_object.options_exn ~checksum_mode:Object.Checksum.Mode.Enabled
      ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:read_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get checksum mode");
  let head_options =
    Head_object.options_exn ~checksum_mode:Object.Checksum.Mode.Enabled
      ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:head_options ()
    |> ok_or_fail "head checksum mode");
  let delete_options =
    Delete_object.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.delete conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:delete_options ()
    |> ok_or_fail "delete expected owner");
  let delete_many_options =
    Delete_objects.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
       ~objects:[ Delete_objects.object_ ~key:(object_key "file") () ]
       ~options:delete_many_options ()
    |> ok_or_fail "delete many expected owner");
  let copy_options =
    Copy_object.options_exn ~checksum_algorithm:Object.Checksum.Algorithm.Sha256
      ~expected_bucket_owner:expected_owner
      ~source_expected_bucket_owner:(account_id "210987654321")
      ()
  in
  ignore
    (Recording_s3.Object.copy conn ~source_bucket:(bucket_name "source")
       ~source_key:(object_key "file")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ~options:copy_options ()
    |> ok_or_fail "copy expected owner");
  let list_options =
    List_objects_v2.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket")
       ~options:list_options ()
    |> ok_or_fail "list expected owner");
  let version_options =
    List_object_versions.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket")
       ~options:version_options ()
    |> ok_or_fail "list versions expected owner");
  match List.rev conn.calls with
  | [ get; head; delete; delete_many; copy; list; versions ] ->
      List.iter
        (fun (label, (call : Recording_runtime.call)) ->
          Alcotest.(check (option string))
            (label ^ " expected owner")
            (Some (Account_id.to_string expected_owner))
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
        Alcotest.test_case "delete objects rejects invalid count" `Quick
          test_delete_objects_rejects_invalid_count;
        Alcotest.test_case "object versioning requests and parse" `Quick
          test_object_versioning_requests_and_parse;
        Alcotest.test_case "object versioning empty markers are absent" `Quick
          test_object_versioning_empty_markers_are_absent;
        Alcotest.test_case "version paginator rejects invalid next key marker"
          `Quick test_version_paginator_rejects_invalid_next_key_marker;
        Alcotest.test_case "find metadata missing object returns none" `Quick
          test_find_metadata_missing_object_returns_none;
        Alcotest.test_case "find metadata bare head 404 returns none" `Quick
          test_find_metadata_bare_head_404_returns_none;
        Alcotest.test_case "find metadata missing bucket returns error" `Quick
          test_find_metadata_missing_bucket_returns_error;
        Alcotest.test_case "find success returns some" `Quick
          test_find_success_returns_some;
        Alcotest.test_case "find missing object returns none" `Quick
          test_find_missing_object_returns_none;
        Alcotest.test_case "find missing bucket returns error" `Quick
          test_find_missing_bucket_returns_error;
        Alcotest.test_case "find preserves consumer not found error" `Quick
          test_find_preserves_consumer_not_found_error;
        Alcotest.test_case "malformed xml responses" `Quick
          test_malformed_xml_responses;
        Alcotest.test_case "object list rejects malformed known fields" `Quick
          test_object_list_rejects_malformed_known_fields;
        Alcotest.test_case "object list rejects negative numeric fields" `Quick
          test_object_list_rejects_negative_numeric_fields;
        Alcotest.test_case "object versions rejects malformed known fields"
          `Quick test_object_versions_rejects_malformed_known_fields;
        Alcotest.test_case "object list allows unknown extra elements" `Quick
          test_object_list_allows_unknown_extra_elements;
        Alcotest.test_case "object tagging rejects incomplete tag xml" `Quick
          test_object_tagging_rejects_incomplete_tag_xml;
        Alcotest.test_case
          "head rejects duplicate metadata headers as decode error" `Quick
          test_head_rejects_duplicate_metadata_headers_as_decode_error;
        Alcotest.test_case "copy object embedded error" `Quick
          test_copy_object_embedded_error;
        Alcotest.test_case "object checksum mode and expected owner headers"
          `Quick test_object_checksum_mode_and_expected_owner_headers;
      ] );
  ]
