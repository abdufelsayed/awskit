open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  let open Awskit.Error in
  match kind error with Decode _ -> true | _ -> false

let body_details error =
  let open Awskit.Error in
  match kind error with Body body -> Some body | _ -> None

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let expect_validation_field label field = function
  | Error error when is_validation_field field error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected validation error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let service_error ?code ?message status =
  Awskit.Error.Producer.service ~status ?code ?message ~headers:[] ()

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
      Object.Put.default_options with
      checksum = Some checksum;
      content_type = Some (content_type "text/plain");
      metadata = Metadata.of_list_exn [ ("source", "unit-test") ];
      tags = tag_set [ tag "env name" "prod+stage"; tag "path/key" "x@y" ];
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
  check_method "put method" "PUT" call.request;
  Alcotest.(check string) "body" "hello" call.body;
  Alcotest.(check (option string))
    "content length" (Some "5")
    (header "content-length" call.request.headers);
  Alcotest.(check (option string))
    "content type" (Some "text/plain")
    (header "content-type" call.request.headers);
  Alcotest.(check (option string))
    "metadata" (Some "unit-test")
    (header "x-amz-meta-source" call.request.headers);
  Alcotest.(check (option string))
    "tags" (Some "env%20name=prod%2Bstage&path%2Fkey=x%40y")
    (header "x-amz-tagging" call.request.headers);
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
  let time = "Wed, 08 Apr 2026 12:00:00 GMT" in
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
    { Object.Put.default_options with preconditions = write_preconditions }
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
    { Object.Get.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:get_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get preconditions");
  let head_options =
    { Object.Head.default_options with preconditions = read_preconditions }
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
    { Object.Delete.default_options with preconditions = delete_preconditions }
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
      Object.Copy.default_options with
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
      check_method "put method" "PUT" put.request;
      check_method "get method" "GET" get.request;
      check_method "head method" "HEAD" head.request;
      check_method "delete method" "DELETE" delete.request;
      check_method "copy method" "PUT" copy.request;
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

let test_object_get_range_header () =
  let conn =
    Recording_runtime.connect
      [
        response 206
          ~headers:
            [ ("content-length", "4"); ("content-range", "bytes 2-5/10") ]
          "cdef";
      ]
  in
  let options =
    Object.Get.options_exn ~range:(Range.bytes_exn ~start:2L ~finish:5L) ()
  in
  let result =
    Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ~options
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
    |> ok_or_fail "get range"
  in
  Alcotest.(check string) "range body" "cdef" result.value;
  (match result.content_range with
  | None -> Alcotest.fail "expected parsed content range"
  | Some content_range ->
      Alcotest.(check int64) "content range start" 2L content_range.start;
      Alcotest.(check int64) "content range finish" 5L content_range.finish;
      Alcotest.(check (option int64))
        "content range complete length" (Some 10L) content_range.complete_length);
  let call = Recording_runtime.last_call conn in
  check_method "range method" "GET" call.request;
  let range_headers =
    List.filter
      (fun (name, _) -> String.lowercase_ascii name = "range")
      call.request.headers
  in
  Alcotest.(check (list (pair string string)))
    "single range header"
    [ ("range", "bytes=2-5") ]
    range_headers

let test_object_string_conveniences_share_operation_model () =
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("etag", "\"put\"") ] "";
        response 200
          ~headers:[ ("etag", "\"get\""); ("content-length", "5") ]
          "hello";
      ]
  in
  let put_options =
    Object.Put.options_exn ~content_type:(content_type "text/plain") ()
  in
  let put =
    Recording_s3.Object.put_string conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file.txt") ~options:put_options ~contents:"hello" ()
    |> ok_or_fail "put string"
  in
  Alcotest.(check (option string))
    "put etag" (Some "\"put\"")
    (Option.map Object.Etag.to_string put.etag);
  let get_options =
    Object.Get.options_exn ~expected_bucket_owner:(account_id "123456789012") ()
  in
  let get =
    Recording_s3.Object.get_string conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file.txt") ~options:get_options ~max_bytes:16L ()
    |> ok_or_fail "get string"
  in
  Alcotest.(check string) "get value" "hello" get.value;
  Alcotest.(check (option string))
    "get etag" (Some "\"get\"")
    (Option.map Object.Etag.to_string get.etag);
  match List.rev conn.calls with
  | [ put_call; get_call ] ->
      check_method "put method" "PUT" put_call.request;
      Alcotest.(check string) "put body" "hello" put_call.body;
      Alcotest.(check (option string))
        "put content type" (Some "text/plain")
        (header "content-type" put_call.request.headers);
      check_method "get method" "GET" get_call.request;
      Alcotest.(check (option string))
        "get expected owner" (Some "123456789012")
        (header "x-amz-expected-bucket-owner" get_call.request.headers)
  | _ -> Alcotest.fail "expected put and get calls"

let test_object_bytes_conveniences_preserve_binary_data () =
  let payload = "\000\255bytes" in
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("etag", "\"put\"") ] "";
        response 200
          ~headers:
            [
              ("etag", "\"get\"");
              ("content-length", string_of_int (String.length payload));
            ]
          payload;
      ]
  in
  ignore
    (Recording_s3.Object.put_bytes conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "blob.bin") ~contents:(Bytes.of_string payload) ()
    |> ok_or_fail "put bytes");
  let get =
    Recording_s3.Object.get_bytes conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "blob.bin")
      ~max_bytes:(Int64.of_int (String.length payload))
      ()
    |> ok_or_fail "get bytes"
  in
  Alcotest.(check string) "get bytes" payload (Bytes.to_string get.value);
  match List.rev conn.calls with
  | [ put_call; _get_call ] ->
      Alcotest.(check string) "put bytes" payload put_call.body
  | _ -> Alcotest.fail "expected put and get calls"

let test_object_find_conveniences_return_options () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"get\""); ("content-length", "5") ]
          "hello";
        response 404 no_such_key_body;
      ]
  in
  let found =
    Recording_s3.Object.find_string conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "present.txt") ~max_bytes:16L ()
    |> ok_or_fail "find string"
  in
  (match found with
  | Some result -> Alcotest.(check string) "found value" "hello" result.value
  | None -> Alcotest.fail "expected found object");
  let missing =
    Recording_s3.Object.find_bytes conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "missing.txt") ~max_bytes:16L ()
    |> ok_or_fail "find bytes missing"
  in
  Alcotest.(check bool) "missing" true (Option.is_none missing)

let test_object_convenience_get_validates_max_bytes () =
  let conn = Recording_runtime.connect [ response 200 "should-not-read" ] in
  (match
     Recording_s3.Object.get_string conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file.txt") ~max_bytes:(-1L) ()
   with
  | Error error when is_validation_field "max_bytes" error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_bytes validation error");
  Alcotest.(check int) "transport not called" 0 (List.length conn.calls);
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"get\""); ("content-length", "6") ]
          "abcdef";
      ]
  in
  match
    Recording_s3.Object.get_bytes conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file.txt") ~max_bytes:3L ()
  with
  | Error error -> (
      match body_details error with
      | Some { message; limit = Some 3L } ->
          Alcotest.(check string)
            "message" "response body exceeded max_bytes" message
      | _ -> Alcotest.failf "unexpected error: %a" Error.pp error)
  | Ok _ -> Alcotest.fail "expected body limit error"

let test_object_get_rejects_malformed_content_range () =
  let conn =
    Recording_runtime.connect
      [
        response 206
          ~headers:
            [ ("content-length", "4"); ("content-range", "bytes 5-2/10") ]
          "cdef";
      ]
  in
  let options =
    Object.Get.options_exn ~range:(Range.bytes_exn ~start:2L ~finish:5L) ()
  in
  match
    Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ~options
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Error error when is_decode_error error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed Content-Range decode error"

let test_object_head_parses_int64_content_length () =
  let large_content_length = 9_223_372_036_854_775_807L in
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("etag", "\"etag\"");
              ("content-length", Int64.to_string large_content_length);
            ]
          "";
      ]
  in
  let result =
    Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "huge.bin") ()
    |> ok_or_fail "head huge object"
  in
  Alcotest.(check (option int64))
    "content length" (Some large_content_length) result.content_length

let test_object_head_accepts_http_date_last_modified () =
  let last_modified_header = "Wed, 24 Jun 2026 02:04:39 GMT" in
  let expected =
    Ptime.of_date_time ((2026, 6, 24), ((2, 4, 39), 0)) |> Option.get
  in
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("etag", "\"etag\"");
              ("content-length", "0");
              ("last-modified", last_modified_header);
            ]
          "";
      ]
  in
  let result =
    Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ()
    |> ok_or_fail "head HTTP-date last-modified"
  in
  Alcotest.(check (option string))
    "last modified"
    (Some (Ptime.to_rfc3339 expected))
    (Option.map Ptime.to_rfc3339 result.last_modified)

let test_object_head_rejects_malformed_known_headers () =
  let cases =
    [
      ("etag", [ ("etag", ""); ("content-length", "0") ], "etag");
      ( "last modified",
        [
          ("etag", "\"etag\"");
          ("content-length", "0");
          ("last-modified", "not-a-time");
        ],
        "last-modified" );
      ( "content length",
        [ ("etag", "\"etag\""); ("content-length", "-1") ],
        "content-length" );
    ]
  in
  List.iter
    (fun (label, headers, field) ->
      let conn = Recording_runtime.connect [ response 200 ~headers "" ] in
      match
        Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
          ~key:(object_key "file") ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            (label ^ " mentions field")
            true
            (string_contains text ~substring:field)
      | Error error ->
          Alcotest.failf "%s: unexpected error: %a" label Error.pp error
      | Ok _ -> Alcotest.failf "%s: expected decode error" label)
    cases

let test_delete_objects_request_body () =
  let conn = Recording_runtime.connect [ response 200 "<DeleteResult/>" ] in
  let version_id = Object.Version_id.of_string_exn "version-1" in
  let etag = Object.Etag.of_string_exn "\"etag\"" in
  let objects =
    [
      Object.Delete_many.object_ ~key:(object_key "key-only.txt") ();
      Object.Delete_many.object_
        ~key:(object_key "versioned.txt")
        ~version_id ();
      Object.Delete_many.object_ ~key:(object_key "etag.txt") ~etag ();
    ]
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
       ~objects ()
    |> ok_or_fail "delete objects request body");
  let call = Recording_runtime.last_call conn in
  check_method "delete objects method" "POST" call.request;
  Alcotest.(check (list (pair string (list string))))
    "delete query"
    [ ("delete", []) ]
    call.request.target.query;
  Alcotest.(check bool)
    "content-md5 present" true
    (Option.is_some (header "content-md5" call.request.headers));
  Alcotest.(check (option string))
    "content type" (Some "application/xml")
    (header "content-type" call.request.headers);
  let body = call.body in
  let check_contains label substring =
    Alcotest.(check bool) label true (string_contains ~substring body)
  in
  let check_absent label substring =
    Alcotest.(check bool) label false (string_contains ~substring body)
  in
  check_absent "quiet omitted" "<Quiet>";
  check_contains "key-only key" "<Key>key-only.txt</Key>";
  check_contains "versioned key" "<Key>versioned.txt</Key>";
  check_contains "version id" "<VersionId>version-1</VersionId>";
  check_contains "etag key" "<Key>etag.txt</Key>";
  check_contains "etag" "<ETag>etag</ETag>";
  check_absent "last modified omitted" "LastModifiedTime";
  check_absent "size omitted" "<Size>"

let test_delete_objects_response_decode () =
  let body =
    {|<DeleteResult><Deleted><Key>deleted.txt</Key><VersionId>version-1</VersionId><DeleteMarker>true</DeleteMarker><DeleteMarkerVersionId>marker-version</DeleteMarkerVersionId></Deleted><Error><Key>blocked.txt</Key><Code>AccessDenied</Code><Message>denied</Message></Error></DeleteResult>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let object_ = Object.Delete_many.object_ ~key:(object_key "deleted.txt") () in
  let result =
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects:[ object_ ] ()
    |> ok_or_fail "delete objects response decode"
  in
  (match result.deleted with
  | [ deleted ] ->
      Alcotest.(check string)
        "deleted key" "deleted.txt"
        (Object_key.to_string deleted.key);
      Alcotest.(check (option string))
        "deleted version" (Some "version-1")
        (version_string deleted.version_id);
      Alcotest.(check (option bool))
        "delete marker" (Some true) deleted.delete_marker;
      Alcotest.(check (option string))
        "delete marker version" (Some "marker-version")
        (version_string deleted.delete_marker_version_id)
  | _ -> Alcotest.fail "expected one deleted member");
  match result.errors with
  | [ error ] ->
      Alcotest.(check string)
        "error key" "blocked.txt"
        (Object_key.to_string error.key);
      Alcotest.(check string) "error code" "AccessDenied" error.code;
      Alcotest.(check (option string))
        "error message" (Some "denied") error.message
  | _ -> Alcotest.fail "expected one error member"

let test_delete_objects_embedded_error () =
  let body =
    {|<Error><Code>MalformedXML</Code><Message>request body is malformed</Message></Error>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let object_ = Object.Delete_many.object_ ~key:(object_key "file") () in
  match
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects:[ object_ ] ()
  with
  | Error error when Error.service_code error = Some "MalformedXML" -> ()
  | Error error ->
      Alcotest.failf "unexpected delete objects error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded delete objects error"

let test_delete_objects_rejects_invalid_count () =
  let conn = Recording_runtime.connect [] in
  let object_ = Object.Delete_many.object_ ~key:(object_key "file") () in
  let result =
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects:[] ()
  in
  expect_validation "delete objects empty" result;
  let objects =
    List.init (Object.Delete_many.max_objects + 1) (fun _ -> object_)
  in
  let result =
    Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
      ~objects ()
  in
  expect_validation "delete objects too many" result

let test_object_version_id_queries () =
  let version_id = Object.Version_id.of_string_exn "version-1" in
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("content-length", "0") ] "";
        response 200 ~headers:[ ("content-length", "0") ] "";
        response 204 "";
      ]
  in
  let get_options = Object.Get.options_exn ~version_id () in
  ignore
    (Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:get_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get version");
  let head_options = Object.Head.options_exn ~version_id () in
  ignore
    (Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:head_options ()
    |> ok_or_fail "head version");
  let delete_options = Object.Delete.options_exn ~version_id () in
  ignore
    (Recording_s3.Object.delete conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:delete_options ()
    |> ok_or_fail "delete version");
  match List.rev conn.calls with
  | [ get; head; delete ] ->
      List.iter
        (fun (label, expected_method, (call : Recording_runtime.call)) ->
          check_method (label ^ " method") expected_method call.request;
          Alcotest.(check (list (pair string (list string))))
            (label ^ " query")
            [ ("versionId", [ "version-1" ]) ]
            call.request.target.query)
        [
          ("get", "GET", get);
          ("head", "HEAD", head);
          ("delete", "DELETE", delete);
        ]
  | _ -> Alcotest.fail "expected get/head/delete calls"

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
    { Object.Copy.default_options with source_version_id = Some version_id }
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
    Object.Versions.options_exn
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
    "next key marker" (Some "logs/b.txt")
    (Option.map Object_key.to_string page.next_key_marker);
  Alcotest.(check (option string))
    "next version marker"
    (Some (Object.Version_id.to_string next_version_id))
    (version_string page.next_version_id_marker);
  match List.rev conn.calls with
  | [ copy_call; versions_call ] ->
      check_method "copy versioning method" "PUT" copy_call.request;
      check_method "versions method" "GET" versions_call.request;
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

let test_copy_source_exact_once_encoding () =
  let version_id = Object.Version_id.of_string_exn "v 1/%?" in
  let conn =
    Recording_runtime.connect
      [
        response 200
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
      ]
  in
  let options =
    { Object.Copy.default_options with source_version_id = Some version_id }
  in
  ignore
    (Recording_s3.Object.copy conn
       ~source_bucket:(bucket_name "source-bucket")
       ~source_key:(object_key "space dir/a%25?b.txt")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ~options ()
    |> ok_or_fail "copy source exact once encoding");
  let call = Recording_runtime.last_call conn in
  check_method "copy source encoding method" "PUT" call.request;
  Alcotest.(check (option string))
    "copy source header"
    (Some "/source-bucket/space%20dir/a%2525%3Fb.txt?versionId=v%201%2F%25%3F")
    (header "x-amz-copy-source" call.request.headers)

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
  Alcotest.(check (option string))
    "key marker" None
    (Option.map Object_key.to_string page.key_marker);
  Alcotest.(check (option string))
    "version marker" None
    (version_string page.version_id_marker);
  Alcotest.(check (option string))
    "next key marker" None
    (Option.map Object_key.to_string page.next_key_marker);
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
    Recording_s3.Object.Versions.pages conn ~bucket:(bucket_name "my-bucket")
      ~max_pages:2 ()
  with
  | Error error when is_decode_error error ->
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
  | Ok None ->
      check_method "find metadata bare head 404 method" "HEAD"
        (Recording_runtime.last_call conn).request
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

let test_object_exists_missing_object_returns_false () =
  let conn = Recording_runtime.connect [ response 404 no_such_key_body ] in
  let exists =
    Recording_s3.Object.exists conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "missing") ()
    |> ok_or_fail "object exists missing object"
  in
  Alcotest.(check bool) "exists" false exists;
  check_method "object exists method" "HEAD"
    (Recording_runtime.last_call conn).request

let test_object_exists_missing_bucket_returns_error () =
  let conn = Recording_runtime.connect [ response 404 no_such_bucket_body ] in
  match
    Recording_s3.Object.exists conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when Error.is_no_such_bucket error ->
      check_method "object exists missing bucket method" "HEAD"
        (Recording_runtime.last_call conn).request
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok false -> Alcotest.fail "expected missing bucket error, got false"
  | Ok true -> Alcotest.fail "expected missing bucket error"

let test_object_exists_forwards_head_options () =
  let etag = Object.Etag.of_string_exn "\"etag\"" in
  let preconditions =
    {
      Object.Preconditions.Read.none with
      if_match = Some (Object.Etag_condition.etag etag);
    }
  in
  let options =
    Object.Head.options_exn ~preconditions
      ~version_id:(Object.Version_id.of_string_exn "version-1")
      ~checksum_mode:Object.Checksum.Mode.Enabled
      ~expected_bucket_owner:(account_id "123456789012")
      ()
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let exists =
    Recording_s3.Object.exists conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ~options ()
    |> ok_or_fail "exists forwards head options"
  in
  Alcotest.(check bool) "exists" true exists;
  let call = Recording_runtime.last_call conn in
  check_method "method" "HEAD" call.request;
  Alcotest.(check (list (pair string (list string))))
    "version query"
    [ ("versionId", [ "version-1" ]) ]
    call.request.target.query;
  Alcotest.(check (option string))
    "if-match" (Some "\"etag\"")
    (header "if-match" call.request.headers);
  Alcotest.(check (option string))
    "checksum mode" (Some "ENABLED")
    (header "x-amz-checksum-mode" call.request.headers);
  Alcotest.(check (option string))
    "expected owner" (Some "123456789012")
    (header "x-amz-expected-bucket-owner" call.request.headers)

let test_object_unknown_storage_class_read_values () =
  let expect_unknown label = function
    | Some (Storage_class.Unknown value) ->
        Alcotest.(check string) label "FUTURE_CLASS" value
    | Some storage_class ->
        Alcotest.failf "%s: expected unknown storage class, got %s" label
          (Storage_class.to_string storage_class)
    | None -> Alcotest.failf "%s: expected storage class" label
  in
  let list_body =
    {|<ListBucketResult><Contents><Key>a.txt</Key><StorageClass>FUTURE_CLASS</StorageClass></Contents></ListBucketResult>|}
  in
  let versions_body =
    {|<ListVersionsResult><Version><Key>a.txt</Key><VersionId>v1</VersionId><StorageClass>FUTURE_CLASS</StorageClass><Owner><ID>owner-id</ID><DisplayName>owner-name</DisplayName></Owner></Version><DeleteMarker><Key>a.txt</Key><Owner><ID>marker-owner</ID><DisplayName>marker-name</DisplayName></Owner></DeleteMarker></ListVersionsResult>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [ ("content-length", "0"); ("x-amz-storage-class", "FUTURE_CLASS") ]
          "";
        response 200
          ~headers:
            [ ("content-length", "0"); ("x-amz-storage-class", "FUTURE_CLASS") ]
          "";
        response 200 list_body;
        response 200 versions_body;
      ]
  in
  let get =
    Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "a.txt")
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
    |> ok_or_fail "get unknown storage class"
  in
  expect_unknown "get storage class" get.storage_class;
  let head =
    Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "a.txt") ()
    |> ok_or_fail "head unknown storage class"
  in
  expect_unknown "head storage class" head.storage_class;
  let page =
    Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "list unknown storage class"
  in
  (match page.objects with
  | [ object_ ] -> expect_unknown "list storage class" object_.storage_class
  | _ -> Alcotest.fail "expected one listed object");
  let versions =
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "versions unknown storage class"
  in
  (match versions.versions with
  | [ version ] -> (
      expect_unknown "version storage class" version.storage_class;
      match version.owner with
      | Some
          {
            Object.Owner.id = Some "owner-id";
            display_name = Some "owner-name";
          } ->
          ()
      | _ -> Alcotest.fail "expected structured version owner")
  | _ -> Alcotest.fail "expected one object version");
  match versions.delete_markers with
  | [ marker ] -> (
      match marker.owner with
      | Some
          {
            Object.Owner.id = Some "marker-owner";
            display_name = Some "marker-name";
          } ->
          ()
      | _ -> Alcotest.fail "expected structured marker owner")
  | _ -> Alcotest.fail "expected one delete marker"

let test_object_observed_only_write_values_rejected () =
  let unknown_storage = Storage_class.Unknown "FUTURE_CLASS" in
  let unknown_checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm =
        Object.Checksum.Algorithm.Unknown "FUTURE_CHECKSUM";
      value = "checksum";
    }
  in
  expect_validation_field "put builder storage" "storage_class"
    (Object.Put.options ~storage_class:unknown_storage ());
  expect_validation_field "put builder checksum" "checksum_algorithm"
    (Object.Put.options ~checksum:unknown_checksum ());
  expect_validation_field "copy builder storage" "storage_class"
    (Object.Copy.options ~storage_class:unknown_storage ());
  expect_validation_field "copy builder checksum" "checksum_algorithm"
    (Object.Copy.options
       ~checksum_algorithm:(Object.Checksum.Algorithm.Unknown "FUTURE_CHECKSUM")
       ());
  let put_options =
    { Object.Put.default_options with storage_class = Some unknown_storage }
  in
  let put_conn = Recording_runtime.connect [] in
  expect_validation_field "put operation storage" "storage_class"
    (Recording_s3.Object.put put_conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:put_options
       ~body:(Recording_s3.Body.of_string "body")
       ());
  Alcotest.(check int) "put not sent" 0 (List.length put_conn.calls);
  let copy_options =
    { Object.Copy.default_options with storage_class = Some unknown_storage }
  in
  let copy_conn = Recording_runtime.connect [] in
  expect_validation_field "copy operation storage" "storage_class"
    (Recording_s3.Object.copy copy_conn ~source_bucket:(bucket_name "my-bucket")
       ~source_key:(object_key "file")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ~options:copy_options ());
  Alcotest.(check int) "copy not sent" 0 (List.length copy_conn.calls)

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
      let body = result.Object.Get.value in
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

let test_get_consumer_retryable_service_error_is_final () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "4") ]
          "body";
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "5") ]
          "again";
      ]
  in
  let consumer_calls = ref 0 in
  let consumer_error =
    service_error ~code:"SlowDown" ~message:"consumer-owned throttling" 503
  in
  (match
     Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file")
       ~consume:(fun _reader ->
         incr consumer_calls;
         Error consumer_error)
       ()
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected consumer service error");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps);
  Alcotest.(check int) "consumer calls" 1 !consumer_calls

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
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  match Recording_s3.Multipart.list_parts conn ~upload () with
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

let expect_list_decode_error label body field =
  let conn = Recording_runtime.connect [ response 200 body ] in
  match Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") () with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        (label ^ " mentions " ^ field)
        true
        (string_contains text ~substring:field)
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected decode error" label

let expect_version_list_decode_error label body field =
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        (label ^ " mentions " ^ field)
        true
        (string_contains text ~substring:field)
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected decode error" label

let test_object_list_rejects_invalid_typed_fields () =
  [
    ( "invalid key",
      "<ListBucketResult><Contents><Key></Key></Contents></ListBucketResult>",
      "Key" );
    ( "invalid bucket",
      "<ListBucketResult><Name>Bad_Bucket</Name></ListBucketResult>",
      "Name" );
    ( "invalid delimiter",
      "<ListBucketResult><Delimiter></Delimiter></ListBucketResult>",
      "Delimiter" );
    ( "invalid continuation token",
      "<ListBucketResult><NextContinuationToken></NextContinuationToken></ListBucketResult>",
      "NextContinuationToken" );
    ( "invalid common prefix",
      "<ListBucketResult><CommonPrefixes><Prefix></Prefix></CommonPrefixes></ListBucketResult>",
      "Prefix" );
    ( "invalid storage class",
      "<ListBucketResult><Contents><Key>a.txt</Key><StorageClass></StorageClass></Contents></ListBucketResult>",
      "StorageClass" );
  ]
  |> List.iter (fun (label, body, field) ->
      expect_list_decode_error label body field)

let test_object_versions_rejects_invalid_typed_fields () =
  [
    ( "invalid version key",
      "<ListVersionsResult><Version><Key></Key></Version></ListVersionsResult>",
      "Key" );
    ( "invalid delete marker key",
      "<ListVersionsResult><DeleteMarker><Key></Key></DeleteMarker></ListVersionsResult>",
      "Key" );
    ( "invalid bucket",
      "<ListVersionsResult><Name>Bad_Bucket</Name></ListVersionsResult>",
      "Name" );
    ( "invalid delimiter",
      "<ListVersionsResult><Delimiter></Delimiter></ListVersionsResult>",
      "Delimiter" );
    ( "invalid common prefix",
      "<ListVersionsResult><CommonPrefixes><Prefix></Prefix></CommonPrefixes></ListVersionsResult>",
      "Prefix" );
    ( "invalid delete marker version id",
      "<ListVersionsResult><DeleteMarker><Key>a.txt</Key><VersionId></VersionId></DeleteMarker></ListVersionsResult>",
      "VersionId" );
    ( "invalid storage class",
      "<ListVersionsResult><Version><Key>a.txt</Key><StorageClass></StorageClass></Version></ListVersionsResult>",
      "StorageClass" );
  ]
  |> List.iter (fun (label, body, field) ->
      expect_version_list_decode_error label body field)

let test_list_options_reject_invalid_max_keys () =
  let check label result =
    match result with
    | Error error when is_validation_field "max_keys" error -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected max_keys validation error" label
  in
  [
    ("list zero", Object.List.options ~max_keys:0 ());
    ("list negative", Object.List.options ~max_keys:(-1) ());
    ("list too large", Object.List.options ~max_keys:1001 ());
  ]
  |> List.iter (fun (label, result) -> check label result);
  [
    ("versions zero", Object.Versions.options ~max_keys:0 ());
    ("versions negative", Object.Versions.options ~max_keys:(-1) ());
    ("versions too large", Object.Versions.options ~max_keys:1001 ());
  ]
  |> List.iter (fun (label, build) -> check label build)

let test_list_operations_reject_record_max_keys () =
  let check label result =
    match result with
    | Error error when is_validation_field "max_keys" error -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected max_keys validation error" label
  in
  let list_options = { Object.List.default_options with max_keys = Some 0 } in
  let version_options =
    { Object.Versions.default_options with max_keys = Some 1001 }
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 "<ListBucketResult></ListBucketResult>";
        response 200 "<ListVersionsResult></ListVersionsResult>";
      ]
  in
  check "list operation"
    (Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket")
       ~options:list_options ());
  check "versions operation"
    (Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket")
       ~options:version_options ())

let test_list_decodes_empty_prefix_as_none () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<ListBucketResult><Name>my-bucket</Name><Prefix></Prefix><IsTruncated>false</IsTruncated></ListBucketResult>";
        response 200
          "<ListVersionsResult><Name>my-bucket</Name><Prefix></Prefix><IsTruncated>false</IsTruncated></ListVersionsResult>";
      ]
  in
  let page =
    Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "list empty prefix"
  in
  Alcotest.(check bool) "list prefix absent" true (Option.is_none page.prefix);
  let versions =
    Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "versions empty prefix"
  in
  Alcotest.(check bool)
    "versions prefix absent" true
    (Option.is_none versions.prefix)

let test_object_versions_rejects_malformed_known_fields () =
  let cases =
    [
      ( "IsLatest",
        "<ListVersionsResult><Version><Key>a.txt</Key><IsLatest>maybe</IsLatest></Version></ListVersionsResult>"
      );
      ( "VersionId",
        "<ListVersionsResult><Version><Key>a.txt</Key><VersionId></VersionId></Version></ListVersionsResult>"
      );
    ]
  in
  List.iter
    (fun (field, body) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match
        Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket")
          ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            ("mentions " ^ field) true
            (string_contains text ~substring:field)
      | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
      | Ok _ -> Alcotest.failf "expected malformed %s decode error" field)
    cases

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

let test_object_tagging_request_wire () =
  let expected_owner = account_id "123456789012" in
  let options =
    Object.Tagging.options_exn ~expected_bucket_owner:expected_owner ()
  in
  let tags = tag_set [ tag "env" "prod" ] in
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<Tagging><TagSet><Tag><Key>env</Key><Value>prod</Value></Tag></TagSet></Tagging>";
        response 200 "";
        response 204 "";
      ]
  in
  ignore
    (Recording_s3.Object.Tagging.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options ()
    |> ok_or_fail "get object tagging");
  ignore
    (Recording_s3.Object.Tagging.put conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options ~tags ()
    |> ok_or_fail "put object tagging");
  ignore
    (Recording_s3.Object.Tagging.delete conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options ()
    |> ok_or_fail "delete object tagging");
  match List.rev conn.calls with
  | [ get; put; delete ] ->
      List.iter
        (fun (label, expected_method, (call : Recording_runtime.call)) ->
          check_method (label ^ " method") expected_method call.request;
          Alcotest.(check (list (pair string (list string))))
            (label ^ " query")
            [ ("tagging", []) ]
            call.request.target.query;
          Alcotest.(check (option string))
            (label ^ " expected owner")
            (Some "123456789012")
            (header "x-amz-expected-bucket-owner" call.request.headers))
        [
          ("get", "GET", get); ("put", "PUT", put); ("delete", "DELETE", delete);
        ];
      Alcotest.(check bool)
        "put content-md5 present" true
        (Option.is_some (header "content-md5" put.request.headers));
      Alcotest.(check (option string))
        "put content type" (Some "application/xml")
        (header "content-type" put.request.headers);
      Alcotest.(check bool)
        "put tag key" true
        (string_contains put.body ~substring:"<Key>env</Key>");
      Alcotest.(check bool)
        "put tag value" true
        (string_contains put.body ~substring:"<Value>prod</Value>")
  | _ -> Alcotest.fail "expected object tagging calls"

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

let test_copy_object_retryable_embedded_error_retries_then_succeeds () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 slow_down;
        response 200
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
      ]
  in
  ignore
    (Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
       ~source_key:(object_key "file")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ()
    |> ok_or_fail "copy after embedded SlowDown");
  Alcotest.(check int) "attempts" 2 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 1 (List.length conn.sleeps)

let test_copy_object_non_retryable_embedded_error_is_final () =
  let invalid_request =
    {|<Error><Code>InvalidRequest</Code><Message>invalid copy</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 invalid_request;
        response 200
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
      ]
  in
  (match
     Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
       ~source_key:(object_key "file")
       ~destination_bucket:(bucket_name "my-bucket")
       ~destination_key:(object_key "copy") ()
   with
  | Error error when Error.service_code error = Some "InvalidRequest" -> ()
  | Error error -> Alcotest.failf "unexpected copy error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected embedded copy error");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_copy_object_rejects_malformed_result_fields () =
  let cases =
    [
      ("etag", {|<CopyObjectResult><ETag></ETag></CopyObjectResult>|}, "ETag");
      ( "last modified",
        {|<CopyObjectResult><ETag>"copy"</ETag><LastModified>not-a-time</LastModified></CopyObjectResult>|},
        "LastModified" );
    ]
  in
  List.iter
    (fun (label, body, field) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match
        Recording_s3.Object.copy conn ~source_bucket:(bucket_name "my-bucket")
          ~source_key:(object_key "file")
          ~destination_bucket:(bucket_name "my-bucket")
          ~destination_key:(object_key "copy") ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            (label ^ " mentions field")
            true
            (string_contains text ~substring:field)
      | Error error ->
          Alcotest.failf "%s: unexpected error: %a" label Error.pp error
      | Ok _ -> Alcotest.failf "%s: expected decode error" label)
    cases

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
    Object.Get.options_exn ~checksum_mode:Object.Checksum.Mode.Enabled
      ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.get conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:read_options
       ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
       ()
    |> ok_or_fail "get checksum mode");
  let head_options =
    Object.Head.options_exn ~checksum_mode:Object.Checksum.Mode.Enabled
      ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:head_options ()
    |> ok_or_fail "head checksum mode");
  let delete_options =
    Object.Delete.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.delete conn ~bucket:(bucket_name "my-bucket")
       ~key:(object_key "file") ~options:delete_options ()
    |> ok_or_fail "delete expected owner");
  let delete_many_options =
    Object.Delete_many.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.delete_objects conn ~bucket:(bucket_name "my-bucket")
       ~objects:[ Object.Delete_many.object_ ~key:(object_key "file") () ]
       ~options:delete_many_options ()
    |> ok_or_fail "delete many expected owner");
  let copy_options =
    Object.Copy.options_exn ~checksum_algorithm:Object.Checksum.Algorithm.Sha256
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
    Object.List.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket")
       ~options:list_options ()
    |> ok_or_fail "list expected owner");
  let version_options =
    Object.Versions.options_exn ~expected_bucket_owner:expected_owner ()
  in
  ignore
    (Recording_s3.Object.list_versions conn ~bucket:(bucket_name "my-bucket")
       ~options:version_options ()
    |> ok_or_fail "list versions expected owner");
  match List.rev conn.calls with
  | [ get; head; delete; delete_many; copy; list; versions ] ->
      check_method "get expected owner method" "GET" get.request;
      check_method "head expected owner method" "HEAD" head.request;
      check_method "delete expected owner method" "DELETE" delete.request;
      check_method "delete many expected owner method" "POST"
        delete_many.request;
      check_method "copy expected owner method" "PUT" copy.request;
      check_method "list expected owner method" "GET" list.request;
      check_method "versions expected owner method" "GET" versions.request;
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
        Alcotest.test_case "object get range header" `Quick
          test_object_get_range_header;
        Alcotest.test_case "object string conveniences share operation model"
          `Quick test_object_string_conveniences_share_operation_model;
        Alcotest.test_case "object bytes conveniences preserve binary data"
          `Quick test_object_bytes_conveniences_preserve_binary_data;
        Alcotest.test_case "object find conveniences return options" `Quick
          test_object_find_conveniences_return_options;
        Alcotest.test_case "object convenience get validates max bytes" `Quick
          test_object_convenience_get_validates_max_bytes;
        Alcotest.test_case "object get rejects malformed content range" `Quick
          test_object_get_rejects_malformed_content_range;
        Alcotest.test_case "object head parses int64 content length" `Quick
          test_object_head_parses_int64_content_length;
        Alcotest.test_case "object head accepts HTTP-date Last-Modified" `Quick
          test_object_head_accepts_http_date_last_modified;
        Alcotest.test_case "object head rejects malformed known headers" `Quick
          test_object_head_rejects_malformed_known_headers;
        Alcotest.test_case "delete objects request body" `Quick
          test_delete_objects_request_body;
        Alcotest.test_case "delete objects response decode" `Quick
          test_delete_objects_response_decode;
        Alcotest.test_case "delete objects embedded error" `Quick
          test_delete_objects_embedded_error;
        Alcotest.test_case "delete objects rejects invalid count" `Quick
          test_delete_objects_rejects_invalid_count;
        Alcotest.test_case "object version id queries" `Quick
          test_object_version_id_queries;
        Alcotest.test_case "object versioning requests and parse" `Quick
          test_object_versioning_requests_and_parse;
        Alcotest.test_case "copy source exact once encoding" `Quick
          test_copy_source_exact_once_encoding;
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
        Alcotest.test_case "object exists missing object returns false" `Quick
          test_object_exists_missing_object_returns_false;
        Alcotest.test_case "object exists missing bucket returns error" `Quick
          test_object_exists_missing_bucket_returns_error;
        Alcotest.test_case "object exists forwards head options" `Quick
          test_object_exists_forwards_head_options;
        Alcotest.test_case "object unknown storage class read values" `Quick
          test_object_unknown_storage_class_read_values;
        Alcotest.test_case "object observed-only write values rejected" `Quick
          test_object_observed_only_write_values_rejected;
        Alcotest.test_case "find success returns some" `Quick
          test_find_success_returns_some;
        Alcotest.test_case "find missing object returns none" `Quick
          test_find_missing_object_returns_none;
        Alcotest.test_case "find missing bucket returns error" `Quick
          test_find_missing_bucket_returns_error;
        Alcotest.test_case "find preserves consumer not found error" `Quick
          test_find_preserves_consumer_not_found_error;
        Alcotest.test_case "get consumer retryable service error is final"
          `Quick test_get_consumer_retryable_service_error_is_final;
        Alcotest.test_case "malformed xml responses" `Quick
          test_malformed_xml_responses;
        Alcotest.test_case "object list rejects malformed known fields" `Quick
          test_object_list_rejects_malformed_known_fields;
        Alcotest.test_case "object list rejects negative numeric fields" `Quick
          test_object_list_rejects_negative_numeric_fields;
        Alcotest.test_case "object list rejects invalid typed fields" `Quick
          test_object_list_rejects_invalid_typed_fields;
        Alcotest.test_case "object versions rejects invalid typed fields" `Quick
          test_object_versions_rejects_invalid_typed_fields;
        Alcotest.test_case "list options reject invalid max keys" `Quick
          test_list_options_reject_invalid_max_keys;
        Alcotest.test_case "list operations reject record max keys" `Quick
          test_list_operations_reject_record_max_keys;
        Alcotest.test_case "list decodes empty prefix as none" `Quick
          test_list_decodes_empty_prefix_as_none;
        Alcotest.test_case "object versions rejects malformed known fields"
          `Quick test_object_versions_rejects_malformed_known_fields;
        Alcotest.test_case "object list allows unknown extra elements" `Quick
          test_object_list_allows_unknown_extra_elements;
        Alcotest.test_case "object tagging rejects incomplete tag xml" `Quick
          test_object_tagging_rejects_incomplete_tag_xml;
        Alcotest.test_case "object tagging request wire" `Quick
          test_object_tagging_request_wire;
        Alcotest.test_case
          "head rejects duplicate metadata headers as decode error" `Quick
          test_head_rejects_duplicate_metadata_headers_as_decode_error;
        Alcotest.test_case "copy object retryable embedded error" `Quick
          test_copy_object_retryable_embedded_error_retries_then_succeeds;
        Alcotest.test_case "copy object non-retryable embedded error" `Quick
          test_copy_object_non_retryable_embedded_error_is_final;
        Alcotest.test_case "copy object rejects malformed result fields" `Quick
          test_copy_object_rejects_malformed_result_fields;
        Alcotest.test_case "object checksum mode and expected owner headers"
          `Quick test_object_checksum_mode_and_expected_owner_headers;
      ] );
  ]
