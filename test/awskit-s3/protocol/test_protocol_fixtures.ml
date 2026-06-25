open Awskit_s3
open Awskit_s3_test

let fixture_path parts =
  List.fold_left Filename.concat "../fixtures/protocol" parts

let check_fixture label parts ~actual =
  Fixture_diff.check_file label (fixture_path parts) ~actual

let option_to_string render = function
  | None -> "none"
  | Some value -> render value

let query_to_string query = Awskit.Signing.canonical_query_params query

let header_or_empty name headers =
  match header name headers with None -> "" | Some value -> value

let test_presigned_get_fixture () =
  let options : Presigned.Get_object.options =
    {
      Presigned.Get_object.default_options with
      expires_in = Some (Ptime.Span.of_int_s 900);
      response_content_type = Some (content_type "text/plain");
      version_id = Some (Object.Version_id.of_string_exn "version-1");
    }
  in
  let request =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned get fixture"
  in
  let redacted_url =
    Fixture_diff.normalize_uri
      ~redact:[ "X-Amz-Credential"; "X-Amz-Signature" ]
      (Presigned.reveal_url request)
  in
  check_fixture "presigned GET redacted URL"
    [ "presign"; "get-object-redacted-url.txt" ]
    ~actual:redacted_url;
  let safe_uri = Presigned.safe_uri request |> Uri.to_string in
  check_fixture "presigned GET safe URI"
    [ "presign"; "get-object-safe-uri.txt" ]
    ~actual:safe_uri

let redact_authorization authorization =
  match String.index_opt authorization ' ' with
  | Some index ->
      let algorithm = String.sub authorization 0 index in
      let fields =
        String.sub authorization (index + 1)
          (String.length authorization - index - 1)
      in
      let redacted_field field =
        match String.split_on_char '=' field with
        | [ "Credential"; _ ] -> "Credential=REDACTED"
        | [ "Signature"; _ ] -> "Signature=REDACTED"
        | _ -> field
      in
      algorithm
      ^ " "
      ^ (fields
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.map redacted_field
        |> String.concat ", ")
  | None -> authorization

let test_signing_artifact_fixture () =
  let headers =
    [
      ("host", "bucket.s3.us-east-1.amazonaws.com");
      ("x-amz-meta-trace", "  replay\t fixture  ");
      ("x-amz-meta-trace", "second");
    ]
  in
  let payload_hash = Awskit.Body.Payload_hash.sha256_of_string "body" in
  let signed =
    Awskit.Signing.sign_request_params ~credentials:creds
      ~region:(Region.of_string_exn "us-east-1")
      ~service:"s3" ~method_:`PUT ~path:"/photos/cat space.jpg"
      ~query_params:
        [
          ("partNumber", [ "10" ]);
          ("uploadId", [ "upload 1" ]);
          ("X-Amz-Meta", [ "a/b"; "a b" ]);
        ]
      ~headers ~payload_hash ~now:test_time
    |> ok_or_fail "signing artifact fixture"
  in
  let canonical_headers =
    Awskit.Signing.canonical_headers
      (List.filter
         (fun (name, _) -> not (String.equal name "authorization"))
         signed.headers)
  in
  let authorization = header_or_empty "authorization" signed.headers in
  let actual =
    Fmt.str
      "method=PUT\n\
       canonical-path=%s\n\
       canonical-query=%s\n\
       canonical-headers=%ssigned-headers=%s\n\
       payload-hash=%s\n\
       authorization=%s"
      (Awskit.Signing.uri_encode ~encode_slash:false "/photos/cat space.jpg")
      (Awskit.Signing.canonical_query_params
         [
           ("partNumber", [ "10" ]);
           ("uploadId", [ "upload 1" ]);
           ("X-Amz-Meta", [ "a/b"; "a b" ]);
         ])
      (Awskit.Signing.canonical_headers_block canonical_headers)
      signed.signed_headers_str
      (Awskit.Body.Payload_hash.to_header_value payload_hash)
      (redact_authorization authorization)
  in
  check_fixture "signing canonical artifact"
    [ "signing"; "put-object-canonical.expected" ]
    ~actual

let test_endpoint_resolution_fixture () =
  let region = Region.of_string_exn "us-east-1" in
  let resolved =
    Endpoint_resolver.resolve_object_request Endpoint_config.default ~region
      ~bucket:(bucket_name "bucket")
      ~key:(object_key "photos/cat.jpg")
    |> ok_or_fail "endpoint fixture"
  in
  let style =
    match resolved.style with
    | `Path -> "path"
    | `Virtual_hosted -> "virtual-hosted"
  in
  let actual =
    Fmt.str "endpoint=%s\npath=%s\nsigning_path=%s\nsigning_region=%s\nstyle=%s"
      (Awskit.Endpoint.to_url_prefix resolved.endpoint)
      resolved.path resolved.signing_path
      (Region.to_string resolved.signing_region)
      style
  in
  check_fixture "endpoint resolution"
    [ "endpoint"; "default-object.txt" ]
    ~actual

let test_put_object_metadata_tags_fixture () =
  let options =
    Object.Put.options_exn
      ~content_type:(content_type "text/plain")
      ~metadata:
        (Metadata.of_list_exn [ ("origin", "fixture"); ("trace", "abc-123") ])
      ~tags:(tag_set [ tag "env" "test"; tag "owner" "sdk" ])
      ()
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Object.put conn ~bucket:(bucket_name "bucket")
       ~key:(object_key "file.txt") ~options
       ~body:(Recording_s3.Body.of_string "body")
       ()
    |> ok_or_fail "put object metadata fixture");
  let call = Recording_runtime.last_call conn in
  let request = call.request in
  let target = request.Awskit.Request.target in
  let actual =
    Fmt.str
      "method=%s\n\
       path=%s\n\
       query=%s\n\
       content-type=%s\n\
       x-amz-meta-origin=%s\n\
       x-amz-meta-trace=%s\n\
       x-amz-tagging=%s\n\
       body=%s"
      (Awskit.Request.Method.to_string request.method_)
      target.path
      (query_to_string target.query)
      (header_or_empty "content-type" request.headers)
      (header_or_empty "x-amz-meta-origin" request.headers)
      (header_or_empty "x-amz-meta-trace" request.headers)
      (header_or_empty "x-amz-tagging" request.headers)
      call.body
  in
  check_fixture "put object metadata and tags"
    [ "object"; "put-metadata-tags.expected" ]
    ~actual

let content_range_field label field = function
  | None -> Alcotest.failf "%s: expected content range" label
  | Some content_range -> field content_range

let test_range_get_fixture () =
  let headers =
    [
      ("content-length", "4");
      ("content-range", "bytes 2-5/10");
      ("x-amz-meta-origin", "fixture");
    ]
  in
  let options =
    Object.Get.options_exn ~range:(Range.bytes_exn ~start:2L ~finish:5L) ()
  in
  let conn = Recording_runtime.connect [ response ~headers 206 "cdef" ] in
  let result =
    Recording_s3.Object.get conn ~bucket:(bucket_name "bucket")
      ~key:(object_key "file.txt") ~options
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:16L)
      ()
    |> ok_or_fail "range get fixture"
  in
  let actual =
    Fmt.str
      "status=%d\n\
       body=%s\n\
       content-length=%s\n\
       content-range=%s\n\
       range-start=%Ld\n\
       range-finish=%Ld\n\
       range-complete=%s\n\
       metadata-origin=%s"
      (Awskit.Response.status result.response)
      result.value
      (option_to_string Int64.to_string result.content_length)
      (option_to_string Range.Content_range.to_header result.content_range)
      (content_range_field "range start"
         (fun (range : Range.Content_range.t) -> range.start)
         result.content_range)
      (content_range_field "range finish"
         (fun (range : Range.Content_range.t) -> range.finish)
         result.content_range)
      (content_range_field "range complete"
         (fun (range : Range.Content_range.t) ->
           option_to_string Int64.to_string range.complete_length)
         result.content_range)
      (Option.value ~default:""
         (List.assoc_opt "origin" (Metadata.to_list result.metadata)))
  in
  check_fixture "range GET response" [ "object"; "range-get.expected" ] ~actual

let describe_request (call : Recording_runtime.call) =
  let request = call.request in
  let target = request.Awskit.Request.target in
  Fmt.str
    "method=%s\n\
     path=%s\n\
     query=%s\n\
     content-md5=%s\n\
     content-type=%s\n\
     expected-owner=%s\n\
     body=%s"
    (Awskit.Request.Method.to_string request.method_)
    target.path
    (query_to_string target.query)
    (header_or_empty "content-md5" request.headers)
    (header_or_empty "content-type" request.headers)
    (header_or_empty "x-amz-expected-bucket-owner" request.headers)
    call.body

let test_bucket_versioning_xml_fixture () =
  let options =
    Bucket.Versioning.options_exn
      ~expected_bucket_owner:(account_id "123456789012")
      ()
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.Versioning.put conn ~bucket:(bucket_name "my-bucket")
       ~options ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "bucket versioning XML fixture");
  check_fixture "bucket versioning XML"
    [ "bucket"; "versioning-put.expected" ]
    ~actual:(describe_request (Recording_runtime.last_call conn))

let test_bucket_encryption_xml_fixture () =
  let config =
    {
      Bucket.Encryption.rules =
        [
          {
            Bucket.Encryption.Rule.sse_algorithm =
              Some Bucket.Encryption.Algorithm.Aws_kms_dsse;
            kms_master_key_id =
              Some "arn:aws:kms:us-east-1:123456789012:key/test";
            bucket_key_enabled = Some true;
            blocked_encryption_types =
              [ Bucket.Encryption.Blocked_encryption_type.Sse_c ];
          };
        ];
    }
  in
  let options =
    Bucket.Encryption.options_exn
      ~expected_bucket_owner:(account_id "123456789012")
      ()
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  ignore
    (Recording_s3.Bucket.Encryption.put conn ~bucket:(bucket_name "my-bucket")
       ~options ~config ()
    |> ok_or_fail "bucket encryption XML fixture");
  check_fixture "bucket encryption XML"
    [ "bucket"; "encryption-put.expected" ]
    ~actual:(describe_request (Recording_runtime.last_call conn))

let describe_list_page (page : Object.List.page) =
  let render_prefix = Object_key.Prefix.to_string in
  let render_token = Object.List.Continuation_token.to_string in
  let objects =
    page.objects
    |> List.map (fun (object_ : Object.List.object_summary) ->
        Fmt.str "%s:%s"
          (Object_key.to_string object_.key)
          (option_to_string Int64.to_string object_.size))
    |> String.concat ","
  in
  let common_prefixes =
    page.common_prefixes
    |> List.map Object_key.Prefix.to_string
    |> String.concat ","
  in
  Fmt.str
    "bucket=%s\n\
     prefix=%s\n\
     delimiter=%s\n\
     key_count=%s\n\
     truncated=%b\n\
     continuation=%s\n\
     next=%s\n\
     objects=%s\n\
     common_prefixes=%s"
    (option_to_string Bucket_name.to_string page.bucket)
    (option_to_string render_prefix page.prefix)
    (option_to_string Object.List.Delimiter.to_string page.delimiter)
    (option_to_string string_of_int page.key_count)
    page.is_truncated
    (option_to_string render_token page.continuation_token)
    (option_to_string render_token page.next_continuation_token)
    objects common_prefixes

let test_list_objects_v2_fixture () =
  let body =
    Fixture_diff.read_file (fixture_path [ "pagination"; "list-v2.xml" ])
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let page =
    Recording_s3.Object.list conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "list objects fixture"
  in
  check_fixture "list objects v2 summary"
    [ "pagination"; "list-v2.expected" ]
    ~actual:(describe_list_page page)

let test_service_error_fixture () =
  let body =
    Fixture_diff.read_file (fixture_path [ "service-errors"; "slow-down.xml" ])
  in
  let conn =
    Recording_runtime.connect ~retry_policy:Awskit.Retry.disabled
      [ response 503 body ]
  in
  match
    Recording_s3.Object.put conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file.txt")
      ~body:(Recording_s3.Body.of_string "body")
      ()
  with
  | Error error -> (
      match Awskit.Error.kind error with
      | Service service ->
          let actual =
            Fmt.str "status=%d\ncode=%s\nmessage=%s" service.status
              (option_to_string Fun.id service.code)
              (option_to_string Fun.id service.message)
          in
          check_fixture "slow down service error"
            [ "service-errors"; "slow-down.expected" ]
            ~actual
      | _ -> Alcotest.failf "unexpected error kind: %a" Awskit.Error.pp error)
  | Ok _ -> Alcotest.fail "expected SlowDown service error"

let test_retry_decision_fixture () =
  let retry_error =
    Awskit.Error.Producer.transport ~retryable:true "temporary"
  in
  let policy =
    Awskit.Retry.create_exn ~max_attempts:4
      ~base_delay:(Ptime.Span.of_float_s 0.5 |> Option.get)
      ~max_delay:(Ptime.Span.of_float_s 2.0 |> Option.get)
      ~jitter:0.0 ()
  in
  let random_float ~upper_bound:_ = Alcotest.fail "jitter disabled" in
  let line attempt =
    match
      Awskit.Retry.delay policy ~attempt ~error:retry_error ~random_float
    with
    | None -> Fmt.str "attempt=%d stop" attempt
    | Some delay ->
        Fmt.str "attempt=%d delay=%.1f" attempt (Ptime.Span.to_float_s delay)
  in
  let actual = [ line 1; line 2; line 3; line 4 ] |> String.concat "\n" in
  check_fixture "retry decision trace" [ "retry"; "slow-down.expected" ] ~actual

let checksum value : Object.Checksum.value =
  Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256 ~value

let test_multipart_complete_xml_fixture () =
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin")
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")
  in
  let part number checksum_value =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn number)
      ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" number))
      ~checksum:(checksum checksum_value) ()
  in
  let conn =
    Recording_runtime.connect
      [
        response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  ignore
    (Recording_s3.Multipart.complete_upload conn ~upload
       ~parts:[ part 1 "sha256-part-1"; part 2 "sha256-part-2" ]
       ()
    |> ok_or_fail "complete multipart fixture");
  let body = (Recording_runtime.last_call conn).body in
  check_fixture "complete multipart XML"
    [ "multipart"; "complete.xml" ]
    ~actual:body

let suite =
  [
    ( "fixture:awskit-s3:protocol",
      [
        Alcotest.test_case "presigned GET" `Quick test_presigned_get_fixture;
        Alcotest.test_case "endpoint resolution" `Quick
          test_endpoint_resolution_fixture;
        Alcotest.test_case "PUT metadata/tags" `Quick
          test_put_object_metadata_tags_fixture;
        Alcotest.test_case "range GET response" `Quick test_range_get_fixture;
        Alcotest.test_case "signing canonical artifact" `Quick
          test_signing_artifact_fixture;
        Alcotest.test_case "bucket versioning XML" `Quick
          test_bucket_versioning_xml_fixture;
        Alcotest.test_case "bucket encryption XML" `Quick
          test_bucket_encryption_xml_fixture;
        Alcotest.test_case "ListObjectsV2 XML" `Quick
          test_list_objects_v2_fixture;
        Alcotest.test_case "service error XML" `Quick test_service_error_fixture;
        Alcotest.test_case "retry decisions" `Quick test_retry_decision_fixture;
        Alcotest.test_case "CompleteMultipartUpload XML" `Quick
          test_multipart_complete_xml_fixture;
      ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-fixtures" suite
