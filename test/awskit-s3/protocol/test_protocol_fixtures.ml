open Awskit_s3

let fixture_path parts =
  List.fold_left Filename.concat "../fixtures/protocol" parts

let check_fixture label parts ~actual =
  Protocol_fixture_diff.check_file label (fixture_path parts) ~actual

let option_to_string render = function
  | None -> "none"
  | Some value -> render value

let query_to_string query = Awskit.Signing.canonical_query_params query

let header_or_empty name headers =
  match Protocol_support.header name headers with
  | None -> ""
  | Some value -> value

let test_presigned_get_fixture () =
  let options =
    Presigned.Get_object.options_exn ~expires_in:(Ptime.Span.of_int_s 900)
      ~response_content_type:"text/plain" ~version_id:"version-1" ()
  in
  let request =
    Presigned.get_object ~region:"us-east-1"
      ~credentials:Protocol_support.credentials ~now:Protocol_support.test_time
      ~bucket:"bucket" ~key:"file.txt" ~options ()
    |> Protocol_support.ok_or_fail "presigned get fixture"
  in
  let redacted_url =
    Protocol_fixture_diff.normalize_uri
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

let authorization_field_or_fail name authorization =
  match String.index_opt authorization ' ' with
  | None -> Alcotest.failf "authorization missing %s field" name
  | Some index -> (
      let fields =
        String.sub authorization (index + 1)
          (String.length authorization - index - 1)
      in
      let find_field field =
        match String.split_on_char '=' (String.trim field) with
        | [ key; value ] when String.equal key name -> Some value
        | _ -> None
      in
      fields |> String.split_on_char ',' |> List.find_map find_field |> function
      | Some value -> value
      | None -> Alcotest.failf "authorization missing %s field" name)

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
    Awskit.Signing.sign_request_params ~credentials:Protocol_support.credentials
      ~region:(Region.of_string_exn "us-east-1")
      ~service:"s3" ~method_:`PUT ~path:"/photos/cat space.jpg"
      ~query_params:
        [
          ("partNumber", [ "10" ]);
          ("uploadId", [ "upload 1" ]);
          ("X-Amz-Meta", [ "a/b"; "a b" ]);
        ]
      ~headers ~payload_hash ~now:Protocol_support.test_time
    |> Protocol_support.ok_or_fail "signing artifact fixture"
  in
  let canonical_headers =
    Awskit.Signing.canonical_headers
      (List.filter
         (fun (name, _) -> not (String.equal name "authorization"))
         signed.headers)
  in
  let authorization = header_or_empty "authorization" signed.headers in
  let signature = authorization_field_or_fail "Signature" authorization in
  let actual =
    Fmt.str
      "method=PUT\n\
       canonical-path=%s\n\
       canonical-query=%s\n\
       canonical-headers=%ssigned-headers=%s\n\
       payload-hash=%s\n\
       authorization=%s\n\
       signature=%s"
      (Protocol_wire_model.uri_encode ~encode_slash:false
         "/photos/cat space.jpg")
      (Protocol_wire_model.canonical_query
         [
           ("partNumber", [ "10" ]);
           ("uploadId", [ "upload 1" ]);
           ("X-Amz-Meta", [ "a/b"; "a b" ]);
         ])
      (Awskit.Signing.canonical_headers_block canonical_headers)
      signed.signed_headers_str
      (Awskit.Body.Payload_hash.to_header_value payload_hash)
      (redact_authorization authorization)
      signature
  in
  check_fixture "signing canonical artifact"
    [ "signing"; "put-object-canonical.expected" ]
    ~actual

let test_endpoint_resolution_fixture () =
  let region = Region.of_string_exn "us-east-1" in
  let resolved =
    Endpoint_resolver.resolve_object_request Endpoint_config.default ~region
      ~bucket:(Protocol_support.bucket_name "bucket")
      ~key:(Protocol_support.object_key "photos/cat.jpg")
    |> Protocol_support.ok_or_fail "endpoint fixture"
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

let describe_resolved_endpoint label (resolved : Endpoint_resolver.Request.t) =
  let style =
    match resolved.style with
    | `Path -> "path"
    | `Virtual_hosted -> "virtual-hosted"
  in
  Fmt.str
    "[%s]\nendpoint=%s\npath=%s\nsigning_path=%s\nsigning_region=%s\nstyle=%s"
    label
    (Awskit.Endpoint.to_url_prefix resolved.endpoint)
    resolved.path resolved.signing_path
    (Region.to_string resolved.signing_region)
    style

let resolve_object_or_fail label config ~bucket ~key =
  Endpoint_resolver.resolve_object_request config
    ~region:(Region.of_string_exn "us-east-1")
    ~bucket:(Protocol_support.bucket_name bucket)
    ~key:(Protocol_support.object_key key)
  |> Protocol_support.ok_or_fail label

let test_endpoint_style_matrix_fixture () =
  let virtual_hosted =
    resolve_object_or_fail "virtual-hosted endpoint fixture"
      Endpoint_config.default ~bucket:"bucket" ~key:"photos/cat space.jpg"
  in
  let path_style =
    resolve_object_or_fail "path-style endpoint fixture" Endpoint_config.default
      ~bucket:"bucket.example" ~key:"photos/cat space.jpg"
  in
  let actual =
    String.concat "\n\n"
      [
        describe_resolved_endpoint "virtual-hosted" virtual_hosted;
        describe_resolved_endpoint "path-style" path_style;
      ]
  in
  check_fixture "endpoint style matrix"
    [ "endpoint"; "style-matrix.txt" ]
    ~actual

let test_put_object_metadata_tags_fixture () =
  let options =
    Object.Put.options_exn ~content_type:"text/plain"
      ~metadata:
        (Metadata.of_list_exn [ ("origin", "fixture"); ("trace", "abc-123") ])
      ~tags:
        (Protocol_support.tag_set
           [
             Protocol_support.tag "env" "test";
             Protocol_support.tag "owner" "sdk";
           ])
      ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "" ]
  in
  ignore
    (Protocol_recording_runtime.S3.Object.put conn ~bucket:"bucket"
       ~key:"file.txt" ~options
       ~body:(Protocol_recording_runtime.S3.Body.of_string "body")
       ()
    |> Protocol_support.ok_or_fail "put object metadata fixture");
  let call = Protocol_recording_runtime.last_call conn in
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

let test_copy_object_headers_fixture () =
  let options =
    Object.Copy.options_exn ~source_version_id:"version 1/2"
      ~metadata_directive:
        (`Replace (Metadata.of_list_exn [ ("origin", "copy fixture") ]))
      ~checksum_algorithm:Object.Checksum.Algorithm.Sha256
      ~expected_bucket_owner:"123456789012"
      ~source_expected_bucket_owner:"210987654321" ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [
        Protocol_recording_runtime.response 200
          {|<CopyObjectResult><ETag>"copied"</ETag></CopyObjectResult>|};
      ]
  in
  ignore
    (Protocol_recording_runtime.S3.Object.copy conn
       ~source_bucket:"source-bucket" ~source_key:"photos/cat space+plus.txt"
       ~destination_bucket:"dest-bucket" ~destination_key:"archive/copy.txt"
       ~options ()
    |> Protocol_support.ok_or_fail "copy object fixture");
  let call = Protocol_recording_runtime.last_call conn in
  let request = call.request in
  let target = request.Awskit.Request.target in
  let actual =
    Fmt.str
      "method=%s\n\
       path=%s\n\
       query=%s\n\
       x-amz-copy-source=%s\n\
       x-amz-checksum-algorithm=%s\n\
       x-amz-metadata-directive=%s\n\
       x-amz-meta-origin=%s\n\
       x-amz-expected-bucket-owner=%s\n\
       x-amz-source-expected-bucket-owner=%s\n\
       body=%s"
      (Awskit.Request.Method.to_string request.method_)
      target.path
      (query_to_string target.query)
      (header_or_empty "x-amz-copy-source" request.headers)
      (header_or_empty "x-amz-checksum-algorithm" request.headers)
      (header_or_empty "x-amz-metadata-directive" request.headers)
      (header_or_empty "x-amz-meta-origin" request.headers)
      (header_or_empty "x-amz-expected-bucket-owner" request.headers)
      (header_or_empty "x-amz-source-expected-bucket-owner" request.headers)
      call.body
  in
  check_fixture "copy object headers"
    [ "object"; "copy-headers.expected" ]
    ~actual

let test_object_tagging_xml_fixture () =
  let options =
    Object.Tagging.options_exn ~expected_bucket_owner:"123456789012" ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "" ]
  in
  ignore
    (Protocol_recording_runtime.S3.Object.Tagging.put conn ~bucket:"bucket"
       ~key:"file.txt" ~options
       ~tags:
         (Protocol_support.tag_set
            [
              Protocol_support.tag "env" "test";
              Protocol_support.tag "owner" "sdk";
            ])
       ()
    |> Protocol_support.ok_or_fail "object tagging fixture");
  let call = Protocol_recording_runtime.last_call conn in
  let request = call.request in
  let target = request.Awskit.Request.target in
  let actual =
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
  in
  check_fixture "object tagging XML"
    [ "object"; "tagging-put.expected" ]
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
      ("x-amz-meta-title", "=?UTF-8?B?UmVzdW3DqQ==?=");
    ]
  in
  let options =
    Object.Get.options_exn ~range:(Range.bytes_exn ~start:2L ~finish:5L) ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response ~headers 206 "cdef" ]
  in
  let result =
    Protocol_recording_runtime.S3.Object.get conn ~bucket:"bucket"
      ~key:"file.txt" ~options
      ~consume:(Protocol_recording_runtime.S3.Reader.to_string ~max_bytes:16L)
      ()
    |> Protocol_support.ok_or_fail "range get fixture"
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
       metadata-origin=%s\n\
       metadata-title=%s"
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
      (Option.value ~default:""
         (Option.map String.escaped
            (List.assoc_opt "title" (Metadata.to_list result.metadata))))
  in
  check_fixture "range GET response" [ "object"; "range-get.expected" ] ~actual

let expect_validation_field ?(label = "validation") field = function
  | Ok _ -> Alcotest.failf "%s: expected validation error for %s" label field
  | Error error ->
      Alcotest.(check bool)
        (label ^ " kind") true
        (Awskit.Error.is_validation error);
      Alcotest.(check (option string))
        (label ^ " field") (Some field)
        (Awskit.Error.validation_field error)

let expect_validation_exn label field f =
  match f () with
  | _ -> Alcotest.failf "%s: expected validation exception for %s" label field
  | exception Awskit.Error.Awskit_error error ->
      Alcotest.(check bool)
        (label ^ " kind") true
        (Awskit.Error.is_validation error);
      Alcotest.(check (option string))
        (label ^ " field") (Some field)
        (Awskit.Error.validation_field error)
  | exception exn ->
      Alcotest.failf "%s: unexpected exception %s" label
        (Printexc.to_string exn)

let recorded_request_count conn =
  List.length conn.Protocol_recording_runtime.Runtime.calls

let expect_no_request label field f =
  let conn = Protocol_recording_runtime.connect [] in
  expect_validation_field ~label field (f conn);
  Alcotest.(check int)
    (label ^ " request count") 0
    (recorded_request_count conn)

let expect_no_credentials label field f =
  let credential_error =
    Awskit.Error.Producer.credentials "unexpected credential resolution"
  in
  let conn = Protocol_recording_runtime.connect ~credential_error [] in
  expect_validation_field ~label field (f conn);
  Alcotest.(check int)
    (label ^ " credential resolve count")
    0
    (Protocol_recording_runtime.credential_resolve_count conn)

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let request_body () = Protocol_recording_runtime.S3.Body.of_string "body"

let test_public_operation_validation_sends_no_request () =
  expect_no_request "object put invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.put_string conn ~bucket:"Invalid"
        ~key:"file.txt" ~contents:"body" ());
  expect_no_request "object get invalid key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Object.get_string conn
        ~bucket:"valid-bucket" ~key:"" ~max_bytes:16L ());
  expect_no_request "object get invalid max bytes" "max_bytes" (fun conn ->
      Protocol_recording_runtime.S3.Object.get_string conn
        ~bucket:"valid-bucket" ~key:"file.txt" ~max_bytes:(-1L) ());
  expect_no_request "object head invalid key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Object.head conn ~bucket:"valid-bucket"
        ~key:"" ());
  expect_no_request "object delete invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.delete conn ~bucket:"Invalid"
        ~key:"file.txt" ());
  let delete_member = Object.Delete_objects.object_exn ~key:"file.txt" () in
  Alcotest.(check string)
    "delete member key inspectable" "file.txt"
    (Object_key.to_string delete_member.key);
  expect_no_request "delete objects invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.delete_objects conn ~bucket:"Invalid"
        ~objects:[ delete_member ] ());
  expect_no_request "copy invalid source bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.copy conn ~source_bucket:"Invalid"
        ~source_key:"source.txt" ~destination_bucket:"dest-bucket"
        ~destination_key:"copy.txt" ());
  expect_no_request "copy invalid source key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Object.copy conn
        ~source_bucket:"source-bucket" ~source_key:""
        ~destination_bucket:"dest-bucket" ~destination_key:"copy.txt" ());
  expect_no_request "copy invalid destination bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.copy conn
        ~source_bucket:"source-bucket" ~source_key:"source.txt"
        ~destination_bucket:"Invalid" ~destination_key:"copy.txt" ());
  expect_no_request "copy invalid destination key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Object.copy conn
        ~source_bucket:"source-bucket" ~source_key:"source.txt"
        ~destination_bucket:"dest-bucket" ~destination_key:"" ());
  expect_no_request "bucket create invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Bucket.create conn ~bucket:"Invalid" ());
  expect_no_request "bucket policy invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Bucket.Policy.get conn ~bucket:"ab" ());
  expect_no_request "list invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Object.list conn ~bucket:"Invalid" ());
  expect_no_request "list pages invalid max pages" "max_pages" (fun conn ->
      Protocol_recording_runtime.S3.Object.List.pages conn
        ~bucket:"valid-bucket" ~max_pages:0 ());
  expect_no_request "version pages invalid max pages" "max_pages" (fun conn ->
      Protocol_recording_runtime.S3.Object.Versions.pages conn
        ~bucket:"valid-bucket" ~max_pages:0 ());
  expect_no_request "multipart create invalid bucket" "bucket" (fun conn ->
      Protocol_recording_runtime.S3.Multipart.create_upload conn
        ~bucket:"Invalid" ~key:"large.bin" ());
  expect_no_request "multipart create invalid key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Multipart.create_upload conn
        ~bucket:"valid-bucket" ~key:"" ());
  let upload =
    Multipart.Upload.resume_exn ~bucket:"valid-bucket" ~key:"large.bin"
      ~upload_id:"upload-1"
  in
  expect_no_request "upload part invalid part number" "part_number" (fun conn ->
      Protocol_recording_runtime.S3.Multipart.upload_part conn ~upload
        ~part_number:0 ~body:(request_body ()) ());
  expect_no_request "complete upload empty parts" "parts" (fun conn ->
      Protocol_recording_runtime.S3.Multipart.complete_upload conn ~upload
        ~parts:[] ());
  expect_no_request "list parts invalid max pages" "max_pages" (fun conn ->
      Protocol_recording_runtime.S3.Multipart.List_parts.parts conn ~upload
        ~max_pages:0 ())

let test_public_option_builder_validation () =
  let create_options =
    Bucket.Create.options ~region:"eu-west-1" ()
    |> Protocol_support.ok_or_fail "create options region"
  in
  Alcotest.(check (option string))
    "create region" (Some "eu-west-1")
    (Option.map Region.to_string create_options.Bucket.Create.region);
  expect_validation_field ~label:"create options region" "region"
    (Bucket.Create.options ~region:"" ());
  expect_validation_field ~label:"put content type" "content_type"
    (Object.Put.options ~content_type:"" ());
  expect_validation_field ~label:"put header value" "cache_control"
    (Object.Put.options ~cache_control:"" ());
  expect_validation_field ~label:"put expected owner" "account_id"
    (Object.Put.options ~expected_bucket_owner:"123" ());
  let dsse_bucket_key =
    Encryption.Kms.create_exn ~bucket_key_enabled:true () |> fun kms ->
    Encryption.Destination.Dsse_kms kms
  in
  expect_validation_field ~label:"put dsse bucket key" "sse_bucket_key_enabled"
    (Object.Put.options ~encryption:dsse_bucket_key ());
  expect_validation_exn "put dsse bucket key exn" "sse_bucket_key_enabled"
    (fun () -> Object.Put.options_exn ~encryption:dsse_bucket_key ());
  expect_validation_field ~label:"get version id" "version_id"
    (Object.Get.options ~version_id:"" ());
  expect_validation_field ~label:"head expected owner" "account_id"
    (Object.Head.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"delete version id" "version_id"
    (Object.Delete.options ~version_id:"bad\nversion" ());
  expect_validation_field ~label:"delete object key" "key"
    (Object.Delete_objects.object_ ~key:"" ());
  expect_validation_field ~label:"delete object version" "version_id"
    (Object.Delete_objects.object_ ~key:"file.txt" ~version_id:"" ());
  expect_validation_field ~label:"delete object etag" "etag"
    (Object.Delete_objects.object_ ~key:"file.txt" ~etag:"" ());
  expect_validation_field ~label:"copy source version" "version_id"
    (Object.Copy.options ~source_version_id:"" ());
  expect_validation_field ~label:"copy expected owner" "account_id"
    (Object.Copy.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"copy dsse bucket key" "sse_bucket_key_enabled"
    (Object.Copy.options ~destination_encryption:dsse_bucket_key ());
  expect_validation_exn "copy dsse bucket key exn" "sse_bucket_key_enabled"
    (fun () ->
      Object.Copy.options_exn ~destination_encryption:dsse_bucket_key ());
  expect_validation_field ~label:"list prefix" "prefix"
    (Object.List.options ~prefix:"" ());
  expect_validation_field ~label:"list delimiter" "delimiter"
    (Object.List.options ~delimiter:"" ());
  expect_validation_field ~label:"list max keys" "max_keys"
    (Object.List.options ~max_keys:0 ());
  expect_validation_field ~label:"list start after" "key"
    (Object.List.options ~start_after:"" ());
  expect_validation_field ~label:"list continuation token" "<redacted>"
    (Object.List.options ~continuation_token:"" ());
  expect_validation_field ~label:"list expected owner" "account_id"
    (Object.List.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"versions prefix" "prefix"
    (Object.Versions.options ~prefix:"" ());
  expect_validation_field ~label:"versions delimiter" "delimiter"
    (Object.Versions.options ~delimiter:"" ());
  expect_validation_field ~label:"versions max keys" "max_keys"
    (Object.Versions.options ~max_keys:1001 ());
  expect_validation_field ~label:"versions key marker" "key"
    (Object.Versions.options ~key_marker:"" ());
  expect_validation_field ~label:"versions id marker" "version_id"
    (Object.Versions.options ~version_id_marker:"" ());
  expect_validation_field ~label:"upload resume id" "upload_id"
    (Multipart.Upload.resume ~bucket:"valid-bucket" ~key:"large.bin"
       ~upload_id:"");
  expect_validation_field ~label:"create multipart content type" "content_type"
    (Multipart.Create.options ~content_type:"" ());
  expect_validation_field ~label:"create multipart owner" "account_id"
    (Multipart.Create.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"create multipart dsse bucket key"
    "sse_bucket_key_enabled"
    (Multipart.Create.options ~encryption:dsse_bucket_key ());
  expect_validation_exn "create multipart dsse bucket key exn"
    "sse_bucket_key_enabled" (fun () ->
      Multipart.Create.options_exn ~encryption:dsse_bucket_key ());
  expect_validation_field ~label:"upload part owner" "account_id"
    (Multipart.Upload_part.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"complete object size" "multipart_object_size"
    (Multipart.Complete.options ~multipart_object_size:(-1L) ());
  expect_validation_field ~label:"list parts max parts" "max_parts"
    (Multipart.List_parts.options ~max_parts:0 ());
  expect_validation_field ~label:"list parts marker" "part_number_marker"
    (Multipart.List_parts.options ~part_number_marker:(-1) ());
  expect_validation_field ~label:"list parts owner" "account_id"
    (Multipart.List_parts.options ~expected_bucket_owner:"123" ())

let endpoint_scheme_to_string = function `Http -> "http" | `Https -> "https"

let check_endpoint_config label config ~scheme ~host ~signing_region =
  let client_region = Region.of_string_exn "us-east-1" in
  let endpoint =
    Endpoint_config.endpoint config ~region:client_region
    |> Protocol_support.ok_or_fail (label ^ " endpoint")
  in
  Alcotest.(check string)
    (label ^ " scheme") scheme
    (endpoint_scheme_to_string (Awskit.Endpoint.scheme endpoint));
  Alcotest.(check string) (label ^ " host") host (Awskit.Endpoint.host endpoint);
  Alcotest.(check string)
    (label ^ " signing region")
    signing_region
    (Endpoint_config.signing_region config ~client_region |> Region.to_string)

let test_endpoint_config_public_seams () =
  let https_config =
    Endpoint_config.s3_compatible ~endpoint:"https://minio.example.com"
      ~signing_region:"us-west-2" ~addressing_style:`Path
      ~tls_policy:`Https_required ~feature_policy:`S3_compatible ()
    |> Protocol_support.ok_or_fail "https endpoint config"
  in
  check_endpoint_config "https endpoint" https_config ~scheme:"https"
    ~host:"minio.example.com" ~signing_region:"us-west-2";
  let local_config =
    Endpoint_config.local_plaintext ~endpoint:"http://127.0.0.1:9000"
      ~signing_region:"us-east-1" ~addressing_style:`Path ()
    |> Protocol_support.ok_or_fail "local plaintext endpoint config"
  in
  check_endpoint_config "local plaintext endpoint" local_config ~scheme:"http"
    ~host:"127.0.0.1" ~signing_region:"us-east-1";
  expect_validation_field ~label:"endpoint path rejected" "endpoint"
    (Endpoint_config.s3_compatible ~endpoint:"https://minio.example.com/bucket"
       ~signing_region:"us-east-1" ~addressing_style:`Path
       ~tls_policy:`Https_required ~feature_policy:`S3_compatible ());
  expect_validation_field ~label:"endpoint signing region rejected" "region"
    (Endpoint_config.s3_compatible ~endpoint:"https://minio.example.com"
       ~signing_region:"" ~addressing_style:`Path ~tls_policy:`Https_required
       ~feature_policy:`S3_compatible ());
  expect_validation_field ~label:"https required rejects http" "tls_policy"
    (Endpoint_config.s3_compatible ~endpoint:"http://minio.example.com"
       ~signing_region:"us-east-1" ~addressing_style:`Path
       ~tls_policy:`Https_required ~feature_policy:`S3_compatible ());
  expect_validation_field ~label:"local plaintext rejects remote" "endpoint"
    (Endpoint_config.local_plaintext ~endpoint:"http://minio.example.com"
       ~signing_region:"us-east-1" ~addressing_style:`Path ());
  let unsafe_config =
    Endpoint_config.unsafe_plaintext ~endpoint:"http://minio.example.com:9000"
      ~signing_region:"us-west-2" ~addressing_style:`Path ()
    |> Protocol_support.ok_or_fail "unsafe plaintext endpoint config"
  in
  check_endpoint_config "unsafe plaintext endpoint" unsafe_config ~scheme:"http"
    ~host:"minio.example.com" ~signing_region:"us-west-2";
  Alcotest.(check bool)
    "unsafe feature policy" true
    (Endpoint_config.feature_policy unsafe_config = `S3_compatible);
  expect_validation_field ~label:"unsafe signing region rejected" "region"
    (Endpoint_config.unsafe_plaintext ~endpoint:"http://minio.example.com"
       ~signing_region:"" ~addressing_style:`Path ())

let test_presigned_public_seams () =
  let put_options =
    Presigned.Put_object.options_exn ~content_type:"text/plain"
      ~expected_bucket_owner:"123456789012"
      ~extra_signed_headers:[ ("x-amz-meta-trace", "abc") ]
      ()
  in
  let presigned =
    Presigned.put_object ~region:"us-east-1"
      ~credentials:Protocol_support.credentials ~now:Protocol_support.test_time
      ~bucket:"bucket" ~key:"file.txt" ~options:put_options ()
    |> Protocol_support.ok_or_fail "presigned put success"
  in
  Alcotest.(check string)
    "presigned method" "PUT"
    (method_to_string (Presigned.method_ presigned));
  Alcotest.(check string)
    "presigned content type" "text/plain"
    (header_or_empty "content-type" (Presigned.request_headers presigned));
  Alcotest.(check string)
    "presigned extra header" "abc"
    (header_or_empty "x-amz-meta-trace" (Presigned.request_headers presigned));
  Alcotest.(check bool)
    "host is signed" true
    (List.mem_assoc "host" (Presigned.signed_headers presigned));
  expect_validation_field ~label:"presign region" "region"
    (Presigned.get_object ~region:"" ~credentials:Protocol_support.credentials
       ~now:Protocol_support.test_time ~bucket:"bucket" ~key:"file.txt" ());
  expect_validation_field ~label:"presign bucket" "bucket"
    (Presigned.get_object ~region:"us-east-1"
       ~credentials:Protocol_support.credentials ~now:Protocol_support.test_time
       ~bucket:"Invalid" ~key:"file.txt" ());
  expect_validation_field ~label:"presign key" "key"
    (Presigned.get_object ~region:"us-east-1"
       ~credentials:Protocol_support.credentials ~now:Protocol_support.test_time
       ~bucket:"bucket" ~key:"" ());
  expect_validation_field ~label:"presign content type" "content_type"
    (Presigned.Put_object.options ~content_type:"" ());
  expect_validation_field ~label:"presign response content type" "content_type"
    (Presigned.Get_object.options ~response_content_type:"" ());
  expect_validation_field ~label:"presign response disposition"
    "response_content_disposition"
    (Presigned.Get_object.options ~response_content_disposition:"" ());
  expect_validation_field ~label:"presign version id" "version_id"
    (Presigned.Get_object.options ~version_id:"" ());
  expect_validation_field ~label:"presign owner" "account_id"
    (Presigned.Delete_object.options ~expected_bucket_owner:"123" ());
  expect_validation_field ~label:"presign bad header name" "header"
    (Presigned.Get_object.options
       ~extra_signed_headers:[ ("bad\nname", "value") ]
       ());
  expect_validation_field ~label:"presign bad header value" "header"
    (Presigned.Get_object.options
       ~extra_signed_headers:[ ("x-test", "bad\nvalue") ]
       ());
  expect_validation_field ~label:"presign duplicate header" "header"
    (Presigned.Get_object.options
       ~extra_signed_headers:[ ("x-test", "one"); ("x-test", "two") ]
       ());
  expect_validation_field ~label:"presign duplicate case header" "header"
    (Presigned.Get_object.options
       ~extra_signed_headers:[ ("x-test", "one"); ("X-Test", "two") ]
       ());
  expect_validation_field ~label:"presign duplicate content type" "header"
    (Presigned.Put_object.options ~content_type:"text/plain"
       ~extra_signed_headers:[ ("Content-Type", "text/plain") ]
       ());
  let upload =
    Multipart.Upload.resume_exn ~bucket:"bucket" ~key:"large.bin"
      ~upload_id:"upload-1"
  in
  expect_validation_field ~label:"presign upload part number" "part_number"
    (Presigned.upload_part ~region:"us-east-1"
       ~credentials:Protocol_support.credentials ~now:Protocol_support.test_time
       ~upload ~part_number:0 ())

let test_presigned_runtime_validation_skips_credentials () =
  expect_no_credentials "runtime presign get invalid bucket" "bucket"
    (fun conn ->
      Protocol_recording_runtime.S3.Presigned.get_object conn ~bucket:"Invalid"
        ~key:"file.txt" ());
  expect_no_credentials "runtime presign put invalid key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Presigned.put_object conn
        ~bucket:"valid-bucket" ~key:"" ());
  expect_no_credentials "runtime presign head invalid bucket" "bucket"
    (fun conn ->
      Protocol_recording_runtime.S3.Presigned.head_object conn ~bucket:"Invalid"
        ~key:"file.txt" ());
  expect_no_credentials "runtime presign delete invalid key" "key" (fun conn ->
      Protocol_recording_runtime.S3.Presigned.delete_object conn
        ~bucket:"valid-bucket" ~key:"" ());
  let upload =
    Multipart.Upload.resume_exn ~bucket:"valid-bucket" ~key:"large.bin"
      ~upload_id:"upload-1"
  in
  expect_no_credentials "runtime presign upload invalid part number"
    "part_number" (fun conn ->
      Protocol_recording_runtime.S3.Presigned.upload_part conn ~upload
        ~part_number:0 ())

let describe_request (call : Protocol_recording_runtime.call) =
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
    Bucket.Versioning.options_exn ~expected_bucket_owner:"123456789012" ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "" ]
  in
  ignore
    (Protocol_recording_runtime.S3.Bucket.Versioning.put conn
       ~bucket:"my-bucket" ~options ~status:Bucket.Versioning.Status.Enabled ()
    |> Protocol_support.ok_or_fail "bucket versioning XML fixture");
  check_fixture "bucket versioning XML"
    [ "bucket"; "versioning-put.expected" ]
    ~actual:(describe_request (Protocol_recording_runtime.last_call conn))

let test_bucket_encryption_xml_fixture () =
  let config =
    {
      Bucket.Encryption.rules =
        [
          {
            Bucket.Encryption.Rule.sse_algorithm =
              Some Bucket.Encryption.Algorithm.Aws_kms;
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
    Bucket.Encryption.options_exn ~expected_bucket_owner:"123456789012" ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "" ]
  in
  ignore
    (Protocol_recording_runtime.S3.Bucket.Encryption.put conn
       ~bucket:"my-bucket" ~options ~config ()
    |> Protocol_support.ok_or_fail "bucket encryption XML fixture");
  check_fixture "bucket encryption XML"
    [ "bucket"; "encryption-put.expected" ]
    ~actual:(describe_request (Protocol_recording_runtime.last_call conn))

let test_bucket_encryption_rejects_dsse_bucket_key () =
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
            blocked_encryption_types = [];
          };
        ];
    }
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "" ]
  in
  let result =
    Protocol_recording_runtime.S3.Bucket.Encryption.put conn ~bucket:"my-bucket"
      ~config ()
  in
  expect_validation_field "bucket_key_enabled" result;
  Alcotest.(check int)
    "no request sent" 0
    (List.length conn.Protocol_recording_runtime.Runtime.calls)

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
    Protocol_fixture_diff.read_file
      (fixture_path [ "pagination"; "list-v2.xml" ])
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 body ]
  in
  let page =
    Protocol_recording_runtime.S3.Object.list conn ~bucket:"my-bucket" ()
    |> Protocol_support.ok_or_fail "list objects fixture"
  in
  check_fixture "list objects v2 summary"
    [ "pagination"; "list-v2.expected" ]
    ~actual:(describe_list_page page)

let test_service_error_fixture () =
  let body =
    Protocol_fixture_diff.read_file
      (fixture_path [ "service-errors"; "slow-down.xml" ])
  in
  let conn =
    Protocol_recording_runtime.connect ~retry_policy:Awskit.Retry.disabled
      [ Protocol_recording_runtime.response 503 body ]
  in
  match
    Protocol_recording_runtime.S3.Object.put conn ~bucket:"my-bucket"
      ~key:"file.txt"
      ~body:(Protocol_recording_runtime.S3.Body.of_string "body")
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

let test_delete_objects_cr_xml_fixture () =
  let objects =
    [
      Object.Delete_objects.object_ ~key:"line\rbreak" ()
      |> Protocol_support.ok_or_fail "delete object";
    ]
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 "<DeleteResult />" ]
  in
  ignore
    (Protocol_recording_runtime.S3.Object.delete_objects conn ~bucket:"bucket"
       ~objects ()
    |> Protocol_support.ok_or_fail "delete objects CR fixture");
  check_fixture "DeleteObjects CR XML"
    [ "object"; "delete-cr.expected" ]
    ~actual:(describe_request (Protocol_recording_runtime.last_call conn))

let test_delete_objects_error_preserves_version_id () =
  let body =
    {|<DeleteResult><Error><Key>locked.txt</Key><VersionId>version-1</VersionId><Code>AccessDenied</Code><Message>denied</Message></Error></DeleteResult>|}
  in
  let objects =
    [
      Object.Delete_objects.object_ ~key:"locked.txt" ()
      |> Protocol_support.ok_or_fail "delete object";
    ]
  in
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 body ]
  in
  let result =
    Protocol_recording_runtime.S3.Object.delete_objects conn ~bucket:"bucket"
      ~objects ()
    |> Protocol_support.ok_or_fail "delete objects error version"
  in
  match result.errors with
  | [ error ] ->
      Alcotest.(check string)
        "error key" "locked.txt"
        (Object_key.to_string error.key);
      Alcotest.(check (option string))
        "error version id" (Some "version-1")
        (Option.map Object.Version_id.to_string error.version_id);
      Alcotest.(check string) "error code" "AccessDenied" error.code;
      Alcotest.(check (option string))
        "error message" (Some "denied") error.message
  | errors ->
      Alcotest.failf "expected one item error, got %d" (List.length errors)

let checksum value : Object.Checksum.value =
  Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256 ~value

let test_multipart_complete_xml_fixture () =
  let upload =
    Multipart.Upload.resume_exn ~bucket:"my-bucket" ~key:"large.bin"
      ~upload_id:"upload-1"
  in
  let part number checksum_value =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn number)
      ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" number))
      ~checksum:(checksum checksum_value) ()
  in
  let conn =
    Protocol_recording_runtime.connect
      [
        Protocol_recording_runtime.response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  ignore
    (Protocol_recording_runtime.S3.Multipart.complete_upload conn ~upload
       ~parts:[ part 1 "sha256-part-1"; part 2 "sha256-part-2" ]
       ()
    |> Protocol_support.ok_or_fail "complete multipart fixture");
  let body = (Protocol_recording_runtime.last_call conn).body in
  check_fixture "complete multipart XML"
    [ "multipart"; "complete.xml" ]
    ~actual:body

let suite =
  [
    ( "fixture:awskit-s3:protocol-wire",
      [
        Alcotest.test_case "presigned GET" `Quick test_presigned_get_fixture;
        Alcotest.test_case "endpoint resolution" `Quick
          test_endpoint_resolution_fixture;
        Alcotest.test_case "endpoint style matrix" `Quick
          test_endpoint_style_matrix_fixture;
        Alcotest.test_case "PUT metadata/tags" `Quick
          test_put_object_metadata_tags_fixture;
        Alcotest.test_case "CopyObject headers" `Quick
          test_copy_object_headers_fixture;
        Alcotest.test_case "Object tagging XML" `Quick
          test_object_tagging_xml_fixture;
        Alcotest.test_case "range GET response" `Quick test_range_get_fixture;
        Alcotest.test_case "public operation validation sends no request" `Quick
          test_public_operation_validation_sends_no_request;
        Alcotest.test_case "public option builder validation" `Quick
          test_public_option_builder_validation;
        Alcotest.test_case "endpoint config public seams" `Quick
          test_endpoint_config_public_seams;
        Alcotest.test_case "presigned public seams" `Quick
          test_presigned_public_seams;
        Alcotest.test_case "presigned runtime validation skips credentials"
          `Quick test_presigned_runtime_validation_skips_credentials;
        Alcotest.test_case "signing canonical artifact" `Quick
          test_signing_artifact_fixture;
        Alcotest.test_case "bucket versioning XML" `Quick
          test_bucket_versioning_xml_fixture;
        Alcotest.test_case "bucket encryption XML" `Quick
          test_bucket_encryption_xml_fixture;
        Alcotest.test_case "bucket encryption rejects DSSE bucket key" `Quick
          test_bucket_encryption_rejects_dsse_bucket_key;
        Alcotest.test_case "ListObjectsV2 XML" `Quick
          test_list_objects_v2_fixture;
        Alcotest.test_case "service error XML" `Quick test_service_error_fixture;
        Alcotest.test_case "DeleteObjects CR XML" `Quick
          test_delete_objects_cr_xml_fixture;
        Alcotest.test_case "DeleteObjects error VersionId" `Quick
          test_delete_objects_error_preserves_version_id;
        Alcotest.test_case "CompleteMultipartUpload XML" `Quick
          test_multipart_complete_xml_fixture;
      ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-fixtures" suite
