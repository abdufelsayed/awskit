open Awskit_s3
open Awskit_s3_test

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let span_seconds label span =
  match Ptime.Span.to_int_s span with
  | Some seconds -> seconds
  | None -> Alcotest.failf "%s: span is not representable as seconds" label

let span_float_exn seconds =
  match Ptime.Span.of_float_s seconds with
  | Some span -> span
  | None -> Alcotest.failf "invalid test span %f" seconds

let add_span label time span =
  match Ptime.add_span time span with
  | Some time -> time
  | None -> Alcotest.failf "%s: timestamp outside supported range" label

let contains_string expected = List.exists (String.equal expected)

let multipart_upload ?(bucket = bucket_name "bucket")
    ?(key = object_key "large.bin")
    ?(upload_id = Multipart.Upload_id.of_string_exn "upload-1") () =
  Multipart.Upload.resume ~bucket ~key ~upload_id

let temporary_credentials ?session_token expires_at =
  Awskit.Credentials.create_exn ~access_key_id:"AKIA_TEST_TEMP"
    ~secret_access_key:"temp-secret" ?session_token ~expires_at ()

let test_presigned_result () =
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ()
    |> ok_or_fail "presigned get"
  in
  Alcotest.(check string)
    "method" "GET"
    (Awskit.Request.Method.to_string
       (Presigned.method_ result :> Awskit.Request.Method.t));
  Alcotest.(check bool)
    "has signature" true
    (let url = Presigned.reveal_url result in
     String.contains url '?'
     && String.contains url '='
     && String.contains url '&');
  Alcotest.(check bool)
    "virtual-hosted URL" true
    (String.starts_with ~prefix:"https://bucket.s3.us-east-1.amazonaws.com/"
       (Presigned.reveal_url result))

let test_presigned_safe_artifact_redacts_bearer_material () =
  let token = "SESSION-TOKEN-SENTINEL" in
  let header_secret = "signed-header-secret-sentinel" in
  let expires_in = Ptime.Span.of_int_s 900 in
  let version_id = Object.Version_id.of_string_exn "version-1" in
  let credentials =
    temporary_credentials ~session_token:token
      (add_span "credential expiration" test_time (Ptime.Span.of_int_s 3600))
  in
  let options : Presigned.Get_object.options =
    {
      expires_in = Some expires_in;
      response_content_type = Some (content_type "text/plain");
      response_content_disposition =
        Some (header_value ~field:"response-content-disposition" "attachment");
      version_id = Some version_id;
      expected_bucket_owner = Some (account_id "123456789012");
      extra_signed_headers = [ ("x-user-secret", header_secret) ];
    }
  in
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned get safe artifact"
  in
  let raw_url = Presigned.reveal_url result in
  let safe_uri = Presigned.safe_uri result in
  let safe_text = Uri.to_string safe_uri in
  Alcotest.(check bool)
    "raw URL still includes bearer signature" true
    (String.contains raw_url '?'
    && query_param "X-Amz-Signature" raw_url <> None);
  List.iter
    (fun param ->
      Alcotest.(check (option (list string)))
        ("safe uri omits " ^ param)
        None
        (Uri.query safe_uri |> List.assoc_opt param))
    [
      "X-Amz-Algorithm";
      "X-Amz-Credential";
      "X-Amz-Date";
      "X-Amz-Expires";
      "X-Amz-SignedHeaders";
      "X-Amz-Signature";
      "X-Amz-Security-Token";
    ];
  Alcotest.(check (option (list string)))
    "safe uri preserves response content type" (Some [ "text/plain" ])
    (Uri.query safe_uri |> List.assoc_opt "response-content-type");
  Alcotest.(check (option (list string)))
    "safe uri preserves version id" (Some [ "version-1" ])
    (Uri.query safe_uri |> List.assoc_opt "versionId");
  let rendered = Format.asprintf "%a" Presigned.pp result in
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        ("public rendering omits " ^ secret)
        false
        (string_contains rendered ~substring:secret))
    [ raw_url; "X-Amz-Signature"; "X-Amz-Credential"; token; header_secret ];
  Alcotest.(check bool)
    "public rendering includes safe uri" true
    (string_contains rendered ~substring:safe_text);
  Alcotest.(check (list string))
    "signed header names"
    [ "host"; "x-amz-expected-bucket-owner"; "x-user-secret" ]
    (Presigned.signed_headers result |> List.map fst);
  Alcotest.(check (list string))
    "explicit request header names"
    [ "x-amz-expected-bucket-owner"; "x-user-secret" ]
    (Presigned.request_headers result |> List.map fst);
  Alcotest.(check int)
    "requested expiry" 900
    (span_seconds "requested" (Presigned.requested_expires_in result));
  Alcotest.(check int)
    "effective expiry" 900
    (span_seconds "effective" (Presigned.effective_expires_in result));
  Alcotest.(check (option string))
    "expiration timestamp"
    (Some (add_span "result expiration" test_time expires_in |> Ptime.to_rfc3339))
    (Presigned.expires_at result |> Option.map Ptime.to_rfc3339)

let test_client_bound_presigned_uses_runtime_model () =
  let endpoint_config =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9000 ())
      ~signing_region:(Awskit.Region.of_string_exn "eu-west-1")
      ~addressing_style:`Path ()
    |> ok_or_fail "client-bound endpoint config"
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKIA_CLIENT_BOUND"
      ~secret_access_key:"client-bound-secret" ()
  in
  let conn =
    Recording_runtime.connect ~endpoint_config
      ~region:(Awskit.Region.of_string_exn "ap-southeast-2")
      ~credentials []
  in
  let expires_in = Ptime.Span.of_int_s 600 in
  let options : Presigned.Get_object.options =
    {
      Presigned.Get_object.default_options with
      expires_in = Some expires_in;
      expected_bucket_owner = Some (account_id "123456789012");
      extra_signed_headers = [ ("x-client-context", "present") ];
    }
  in
  let result =
    Recording_s3.Presigned.get_object conn ~bucket:(bucket_name "bucket")
      ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "client-bound presigned get"
  in
  Alcotest.(check string)
    "method" "GET"
    (Awskit.Request.Method.to_string
       (Presigned.method_ result :> Awskit.Request.Method.t));
  Alcotest.(check bool)
    "raw URL has bearer signature" true
    (query_param "X-Amz-Signature" (Presigned.reveal_url result) <> None);
  Alcotest.(check bool)
    "uses connection endpoint" true
    (String.starts_with ~prefix:"http://127.0.0.1:9000/bucket/file.txt"
       (Presigned.reveal_url result));
  let datestamp, _amz_date = Awskit.Signing.ptime_to_date_time test_time in
  Alcotest.(check (option (list string)))
    "uses connection credentials and signing region"
    (Some [ Fmt.str "AKIA_CLIENT_BOUND/%s/eu-west-1/s3/aws4_request" datestamp ])
    (query_param "X-Amz-Credential" (Presigned.reveal_url result));
  Alcotest.(check (option (list string)))
    "safe uri omits signature" None
    (Uri.query (Presigned.safe_uri result) |> List.assoc_opt "X-Amz-Signature");
  Alcotest.(check (list string))
    "signed header names"
    [ "host"; "x-amz-expected-bucket-owner"; "x-client-context" ]
    (Presigned.signed_headers result |> List.map fst);
  Alcotest.(check (list string))
    "explicit request header names"
    [ "x-amz-expected-bucket-owner"; "x-client-context" ]
    (Presigned.request_headers result |> List.map fst);
  Alcotest.(check int)
    "requested expiry" 600
    (span_seconds "requested" (Presigned.requested_expires_in result));
  Alcotest.(check int)
    "effective expiry" 600
    (span_seconds "effective" (Presigned.effective_expires_in result));
  Alcotest.(check (option string))
    "expiration timestamp"
    (Some
       (add_span "client-bound expiration" test_time expires_in
       |> Ptime.to_rfc3339))
    (Presigned.expires_at result |> Option.map Ptime.to_rfc3339);
  Alcotest.(check int) "transport calls" 0 (List.length conn.calls)

let test_presigned_head_uses_dedicated_options () =
  let options : Presigned.Head_object.options =
    {
      expires_in = Some (Ptime.Span.of_int_s 300);
      response_content_type = Some (content_type "text/plain");
      response_content_disposition =
        Some (header_value ~field:"response-content-disposition" "attachment");
      version_id = Some (Object.Version_id.of_string_exn "head-version");
      expected_bucket_owner = Some (account_id "123456789012");
      extra_signed_headers = [ ("x-head-context", "present") ];
    }
  in
  let result =
    Presigned.head_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned head dedicated options"
  in
  Alcotest.(check string)
    "method" "HEAD"
    (Awskit.Request.Method.to_string
       (Presigned.method_ result :> Awskit.Request.Method.t));
  Alcotest.(check (option (list string)))
    "head response content type override" (Some [ "text/plain" ])
    (query_param "response-content-type" (Presigned.reveal_url result));
  Alcotest.(check (option (list string)))
    "head response content disposition override" (Some [ "attachment" ])
    (query_param "response-content-disposition" (Presigned.reveal_url result));
  Alcotest.(check (option (list string)))
    "head version id" (Some [ "head-version" ])
    (query_param "versionId" (Presigned.reveal_url result));
  Alcotest.(check (list string))
    "signed header names"
    [ "host"; "x-amz-expected-bucket-owner"; "x-head-context" ]
    (Presigned.signed_headers result |> List.map fst);
  Alcotest.(check (list string))
    "explicit request header names"
    [ "x-amz-expected-bucket-owner"; "x-head-context" ]
    (Presigned.request_headers result |> List.map fst);
  Alcotest.(check (option (list string)))
    "head signed URL includes host header"
    (Some [ "host;x-amz-expected-bucket-owner;x-head-context" ])
    (query_param "X-Amz-SignedHeaders" (Presigned.reveal_url result))

let test_presigned_effective_expiry_is_capped_by_credentials () =
  let requested = Ptime.Span.of_int_s 3600 in
  let credential_lifetime = Ptime.Span.of_int_s 600 in
  let expires_at =
    add_span "credential expires_at" test_time credential_lifetime
  in
  let credentials = temporary_credentials expires_at in
  let options =
    { Presigned.Get_object.default_options with expires_in = Some requested }
  in
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned get capped expiration"
  in
  Alcotest.(check int)
    "requested expiry" 3600
    (span_seconds "requested" (Presigned.requested_expires_in result));
  Alcotest.(check int)
    "effective expiry" 600
    (span_seconds "effective" (Presigned.effective_expires_in result));
  Alcotest.(check (option string))
    "expires at credential expiration"
    (Some (Ptime.to_rfc3339 expires_at))
    (Presigned.expires_at result |> Option.map Ptime.to_rfc3339);
  Alcotest.(check (option (list string)))
    "signed URL uses capped expiration" (Some [ "600" ])
    (query_param "X-Amz-Expires" (Presigned.reveal_url result))

let test_presigned_rejects_expired_credentials () =
  let credentials =
    temporary_credentials
      (add_span "expired credential timestamp" test_time
         (Ptime.Span.of_int_s (-1)))
  in
  match
    Presigned.get_object ~region:"us-east-1" ~credentials ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ()
  with
  | Error error when Awskit.Error.is_credentials error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected expired credentials error"

let test_presigned_expiry_validation_boundaries () =
  let expect_invalid label expires_in =
    let options =
      { Presigned.Get_object.default_options with expires_in = Some expires_in }
    in
    match
      Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
        ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    with
    | Error error when is_validation_field "expires_in" error -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected invalid expiry" label
  in
  expect_invalid "zero" (Ptime.Span.of_int_s 0);
  expect_invalid "negative" (Ptime.Span.of_int_s (-1));
  expect_invalid "sub-second" (span_float_exn 0.5);
  expect_invalid "over max by fraction" (span_float_exn 604800.5);
  let fractional_options =
    {
      Presigned.Get_object.default_options with
      expires_in = Some (span_float_exn 1.5);
    }
  in
  let fractional =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:fractional_options ()
    |> ok_or_fail "fractional expiry"
  in
  Alcotest.(check (float 0.000001))
    "requested fractional expiry" 1.5
    (Ptime.Span.to_float_s (Presigned.requested_expires_in fractional));
  let rendered = Format.asprintf "%a" Presigned.pp fractional in
  Alcotest.(check bool)
    "printer preserves fractional requested expiry" true
    (string_contains rendered ~substring:"1.5");
  Alcotest.(check int)
    "effective fractional expiry floors" 1
    (span_seconds "fractional effective"
       (Presigned.effective_expires_in fractional));
  Alcotest.(check (option (list string)))
    "signed fractional expiry floors" (Some [ "1" ])
    (query_param "X-Amz-Expires" (Presigned.reveal_url fractional));
  let max_options =
    {
      Presigned.Get_object.default_options with
      expires_in = Some (Ptime.Span.of_int_s 604800);
    }
  in
  let max_result =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:max_options ()
    |> ok_or_fail "max expiry"
  in
  Alcotest.(check int)
    "max effective expiry" 604800
    (span_seconds "max effective" (Presigned.effective_expires_in max_result))

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
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned put"
  in
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha1")
    (header "x-amz-checksum-sha1" (Presigned.signed_headers result));
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" (Presigned.signed_headers result));
  let signed_headers = signed_headers_or_fail (Presigned.reveal_url result) in
  Alcotest.(check bool)
    "signed checksum value" true
    (contains_string "x-amz-checksum-sha1" signed_headers)

let test_presigned_expected_bucket_owner_headers () =
  let owner = account_id "123456789012" in
  let owner_string = Account_id.to_string owner in
  let get_options =
    {
      Presigned.Get_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let get =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:get_options ()
    |> ok_or_fail "presigned get expected owner"
  in
  Alcotest.(check (option string))
    "get expected owner header" (Some owner_string)
    (header "x-amz-expected-bucket-owner" (Presigned.signed_headers get));
  Alcotest.(check bool)
    "get signed expected owner" true
    (contains_string "x-amz-expected-bucket-owner"
       (signed_headers_or_fail (Presigned.reveal_url get)));
  let head_options =
    {
      Presigned.Head_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let head =
    Presigned.head_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:head_options ()
    |> ok_or_fail "presigned head expected owner"
  in
  Alcotest.(check (option string))
    "head expected owner header" (Some owner_string)
    (header "x-amz-expected-bucket-owner" (Presigned.signed_headers head));
  Alcotest.(check bool)
    "head signed expected owner" true
    (contains_string "x-amz-expected-bucket-owner"
       (signed_headers_or_fail (Presigned.reveal_url head)));
  let put_options =
    {
      Presigned.Put_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let put =
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:put_options ()
    |> ok_or_fail "presigned put expected owner"
  in
  Alcotest.(check (option string))
    "put expected owner header" (Some owner_string)
    (header "x-amz-expected-bucket-owner" (Presigned.signed_headers put));
  Alcotest.(check bool)
    "put signed expected owner" true
    (contains_string "x-amz-expected-bucket-owner"
       (signed_headers_or_fail (Presigned.reveal_url put)));
  let upload = multipart_upload () in
  let upload_part_options =
    {
      Presigned.Upload_part.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let upload_part =
    Presigned.upload_part ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~options:upload_part_options ()
    |> ok_or_fail "presigned upload-part expected owner"
  in
  Alcotest.(check (option string))
    "upload-part expected owner header" (Some owner_string)
    (header "x-amz-expected-bucket-owner"
       (Presigned.signed_headers upload_part));
  Alcotest.(check bool)
    "upload-part signed expected owner" true
    (contains_string "x-amz-expected-bucket-owner"
       (signed_headers_or_fail (Presigned.reveal_url upload_part)));
  let delete_options =
    {
      Presigned.Delete_object.default_options with
      expected_bucket_owner = Some owner;
    }
  in
  let delete =
    Presigned.delete_object ~region:"us-east-1" ~credentials:creds
      ~now:test_time ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
      ~options:delete_options ()
    |> ok_or_fail "presigned delete expected owner"
  in
  Alcotest.(check (option string))
    "delete expected owner header" (Some owner_string)
    (header "x-amz-expected-bucket-owner" (Presigned.signed_headers delete));
  Alcotest.(check bool)
    "delete signed expected owner" true
    (contains_string "x-amz-expected-bucket-owner"
       (signed_headers_or_fail (Presigned.reveal_url delete)))

let test_presigned_upload_part () =
  let upload = multipart_upload () in
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
    Presigned.upload_part ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~upload
      ~part_number:(Multipart.Part_number.of_int_exn 7)
      ~options ()
    |> ok_or_fail "presigned upload part"
  in
  Alcotest.(check string)
    "method" "PUT"
    (Awskit.Request.Method.to_string
       (Presigned.method_ result :> Awskit.Request.Method.t));
  Alcotest.(check (option (list string)))
    "part number" (Some [ "7" ])
    (query_param "partNumber" (Presigned.reveal_url result));
  Alcotest.(check (option (list string)))
    "upload id" (Some [ "upload-1" ])
    (query_param "uploadId" (Presigned.reveal_url result));
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha256")
    (header "x-amz-checksum-sha256" (Presigned.signed_headers result));
  Alcotest.(check (option string))
    "no checksum algorithm header" None
    (header "x-amz-checksum-algorithm" (Presigned.signed_headers result));
  let signed_headers = signed_headers_or_fail (Presigned.reveal_url result) in
  Alcotest.(check bool)
    "signed checksum value" true
    (contains_string "x-amz-checksum-sha256" signed_headers)

let test_presigned_rejects_duplicate_signed_headers () =
  let options =
    {
      Presigned.Put_object.default_options with
      content_type = Some (content_type "text/plain");
      extra_signed_headers =
        [
          ("content-type", "application/octet-stream"); ("host", "example.com");
        ];
    }
  in
  match
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
  with
  | Error error when is_validation_field "header" error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected duplicate signed header validation error"

let test_presigned_rejects_header_newline () =
  let options =
    {
      Presigned.Put_object.default_options with
      extra_signed_headers = [ ("x-test", "ok\r\nInjected: yes") ];
    }
  in
  match
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
  with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected header validation error"

let test_presigned_folds_signed_header_whitespace () =
  let options =
    {
      Presigned.Put_object.default_options with
      extra_signed_headers = [ ("x-test", "  one \t  two   three  ") ];
    }
  in
  let result =
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ~options ()
    |> ok_or_fail "presigned folded header"
  in
  Alcotest.(check (option string))
    "folded header" (Some "one two three")
    (header "x-test" (Presigned.signed_headers result))

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
     Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt")
       ~options:put_options ()
   with
  | Error error when is_validation_field "checksum_algorithm" error -> ()
  | Error error -> Alcotest.failf "unexpected put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected presigned put checksum validation");
  let upload = multipart_upload () in
  let upload_part_options =
    { Presigned.Upload_part.default_options with checksum = Some checksum }
  in
  match
    Presigned.upload_part ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~options:upload_part_options ()
  with
  | Error error when is_validation_field "checksum_algorithm" error -> ()
  | Error error ->
      Alcotest.failf "unexpected upload part error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected presigned upload-part checksum validation"

let test_presigned_accepts_string_region_and_endpoint_config () =
  let upload = multipart_upload () in
  let endpoint_config =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 ())
      ~signing_region:(Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> ok_or_fail "local endpoint config"
  in
  let check_url label result =
    let result : Presigned.result = result |> ok_or_fail label in
    Alcotest.(check bool)
      label true
      (String.starts_with ~prefix:"http://localhost:9000/bucket/"
         (Presigned.reveal_url result))
  in
  check_url "get string region endpoint"
    (Presigned.get_object_with_endpoint_config
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~endpoint_config
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ());
  check_url "head string region endpoint"
    (Presigned.head_object_with_endpoint_config
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~endpoint_config
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ());
  check_url "put string region endpoint"
    (Presigned.put_object_with_endpoint_config
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~endpoint_config
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ());
  check_url "delete string region endpoint"
    (Presigned.delete_object_with_endpoint_config
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~endpoint_config
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ());
  check_url "upload part string region endpoint"
    (Presigned.upload_part_with_endpoint_config
       ~region:(Region.of_string_exn "us-east-1")
       ~credentials:creds ~now:test_time ~endpoint_config ~upload
       ~part_number:(Multipart.Part_number.of_int_exn 1)
       ())

let test_presigned_string_validation_errors () =
  (match
     Presigned.get_object ~region:" us-east-1" ~credentials:creds ~now:test_time
       ~bucket:(bucket_name "bucket") ~key:(object_key "file.txt") ()
   with
  | Error error when is_validation_field "region" error -> ()
  | Error error -> Alcotest.failf "unexpected region error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid region");
  match Awskit.Endpoint.of_string "http://localhost:9000/path" with
  | Error error when is_validation_field "endpoint" error -> ()
  | Error error -> Alcotest.failf "unexpected endpoint error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid endpoint"

let suite =
  [
    ( "presigned",
      [
        Alcotest.test_case "presigned result" `Quick test_presigned_result;
        Alcotest.test_case "presigned safe artifact redacts bearer material"
          `Quick test_presigned_safe_artifact_redacts_bearer_material;
        Alcotest.test_case "client-bound presigned uses runtime model" `Quick
          test_client_bound_presigned_uses_runtime_model;
        Alcotest.test_case "presigned head uses dedicated options" `Quick
          test_presigned_head_uses_dedicated_options;
        Alcotest.test_case "presigned caps expiry at credential expiration"
          `Quick test_presigned_effective_expiry_is_capped_by_credentials;
        Alcotest.test_case "presigned rejects expired credentials" `Quick
          test_presigned_rejects_expired_credentials;
        Alcotest.test_case "presigned expiry validation boundaries" `Quick
          test_presigned_expiry_validation_boundaries;
        Alcotest.test_case "presigned put checksum headers" `Quick
          test_presigned_put_checksum_headers;
        Alcotest.test_case "presigned expected owner headers" `Quick
          test_presigned_expected_bucket_owner_headers;
        Alcotest.test_case "presigned multipart upload part" `Quick
          test_presigned_upload_part;
        Alcotest.test_case "presigned rejects duplicate signed headers" `Quick
          test_presigned_rejects_duplicate_signed_headers;
        Alcotest.test_case "presigned rejects header newline" `Quick
          test_presigned_rejects_header_newline;
        Alcotest.test_case "presigned folds signed header whitespace" `Quick
          test_presigned_folds_signed_header_whitespace;
        Alcotest.test_case "presigned rejects unknown checksum" `Quick
          test_presigned_rejects_unknown_checksum;
        Alcotest.test_case "presigned accepts string region endpoint config"
          `Quick test_presigned_accepts_string_region_and_endpoint_config;
        Alcotest.test_case "presigned string validation errors" `Quick
          test_presigned_string_validation_errors;
      ] );
  ]
