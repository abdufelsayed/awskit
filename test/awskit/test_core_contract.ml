open Base

let check_validation_error name result =
  match result with
  | Ok _ -> Alcotest.failf "%s should fail validation" name
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s returned unexpected error: %s" name
        (Awskit.Error.to_string_hum error)

let test_credentials_result_and_exn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Alcotest.(check string)
    "access key" "AK"
    (Awskit.Credentials.access_key_id credentials);
  check_validation_error "blank access key"
    (Awskit.Credentials.create ~access_key_id:"" ~secret_access_key:"SK" ())

let test_region_result_and_exn () =
  let region = Awskit.Region.of_string_exn "us-east-1" in
  Alcotest.(check string) "region" "us-east-1" (Awskit.Region.to_string region);
  check_validation_error "blank region" (Awskit.Region.of_string "")

let test_endpoint_result_and_exn () =
  let endpoint = Awskit.Endpoint.https_exn ~host:"s3.amazonaws.com" () in
  Alcotest.(check string)
    "authority" "s3.amazonaws.com"
    (Awskit.Endpoint.authority endpoint);
  check_validation_error "bad endpoint host"
    (Awskit.Endpoint.https ~host:"https://s3.amazonaws.com" ())

let test_payload_hash_result_and_exn () =
  let hash =
    Awskit.Body.Payload_hash.sha256_of_string "payload"
    |> Awskit.Body.Payload_hash.to_header_value
  in
  Alcotest.(check int) "sha256 length" 64 (String.length hash);
  ignore
    (Awskit.Body.Payload_hash.of_sha256_hex_exn hash
      : Awskit.Body.Payload_hash.t);
  check_validation_error "bad payload hash"
    (Awskit.Body.Payload_hash.of_sha256_hex "not-hex")

let test_runtime_request_response_body_names () =
  let request_descriptor =
    {
      Awskit.Body.Request.content_length = Some 5L;
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string "hello";
      replayable = true;
    }
  in
  Alcotest.(check bool)
    "request descriptor replayable" true request_descriptor.replayable;
  let response_descriptor =
    {
      Awskit.Body.Response.content_length = Some 5L;
      content_type = Some "text/plain";
      headers = [ ("content-type", "text/plain") ];
    }
  in
  Alcotest.(check (option string))
    "response descriptor content type" (Some "text/plain")
    response_descriptor.content_type

let test_request_response_contracts () =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Https ~host:"s3.amazonaws.com"
      ~path:"/bucket/key"
      ~query:[ ("versionId", [ "1" ]) ]
      ()
  in
  let request =
    Awskit.Request.create_exn ~method_:`GET ~target
      ~headers:[ ("host", "s3.amazonaws.com") ]
      ()
  in
  Alcotest.(check string)
    "method" "GET"
    (Awskit.Request.Method.to_string request.method_);
  Alcotest.(check string)
    "path and query" "/bucket/key?versionId=1"
    (Awskit.Request.Target.path_and_query target);
  let response =
    Awskit.Response.create_exn ~status:200
      ~headers:
        [
          ("x-amz-request-id", "req-1");
          ("x-amz-id-2", "host-1");
          ("content-length", "42");
        ]
      ()
  in
  Alcotest.(check bool) "success" true (Awskit.Response.is_success response);
  Alcotest.(check (option string))
    "request id" (Some "req-1")
    (Awskit.Response.request_id response);
  Alcotest.(check (result (option int) reject))
    "content length" (Ok (Some 42))
    (Awskit.Response.header_int response "content-length")

let test_error_context_and_sexp () =
  let error =
    Awskit.Error.validation ~field:"bucket" "bucket must be 3-63 characters"
    |> Awskit.Error.with_operation ~service:"s3" ~name:"CreateBucket"
         ~resource:"s3://ab" ()
    |> Awskit.Error.with_context "validating caller input"
  in
  Alcotest.(check bool)
    "validation classifier" true
    (Awskit.Error.is_validation error);
  Alcotest.(check (option string))
    "validation field" (Some "bucket")
    (Awskit.Error.validation_field error);
  let sexp = Awskit.Error.sexp_of_t error |> Base.Sexp.to_string_hum in
  Alcotest.(check bool)
    "sexp names operation" true
    (String.is_substring sexp ~substring:"CreateBucket"
    && String.is_substring sexp ~substring:"s3://ab");
  let human = Awskit.Error.to_string_hum error in
  Alcotest.(check bool)
    "human includes operation" true
    (String.is_substring human ~substring:"CreateBucket"
    && String.is_substring human ~substring:"s3://ab")

let test_error_multiple_preserves_all_failures () =
  let primary = Awskit.Error.body "download failed" in
  let cleanup = Awskit.Error.body "cleanup failed" in
  let combined = Awskit.Error.multiple [ primary; cleanup ] in
  let human = Awskit.Error.to_string_hum combined in
  Alcotest.(check bool)
    "mentions primary" true
    (String.is_substring human ~substring:"download failed");
  Alcotest.(check bool)
    "mentions cleanup" true
    (String.is_substring human ~substring:"cleanup failed")

let make_service_error ~status ~code =
  Awskit.Error.service
    {
      status;
      code;
      message = None;
      request_id = None;
      host_id = None;
      headers = [];
      body = None;
    }

let retry_class_to_string = function
  | Awskit.Error.Retryable -> "Retryable"
  | Awskit.Error.Throttled -> "Throttled"
  | Awskit.Error.Auth -> "Auth"
  | Awskit.Error.Conflict -> "Conflict"
  | Awskit.Error.Not_found -> "Not_found"
  | Awskit.Error.Fatal -> "Fatal"
  | Awskit.Error.Unknown -> "Unknown"

let check_retry_class name expected actual =
  let matches =
    match (expected, actual) with
    | Awskit.Error.Retryable, Awskit.Error.Retryable
    | Awskit.Error.Throttled, Awskit.Error.Throttled
    | Awskit.Error.Auth, Awskit.Error.Auth
    | Awskit.Error.Conflict, Awskit.Error.Conflict
    | Awskit.Error.Not_found, Awskit.Error.Not_found
    | Awskit.Error.Fatal, Awskit.Error.Fatal
    | Awskit.Error.Unknown, Awskit.Error.Unknown ->
        true
    | _ -> false
  in
  if not matches then
    Alcotest.failf "%s: expected %s, got %s" name
      (retry_class_to_string expected)
      (retry_class_to_string actual)

let test_error_multiple_retry_policy () =
  let validation_error = Awskit.Error.validation "bad caller input" in
  let retryable_transport =
    Awskit.Error.transport ~retryable:true "connection reset"
  in
  let retryable_over_fatal =
    Awskit.Error.multiple [ validation_error; retryable_transport ]
  in
  check_retry_class "retryable outranks fatal" Awskit.Error.Retryable
    (Awskit.Error.retry_class retryable_over_fatal);
  let not_found_service =
    make_service_error ~status:404 ~code:(Some "NoSuchKey")
  in
  let auth_service =
    make_service_error ~status:403 ~code:(Some "AccessDenied")
  in
  let auth_over_not_found =
    Awskit.Error.multiple [ not_found_service; auth_service ]
  in
  check_retry_class "auth outranks not found" Awskit.Error.Auth
    (Awskit.Error.retry_class auth_over_not_found)

let test_error_multiple_classifiers_recurse () =
  let service_error = make_service_error ~status:503 ~code:(Some "SlowDown") in
  let auth_error = make_service_error ~status:403 ~code:(Some "AccessDenied") in
  let combined =
    Awskit.Error.multiple
      [
        Awskit.Error.body "first error has no classifier data";
        Awskit.Error.multiple
          [
            Awskit.Error.validation "validation without field";
            service_error;
            Awskit.Error.validation ~field:"bucket" "bucket is invalid";
          ];
        auth_error;
      ]
  in
  Alcotest.(check bool)
    "nested validation classifier" true
    (Awskit.Error.is_validation combined);
  Alcotest.(check (option string))
    "nested validation field" (Some "bucket")
    (Awskit.Error.validation_field combined);
  Alcotest.(check (option string))
    "nested service code" (Some "SlowDown")
    (Awskit.Error.service_code combined);
  Alcotest.(check (option int))
    "nested service status" (Some 503)
    (Awskit.Error.service_status combined);
  check_retry_class "aggregated retry class" Awskit.Error.Auth
    (Awskit.Error.retry_class combined)

let suite =
  [
    ( "core:contracts",
      [
        Alcotest.test_case "credentials result/exn" `Quick
          test_credentials_result_and_exn;
        Alcotest.test_case "region result/exn" `Quick test_region_result_and_exn;
        Alcotest.test_case "endpoint result/exn" `Quick
          test_endpoint_result_and_exn;
        Alcotest.test_case "payload hash result/exn" `Quick
          test_payload_hash_result_and_exn;
        Alcotest.test_case "runtime request/response body names" `Quick
          test_runtime_request_response_body_names;
        Alcotest.test_case "request/response metadata" `Quick
          test_request_response_contracts;
        Alcotest.test_case "error context and sexp" `Quick
          test_error_context_and_sexp;
        Alcotest.test_case "error multiple preserves failures" `Quick
          test_error_multiple_preserves_all_failures;
        Alcotest.test_case "error multiple retry policy" `Quick
          test_error_multiple_retry_policy;
        Alcotest.test_case "error multiple classifiers recurse" `Quick
          test_error_multiple_classifiers_recurse;
      ] );
  ]
