open Base

let check_validation_error name result =
  match result with
  | Ok _ -> Alcotest.failf "%s should fail validation" name
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s returned unexpected error: %s" name
        (Awskit.Error.to_string_hum error)

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let test_credentials_result_and_exn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Alcotest.(check string)
    "access key" "AK"
    (Awskit.Credentials.access_key_id credentials);
  check_validation_error "blank access key"
    (Awskit.Credentials.create ~access_key_id:"" ~secret_access_key:"SK" ())

let test_credentials_preserve_source_and_expiration () =
  let expires_at =
    Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.value_exn
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~source:(`Custom "unit-test") ~expires_at ()
  in
  Alcotest.(check (option string))
    "source label" (Some "unit-test")
    (Awskit.Credentials.source_label credentials);
  Alcotest.(check (option bool))
    "source variant" (Some true)
    (Option.map (Awskit.Credentials.source credentials) ~f:(function
      | `Custom "unit-test" -> true
      | _ -> false));
  Alcotest.(check (option bool))
    "expiration" (Some true)
    (Option.map
       (Awskit.Credentials.expires_at credentials)
       ~f:(Ptime.equal expires_at));
  let direct =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  Alcotest.(check (option string))
    "direct source label" None
    (Awskit.Credentials.source_label direct);
  Alcotest.(check (option bool))
    "direct expiration" None
    (Option.map (Awskit.Credentials.expires_at direct) ~f:(fun _ -> true))

let test_static_provider_preserves_credential_metadata () =
  let expires_at =
    Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.value_exn
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~source:(`Custom "static-input") ~expires_at
      ()
  in
  let provider = Awskit.Credentials.Provider.static credentials in
  match Awskit.Credentials.Provider.resolve provider with
  | Resolved resolved ->
      Alcotest.(check (option string))
        "source label" (Some "static-input")
        (Awskit.Credentials.source_label resolved);
      Alcotest.(check (option bool))
        "source variant" (Some true)
        (Option.map (Awskit.Credentials.source resolved) ~f:(function
          | `Custom "static-input" -> true
          | _ -> false));
      Alcotest.(check (option bool))
        "expiration" (Some true)
        (Option.map
           (Awskit.Credentials.expires_at resolved)
           ~f:(Ptime.equal expires_at))
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "static provider should resolve credentials"

let test_static_provider_annotates_absent_source () =
  let expires_at =
    Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.value_exn
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~expires_at ()
  in
  let provider = Awskit.Credentials.Provider.static credentials in
  match Awskit.Credentials.Provider.resolve provider with
  | Resolved resolved ->
      Alcotest.(check (option string))
        "source label" (Some "static")
        (Awskit.Credentials.source_label resolved);
      Alcotest.(check (option bool))
        "source variant" (Some true)
        (Option.map (Awskit.Credentials.source resolved) ~f:(function
          | `Static -> true
          | _ -> false));
      Alcotest.(check (option bool))
        "expiration" (Some true)
        (Option.map
           (Awskit.Credentials.expires_at resolved)
           ~f:(Ptime.equal expires_at))
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "static provider should resolve credentials"

let test_region_result_and_exn () =
  let region = Awskit.Region.of_string_exn "us-east-1" in
  Alcotest.(check string) "region" "us-east-1" (Awskit.Region.to_string region);
  let custom_region = Awskit.Region.of_string_exn "local:test/one" in
  Alcotest.(check string)
    "custom region" "local:test/one"
    (Awskit.Region.to_string custom_region);
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
  let uppercase_hash = String.uppercase hash in
  let parsed = Awskit.Body.Payload_hash.of_sha256_hex_exn uppercase_hash in
  Alcotest.(check string)
    "uppercase hash normalizes to lowercase" hash
    (Awskit.Body.Payload_hash.to_header_value parsed);
  check_validation_error "bad payload hash"
    (Awskit.Body.Payload_hash.of_sha256_hex "not-hex");
  let non_hex_hash = String.make 64 'g' in
  check_validation_error "non-hex payload hash"
    (Awskit.Body.Payload_hash.of_sha256_hex non_hex_hash)

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
    (Awskit.Response.header_int response "content-length");
  check_validation_error "bad response status"
    (Awskit.Response.create ~status:99 ());
  check_validation_error "bad response header"
    (Awskit.Response.create ~status:200
       ~headers:[ ("content-type", "text/plain\nbad") ]
       ())

let test_response_header_parse_failures_are_decode_errors () =
  let missing = Awskit.Response.create_exn ~status:200 () in
  (match Awskit.Response.required_header missing "etag" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "missing header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected missing header decode error");
  let empty =
    Awskit.Response.create_exn ~status:200 ~headers:[ ("etag", "") ] ()
  in
  (match Awskit.Response.required_header empty "etag" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "empty header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected empty header decode error");
  let malformed =
    Awskit.Response.create_exn ~status:200
      ~headers:[ ("content-length", "not-an-int") ]
      ()
  in
  (match Awskit.Response.header_int malformed "content-length" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "bad int header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed header decode error");
  let negative =
    Awskit.Response.create_exn ~status:200
      ~headers:[ ("content-length", "-1") ]
      ()
  in
  match Awskit.Response.header_int negative "content-length" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "negative int header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected negative header decode error"

let test_error_context_and_sexp () =
  let error =
    Awskit.Error.Internal.validation ~field:"bucket"
      "bucket must be 3-63 characters"
    |> Awskit.Error.Internal.with_operation ~service:"s3" ~name:"CreateBucket"
         ~resource:"s3://ab" ()
    |> Awskit.Error.Internal.with_context "validating caller input"
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
  let primary = Awskit.Error.Internal.body "download failed" in
  let cleanup = Awskit.Error.Internal.body "cleanup failed" in
  let combined = Awskit.Error.Internal.multiple [ primary; cleanup ] in
  let human = Awskit.Error.to_string_hum combined in
  Alcotest.(check bool)
    "mentions primary" true
    (String.is_substring human ~substring:"download failed");
  Alcotest.(check bool)
    "mentions cleanup" true
    (String.is_substring human ~substring:"cleanup failed")

let test_provider_chain_continues_only_on_unavailable () =
  let open Awskit.Credentials.Provider in
  let valid =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let chain =
    chain
      [
        create (fun () ->
            Unavailable { source = `Env; reason = "not configured" });
        static valid;
      ]
  in
  match resolve chain with
  | Resolved credentials ->
      Alcotest.(check string)
        "access key" "AKID"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "expected resolved credentials"

let test_provider_chain_stops_on_invalid_configured_credentials () =
  let open Awskit.Credentials.Provider in
  let valid =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let chain =
    chain
      [
        create (fun () ->
            Invalid
              (Awskit.Error.Internal.validation ~field:"AWS_SECRET_ACCESS_KEY"
                 "missing secret"));
        static valid;
      ]
  in
  match resolve chain with
  | Invalid error ->
      Alcotest.(check bool) "validation" true (Awskit.Error.is_validation error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "expected invalid credentials to stop chain"

let test_provider_chain_reports_all_unavailable () =
  let open Awskit.Credentials.Provider in
  let chain =
    chain
      [
        create (fun () ->
            Unavailable { source = `Env; reason = "not configured" });
        create (fun () ->
            Unavailable { source = `Shared_file "default"; reason = "missing" });
      ]
  in
  match resolve chain with
  | Unavailable { source = `Shared_file "default"; reason } ->
      Alcotest.(check bool)
        "keeps useful unavailable context" true
        (String.is_substring reason ~substring:"missing")
  | Resolved _ -> Alcotest.fail "expected unavailable credentials"
  | Unavailable _ -> Alcotest.fail "unexpected unavailable source"
  | Invalid _ | Failed _ -> Alcotest.fail "expected unavailable outcome"

let make_service_error ~status ~code =
  Awskit.Error.Internal.service ~status ?code ~headers:[] ()

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
  let validation_error = Awskit.Error.Internal.validation "bad caller input" in
  let retryable_transport =
    Awskit.Error.Internal.transport ~retryable:true "connection reset"
  in
  let retryable_over_fatal =
    Awskit.Error.Internal.multiple [ validation_error; retryable_transport ]
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
    Awskit.Error.Internal.multiple [ not_found_service; auth_service ]
  in
  check_retry_class "auth outranks not found" Awskit.Error.Auth
    (Awskit.Error.retry_class auth_over_not_found)

let test_error_production_retry_classes () =
  let timeout_error =
    Awskit.Error.Internal.timeout ~operation:"connect" "connection timed out"
  in
  let not_found_service =
    make_service_error ~status:404 ~code:(Some "NoSuchKey")
  in
  let cases =
    [
      ( "credentials are auth failures",
        Awskit.Error.Auth,
        Awskit.Error.Internal.credentials ~source:"environment"
          "missing access key id" );
      ("timeouts are retryable", Awskit.Error.Retryable, timeout_error);
      ( "cancellation is fatal",
        Awskit.Error.Fatal,
        Awskit.Error.Internal.cancelled ~reason:"caller cancelled" () );
      ( "not supported is fatal",
        Awskit.Error.Fatal,
        Awskit.Error.Internal.not_supported ~feature:"s3-select"
          "S3 Select is not supported" );
      ( "retry exhaustion delegates to timeout",
        Awskit.Error.Retryable,
        Awskit.Error.Internal.retry_exhausted ~attempts:3
          ~last_error:timeout_error "retry policy exhausted" );
      ( "retry exhaustion delegates to terminal service error",
        Awskit.Error.Not_found,
        Awskit.Error.Internal.retry_exhausted ~attempts:3
          ~last_error:not_found_service "retry policy exhausted" );
      ( "retry exhaustion without terminal error is fatal",
        Awskit.Error.Fatal,
        Awskit.Error.Internal.retry_exhausted ~attempts:3
          "retry policy exhausted" );
    ]
  in
  List.iter cases ~f:(fun (name, expected, error) ->
      check_retry_class name expected (Awskit.Error.retry_class error))

let test_error_multiple_classifiers_recurse () =
  let service_error = make_service_error ~status:503 ~code:(Some "SlowDown") in
  let auth_error = make_service_error ~status:403 ~code:(Some "AccessDenied") in
  let combined =
    Awskit.Error.Internal.multiple
      [
        Awskit.Error.Internal.body "first error has no classifier data";
        Awskit.Error.Internal.multiple
          [
            Awskit.Error.Internal.validation "validation without field";
            service_error;
            Awskit.Error.Internal.validation ~field:"bucket" "bucket is invalid";
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

let test_error_production_categories_and_classifiers () =
  let credentials_error =
    Awskit.Error.Internal.credentials ~source:"environment"
      "missing access key id"
  in
  let endpoint_error =
    Awskit.Error.Internal.endpoint ~uri:"https://s3.amazonaws.com"
      "endpoint host is invalid"
  in
  let timeout_error =
    Awskit.Error.Internal.timeout ~operation:"connect" "connection timed out"
  in
  let cancelled_error =
    Awskit.Error.Internal.cancelled ~reason:"caller requested cancellation" ()
  in
  let combined =
    Awskit.Error.Internal.multiple
      [
        Awskit.Error.Internal.body "stream failed";
        Awskit.Error.Internal.multiple
          [ credentials_error; endpoint_error; timeout_error; cancelled_error ];
      ]
  in
  (match Awskit.Error.kind credentials_error with
  | Awskit.Error.Credentials { source = Some source; message } ->
      Alcotest.(check string) "credentials source" "environment" source;
      Alcotest.(check string)
        "credentials message" "missing access key id" message
  | _ -> Alcotest.fail "expected credentials kind");
  (match Awskit.Error.kind endpoint_error with
  | Awskit.Error.Endpoint { uri = Some uri; message } ->
      Alcotest.(check string) "endpoint uri" "https://s3.amazonaws.com" uri;
      Alcotest.(check string)
        "endpoint message" "endpoint host is invalid" message
  | _ -> Alcotest.fail "expected endpoint kind");
  Alcotest.(check bool)
    "nested credentials classifier" true
    (Awskit.Error.is_credentials combined);
  Alcotest.(check bool)
    "nested endpoint classifier" true
    (Awskit.Error.is_endpoint combined);
  Alcotest.(check bool)
    "nested timeout classifier" true
    (Awskit.Error.is_timeout combined);
  Alcotest.(check bool)
    "nested cancellation classifier" true
    (Awskit.Error.is_cancelled combined)

let test_exn_apis_raise_sdk_exception () =
  let raised =
    try
      ignore (Awskit.Region.of_string_exn "" : Awskit.Region.t);
      false
    with
    | Awskit.Error.Awskit_error error ->
        Awskit.Error.is_validation error
        && Option.equal String.equal
             (Awskit.Error.validation_field error)
             (Some "region")
    | _ -> false
  in
  Alcotest.(check bool) "raises SDK exception" true raised

let suite =
  [
    ( "core:contracts",
      [
        Alcotest.test_case "credentials result/exn" `Quick
          test_credentials_result_and_exn;
        Alcotest.test_case "credentials preserve source and expiration" `Quick
          test_credentials_preserve_source_and_expiration;
        Alcotest.test_case "static provider preserves credential metadata"
          `Quick test_static_provider_preserves_credential_metadata;
        Alcotest.test_case "static provider annotates absent source" `Quick
          test_static_provider_annotates_absent_source;
        Alcotest.test_case "region result/exn" `Quick test_region_result_and_exn;
        Alcotest.test_case "endpoint result/exn" `Quick
          test_endpoint_result_and_exn;
        Alcotest.test_case "payload hash result/exn" `Quick
          test_payload_hash_result_and_exn;
        Alcotest.test_case "runtime request/response body names" `Quick
          test_runtime_request_response_body_names;
        Alcotest.test_case "request/response metadata" `Quick
          test_request_response_contracts;
        Alcotest.test_case "response header parse failures are decode errors"
          `Quick test_response_header_parse_failures_are_decode_errors;
        Alcotest.test_case "error context and sexp" `Quick
          test_error_context_and_sexp;
        Alcotest.test_case "error multiple preserves failures" `Quick
          test_error_multiple_preserves_all_failures;
        Alcotest.test_case "provider chain continues on unavailable" `Quick
          test_provider_chain_continues_only_on_unavailable;
        Alcotest.test_case "provider chain stops on invalid" `Quick
          test_provider_chain_stops_on_invalid_configured_credentials;
        Alcotest.test_case "provider chain reports unavailable" `Quick
          test_provider_chain_reports_all_unavailable;
        Alcotest.test_case "error multiple retry policy" `Quick
          test_error_multiple_retry_policy;
        Alcotest.test_case "error production retry classes" `Quick
          test_error_production_retry_classes;
        Alcotest.test_case "error multiple classifiers recurse" `Quick
          test_error_multiple_classifiers_recurse;
        Alcotest.test_case "error production categories and classifiers" `Quick
          test_error_production_categories_and_classifiers;
        Alcotest.test_case "exn APIs raise SDK exception" `Quick
          test_exn_apis_raise_sdk_exception;
      ] );
  ]
