open Base

let check_validation_error name result =
  match result with
  | Ok _ -> Alcotest.failf "%s should fail validation" name
  | Error (Awskit.Error.Validation _) -> ()
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
      ] );
  ]
