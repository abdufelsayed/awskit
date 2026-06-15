open Awskit_s3
open Awskit_s3_test

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let is_transport_error error =
  let open Awskit.Error in
  match kind error with Transport _ -> true | _ -> false

let body_details error =
  let open Awskit.Error in
  match kind error with Body body -> Some body | _ -> None

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let check_contains label substring text =
  Alcotest.(check bool) label true (string_contains ~substring text)

let check_operation_context error ~operation ~resource =
  let text = Awskit.Error.to_string_hum error in
  check_contains "mentions operation" operation text;
  check_contains "mentions resource" resource text

let test_retry_slow_down_then_success () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 503 slow_down; response 200 "" ]
  in
  let result =
    Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
      ~body:(Recording_s3.Body.of_string "body")
      ()
  in
  ignore (ok_or_fail "retry put" result);
  Alcotest.(check int) "attempts" 2 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 1 (List.length conn.sleeps)

let test_retry_exhaustion_adds_context () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect
      [ response 503 slow_down; response 503 slow_down; response 503 slow_down ]
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
       ~body:(Recording_s3.Body.of_string "body")
       ()
   with
  | Error error when Error.service_code error = Some "SlowDown" ->
      let text = Awskit.Error.to_string_hum error in
      check_contains "mentions exhausted" "retry attempts exhausted" text;
      check_contains "mentions attempt" "attempt=3" text;
      check_contains "mentions max" "max=3" text;
      check_operation_context error ~operation:"PutObject"
        ~resource:"s3://my-bucket/file"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected slow down");
  Alcotest.(check int) "attempts" 3 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 2 (List.length conn.sleeps)

let test_retry_fatal_error_not_retried () =
  let denied =
    {|<Error><Code>AccessDenied</Code><Message>denied</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 403 denied; response 200 "" ]
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
       ~body:(Recording_s3.Body.of_string "body")
       ()
   with
  | Error error when Error.service_code error = Some "AccessDenied" ->
      check_operation_context error ~operation:"PutObject"
        ~resource:"s3://my-bucket/file"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected access denied");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_retry_disabled_policy () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect ~retry_policy:Awskit.Retry.disabled
      [ response 503 slow_down; response 200 "" ]
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
       ~body:(Recording_s3.Body.of_string "body")
       ()
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected slow down");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_non_replayable_request_body_not_retried () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some 4L;
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string "body";
      replayable = false;
    }
  in
  let conn =
    Recording_runtime.connect [ response 503 slow_down; response 200 "" ]
  in
  let body =
    Recording_runtime.stream_request_body descriptor ~write:(fun writer ->
        Recording_runtime.write_request_body_string writer "body")
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file" ~body ()
   with
  | Error error when Error.service_code error = Some "SlowDown" ->
      let text = Awskit.Error.to_string_hum error in
      check_contains "mentions non-replayable"
        "not retried because request body is not replayable" text;
      check_contains "mentions attempt" "attempt=1" text;
      check_contains "mentions max" "max=3" text
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected slow down");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_retry_context_on_exhaustion () =
  let error =
    Awskit.Error.Internal.service ~status:503 ~code:"SlowDown"
      ~message:"reduce your request rate" ~request_id:"req-retry" ~headers:[] ()
    |> Awskit.Error.Internal.with_retry ~attempt:3 ~max_attempts:3
         ~reason:"retry attempts exhausted"
  in
  let text = Awskit.Error.to_string_hum error in
  check_contains "mentions exhausted" "retry attempts exhausted" text;
  check_contains "mentions attempt" "attempt=3" text;
  check_contains "mentions max" "max=3" text

let test_runtime_stream_request_body_error_propagates () =
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some 4L;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let stream_error =
    Awskit.Error.Internal.body "runtime stream request body failed"
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let body =
    Recording_runtime.stream_request_body descriptor ~write:(fun writer ->
        match Recording_runtime.write_request_body_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  match
    Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file" ~body ()
  with
  | Error error when is_body_error error ->
      let text = Awskit.Error.to_string_hum error in
      check_contains "mentions stream error"
        "runtime stream request body failed" text;
      check_contains "mentions retry policy" "error is not retryable by policy"
        text;
      check_operation_context error ~operation:"PutObject"
        ~resource:"s3://my-bucket/file"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream request body error"

let test_runtime_transport_error_adds_operation_context () =
  let conn = Recording_runtime.connect [] in
  match
    Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
      ~body:(Recording_s3.Body.of_string "body")
      ()
  with
  | Error error when is_transport_error error ->
      let text = Awskit.Error.to_string_hum error in
      check_contains "mentions transport error" "no canned response" text;
      check_operation_context error ~operation:"PutObject"
        ~resource:"s3://my-bucket/file"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected transport error"

let test_non_success_response_body_read_error_adds_operation_context () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 503 ~read_error_after:0 slow_down ]
  in
  match
    Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file"
      ~body:(Recording_s3.Body.of_string "body")
      ()
  with
  | Error error when is_body_error error ->
      let text = Awskit.Error.to_string_hum error in
      check_contains "mentions read failure" "simulated download read failure"
        text;
      check_operation_context error ~operation:"PutObject"
        ~resource:"s3://my-bucket/file"
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected non-success body read error"

let test_retry_jitter_bounds () =
  let policy =
    Awskit.Retry.create_exn ~max_attempts:3
      ~base_delay:(Ptime.Span.of_float_s 1.0 |> Option.get)
      ~max_delay:(Ptime.Span.of_float_s 10.0 |> Option.get)
      ~jitter:0.5 ()
  in
  let error =
    Awskit.Error.Internal.transport ~retryable:true
      "temporary transport failure"
  in
  let low =
    Awskit.Retry.delay policy ~attempt:1 ~error ~random_float:(fun () -> 0.0)
    |> Option.get
  in
  let high =
    Awskit.Retry.delay policy ~attempt:1 ~error ~random_float:(fun () -> 1.0)
    |> Option.get
  in
  Alcotest.(check (float 0.0001)) "low jitter" 0.5 (Ptime.Span.to_float_s low);
  Alcotest.(check (float 0.0001)) "high jitter" 1.0 (Ptime.Span.to_float_s high);
  match Awskit.Retry.create ~jitter:1.5 () with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid jitter"

let test_request_body_descriptor_validation () =
  let invalid_descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some (-1L);
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string "";
      replayable = true;
    }
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let body =
    Recording_runtime.stream_request_body invalid_descriptor
      ~write:(fun _writer -> Ok ())
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"file" ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected descriptor validation failure");
  Alcotest.(check int) "object put not called" 0 (List.length conn.calls);
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  (match
     Recording_s3.Multipart.upload_part conn ~bucket:"my-bucket"
       ~key:"large.bin" ~upload_id ~part_number:1 ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error ->
      Alcotest.failf "unexpected multipart error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected multipart descriptor validation failure");
  Alcotest.(check int) "multipart put not called" 0 (List.length conn.calls);
  let unknown_length_descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = None;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  let body =
    Recording_runtime.stream_request_body unknown_length_descriptor
      ~write:(fun writer ->
        Recording_runtime.write_request_body_string writer "body")
  in
  (match
     Recording_s3.Object.put conn ~bucket:"my-bucket" ~key:"unknown" ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error ->
      Alcotest.failf "unexpected unknown-length put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown-length object validation failure");
  (match
     Recording_s3.Multipart.upload_part conn ~bucket:"my-bucket"
       ~key:"large.bin" ~upload_id ~part_number:1 ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error ->
      Alcotest.failf "unexpected unknown-length part error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown-length multipart validation failure");
  Alcotest.(check int)
    "unknown-length upload not called" 0 (List.length conn.calls)

let test_response_body_drain_errors () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "6") ]
          ~read_error_after:3 "abcdef";
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "6") ]
          ~read_error_after:0 "abcdef";
      ]
  in
  let consume reader =
    let bytes = Bytes.create 3 in
    match Recording_runtime.read_response_body reader bytes ~off:0 ~len:3 with
    | Error _ as error -> error
    | Ok read -> Ok (Bytes.sub_string bytes 0 read)
  in
  (match
     Recording_s3.Object.get conn ~bucket:"my-bucket" ~key:"file" ~consume ()
   with
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "unexpected get error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected drain failure after successful consume");
  (match Recording_s3.Object.head conn ~bucket:"my-bucket" ~key:"file" () with
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected discard failure after successful head");
  Alcotest.(check int) "calls" 2 (List.length conn.calls)

let test_in_memory_helper_limit_error_uses_max_bytes () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:[ ("etag", "\"etag\""); ("content-length", "6") ]
          "abcdef";
      ]
  in
  match
    Recording_s3.Object.get conn ~bucket:"my-bucket" ~key:"file"
      ~consume:(Recording_s3.Reader.to_string ~max_bytes:3L)
      ()
  with
  | Error error -> (
      match body_details error with
      | Some { message; limit = Some 3L } ->
          Alcotest.(check string)
            "message" "response body exceeded max_bytes" message
      | _ -> Alcotest.failf "unexpected error: %a" Error.pp error)
  | Ok _ -> Alcotest.fail "expected max_bytes body limit failure"

let suite =
  [
    ( "retry and body",
      [
        Alcotest.test_case "retry slow down then success" `Quick
          test_retry_slow_down_then_success;
        Alcotest.test_case "retry exhaustion adds context" `Quick
          test_retry_exhaustion_adds_context;
        Alcotest.test_case "retry fatal error not retried" `Quick
          test_retry_fatal_error_not_retried;
        Alcotest.test_case "retry disabled policy" `Quick
          test_retry_disabled_policy;
        Alcotest.test_case "non-replayable request body not retried" `Quick
          test_non_replayable_request_body_not_retried;
        Alcotest.test_case "retry context formatting on exhaustion" `Quick
          test_retry_context_on_exhaustion;
        Alcotest.test_case "runtime stream request body error propagates" `Quick
          test_runtime_stream_request_body_error_propagates;
        Alcotest.test_case "runtime transport error adds operation context"
          `Quick test_runtime_transport_error_adds_operation_context;
        Alcotest.test_case "non-success body read error adds operation context"
          `Quick
          test_non_success_response_body_read_error_adds_operation_context;
        Alcotest.test_case "retry jitter bounds" `Quick test_retry_jitter_bounds;
        Alcotest.test_case "request body descriptor validation" `Quick
          test_request_body_descriptor_validation;
        Alcotest.test_case "response body drain errors" `Quick
          test_response_body_drain_errors;
        Alcotest.test_case "in-memory helper limit error uses max_bytes" `Quick
          test_in_memory_helper_limit_error_uses_max_bytes;
      ] );
  ]
