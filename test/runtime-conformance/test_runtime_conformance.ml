open Awskit_s3_test
module R = Recording_runtime

let error_text error = Awskit.Error.to_string_hum error

let is_body_error error =
  match Awskit.Error.kind error with Body _ -> true | _ -> false

let contains ~substring text =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.sub text index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let test_io_bind_law () =
  let result =
    R.IO.bind (R.IO.return 1) (fun value -> R.IO.return (value + 1))
  in
  Alcotest.(check int) "bind result" 2 result

let test_request_body_descriptor_is_authoritative () =
  let body = R.Request_body.of_string "hello" in
  let descriptor = R.Request_body.descriptor body in
  Alcotest.(check (option int64))
    "content length" (Some 5L) descriptor.Awskit.Body.Request.content_length;
  Alcotest.(check bool) "replayable" true descriptor.replayable;
  Alcotest.(check (option int64))
    "content_length helper" (Some 5L)
    (R.Request_body.content_length body)

let response_body ?read_error_after body : R.response_body =
  { body; read_error_after }

let test_response_consumer_error_wins_over_drain_error () =
  let consumer_error = Awskit.Error.Producer.body "consumer failed" in
  match
    R.Response_body.with_reader (response_body ~read_error_after:0 "abcdef")
      ~consume:(fun _ -> Error consumer_error)
  with
  | Error error ->
      Alcotest.(check bool)
        "consumer error wins" true
        (contains ~substring:"consumer failed" (error_text error))
  | Ok _ -> Alcotest.fail "expected consumer error"

let test_response_success_reports_drain_error () =
  match
    R.Response_body.with_reader (response_body ~read_error_after:0 "abcdef")
      ~consume:(fun _ -> Ok ())
  with
  | Error error ->
      Alcotest.(check bool) "drain error" true (is_body_error error)
  | Ok () -> Alcotest.fail "expected drain error"

let test_discard_reports_drain_error () =
  match
    R.Response_body.discard (response_body ~read_error_after:0 "abcdef")
  with
  | Error error ->
      Alcotest.(check bool) "discard error" true (is_body_error error)
  | Ok () -> Alcotest.fail "expected discard error"

let test_retry_timeout_random_and_sleep_capabilities () =
  let retry_policy =
    Awskit.Retry.create_exn ~max_attempts:2 ~jitter:1.0
      ~base_delay:(Ptime.Span.of_float_s 1.0 |> Option.get)
      ~max_delay:(Ptime.Span.of_float_s 1.0 |> Option.get)
      ()
  in
  let conn = R.connect ~retry_policy [] in
  Alcotest.(check int)
    "max attempts" 2
    (Awskit.Retry.max_attempts (R.Retry.policy conn));
  let delay =
    Awskit.Retry.delay (R.Retry.policy conn) ~attempt:1
      ~error:(Awskit.Error.Producer.transport ~retryable:true "temporary")
      ~random_float:(R.Random.float conn)
    |> Option.get
  in
  Alcotest.(check (float 0.0001))
    "runtime random controls delay" 0.5
    (Ptime.Span.to_float_s delay);
  R.Sleeper.sleep conn delay;
  Alcotest.(check int) "sleep recorded" 1 (List.length conn.sleeps);
  Alcotest.(check bool)
    "timeout policy visible" true
    (Option.is_some (Awskit.Timeout.span (R.Timeout.policy conn) `Connect))

let suite =
  [
    ( "runtime-conformance",
      [
        Alcotest.test_case "io bind law" `Quick test_io_bind_law;
        Alcotest.test_case "request body descriptor" `Quick
          test_request_body_descriptor_is_authoritative;
        Alcotest.test_case "consumer error wins over drain error" `Quick
          test_response_consumer_error_wins_over_drain_error;
        Alcotest.test_case "success reports drain error" `Quick
          test_response_success_reports_drain_error;
        Alcotest.test_case "discard reports drain error" `Quick
          test_discard_reports_drain_error;
        Alcotest.test_case "retry timeout random and sleep" `Quick
          test_retry_timeout_random_and_sleep_capabilities;
      ] );
  ]

let () = Alcotest.run "runtime-conformance" suite
