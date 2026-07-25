open Awskit_s3
module O = Awskit.Observability
module P = O.For_projection
module Recording = Protocol_recording_runtime

let slow_down =
  {|<Error><Code>SlowDown</Code><Message>reduce rate</Message></Error>|}

let policy =
  Awskit.Retry.create_exn ~max_attempts:2 ~base_delay:(Ptime.Span.of_int_s 1)
    ~jitter:0. ()

let put conn body =
  Recording.S3.Object.put conn
    ~bucket:(Protocol_support.bucket_name "bucket")
    ~key:(Protocol_support.object_key "object")
    ~body ()

let get_string conn =
  Recording.S3.Object.get_string conn
    ~bucket:(Protocol_support.bucket_name "bucket")
    ~key:(Protocol_support.object_key "object")
    ~max_bytes:1024L ()

let operation_completions conn name =
  Recording.observations conn
  |> List.filter (fun completion ->
      String.equal name
        (completion |> P.Operation.Completion.info |> P.Operation.Info.name))

let events conn name =
  Recording.events conn
  |> List.filter (fun event ->
      String.equal name (event |> P.Event.info |> P.Event.Info.name))

let event_dimension name event =
  event
  |> P.Event.dimensions
  |> List.find_map (fun value ->
      if String.equal name (P.Dimension.name value) then
        Some (P.Dimension.value value)
      else None)

let event_measurement name event =
  event
  |> P.Event.measurements
  |> List.find_map (fun value ->
      if String.equal name (P.Measurement.name value) then
        Some (P.Measurement.value value)
      else None)

let event_diagnostic name event =
  event
  |> P.Event.diagnostics
  |> List.find_map (fun value ->
      if String.equal name (O.Diagnostic.Public.name value) then
        Some (O.Diagnostic.Public.value value)
      else None)

let measurement name completion =
  completion
  |> P.Operation.Completion.measurements
  |> List.find_map (fun value ->
      if String.equal name (P.Measurement.name value) then
        Some (P.Measurement.value value)
      else None)

let dimension name completion =
  completion
  |> P.Operation.Completion.dimensions
  |> List.find_map (fun value ->
      if String.equal name (P.Dimension.name value) then
        Some (P.Dimension.value value)
      else None)

let diagnostic name completion =
  completion
  |> P.Operation.Completion.diagnostics
  |> List.find_map (fun value ->
      if String.equal name (O.Diagnostic.Public.name value) then
        Some (O.Diagnostic.Public.value value)
      else None)

let check_logical_dimensions conn ~operation expected =
  let completions = operation_completions conn "awskit.s3.operation" in
  List.iter
    (fun completion ->
      Alcotest.(check (option string))
        (Fmt.str "%s logical completion has typed operation" operation)
        (Some expected)
        (dimension "aws.operation" completion))
    completions;
  completions

let timeline_label (entry : Recording.timeline_entry) =
  let kind =
    match entry.kind with
    | Start -> "start"
    | Finish -> "finish"
    | Event -> "event"
    | Sleep -> "sleep"
  in
  let parent = Option.value entry.parent ~default:"-" in
  let attempt = Option.fold ~none:"-" ~some:string_of_int entry.attempt in
  String.concat ":" [ kind; entry.name; parent; attempt ]

let coordination_timeline conn =
  Recording.timeline conn
  |> List.filter (fun (entry : Recording.timeline_entry) ->
      match entry.kind with
      | Start ->
          List.mem entry.name
            [
              "awskit.s3.operation";
              "awskit.s3.attempt";
              "awskit.credentials.resolve";
              "awskit.s3.signing";
              "awskit.http.attempt";
            ]
      | Finish ->
          List.mem entry.name
            [
              "awskit.s3.operation";
              "awskit.s3.attempt";
              "awskit.credentials.resolve";
              "awskit.s3.signing";
              "awskit.http.attempt";
            ]
      | Event | Sleep -> true)
  |> List.map timeline_label

let test_retry_timeline () =
  let conn =
    Recording.connect ~retry_policy:policy
      [
        Recording.response ~headers:[ ("x-amz-request-id", "R1") ] 503 slow_down;
        Recording.response ~headers:[ ("x-amz-request-id", "R2") ] 206 "payload";
      ]
  in
  let result =
    get_string conn |> Protocol_support.ok_or_fail "retrying get object"
  in
  Alcotest.(check string) "successful response body" "payload" result.value;
  let completions = operation_completions conn "awskit.s3.operation" in
  Alcotest.(check int) "one logical operation" 1 (List.length completions);
  let completion = List.hd completions in
  Alcotest.(check (option string))
    "typed operation dimension" (Some "GetObject")
    (dimension "aws.operation" completion);
  Alcotest.(check string)
    "logical result" "ok"
    (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string);
  (match measurement "attempts" completion with
  | Some (Int 2) -> ()
  | _ -> Alcotest.fail "logical completion did not record two attempts");
  Alcotest.(check int)
    "one scheduled retry" 1
    (List.length (events conn "awskit.s3.retry.scheduled"));
  let retry = events conn "awskit.s3.retry.scheduled" |> List.hd in
  Alcotest.(check (option string))
    "scheduled decision" (Some "scheduled")
    (event_dimension "retry.decision" retry);
  Alcotest.(check (option string))
    "throttling retry class" (Some "throttled")
    (event_dimension "retry.class" retry);
  Alcotest.(check (option string))
    "replayable request" (Some "replayable")
    (event_dimension "request.replayability" retry);
  (match event_diagnostic "attempt" retry with
  | Some (Int 1) -> ()
  | _ -> Alcotest.fail "retry event was not attributed to attempt one");
  (match event_measurement "retry.delay" retry with
  | Some (Float 1.) -> ()
  | _ -> Alcotest.fail "retry event did not preserve the chosen delay");
  (match event_measurement "retry.remaining_budget" retry with
  | Some (Int remaining) when remaining >= 0 -> ()
  | _ -> Alcotest.fail "retry event did not preserve remaining budget");
  List.iter
    (fun completion ->
      Alcotest.(check bool)
        "operation completion contains no rendered error diagnostic" true
        (Option.is_none (diagnostic "error" completion)))
    (Recording.observations conn);
  List.iter
    (fun event ->
      Alcotest.(check bool)
        "retry event contains no rendered error diagnostic" true
        (Option.is_none (event_diagnostic "error" event)))
    (Recording.events conn);
  Alcotest.(check int)
    "one backoff sleep" 1
    (List.length (Recording.sleeps conn));
  (match measurement "logical.response_bytes" completion with
  | Some (Int64 7L) -> ()
  | _ -> Alcotest.fail "logical completion did not record seven body bytes");
  Alcotest.(check (list string))
    "attempt topology and backoff boundary"
    [
      "start:awskit.s3.operation:-:-";
      "start:awskit.s3.attempt:awskit.s3.operation:1";
      "start:awskit.credentials.resolve:awskit.s3.attempt:1";
      "finish:awskit.credentials.resolve:awskit.s3.attempt:1";
      "start:awskit.s3.signing:awskit.s3.attempt:1";
      "finish:awskit.s3.signing:awskit.s3.attempt:1";
      "start:awskit.http.attempt:awskit.s3.attempt:1";
      "finish:awskit.http.attempt:awskit.s3.attempt:1";
      "event:awskit.s3.retry.scheduled:awskit.s3.attempt:1";
      "finish:awskit.s3.attempt:awskit.s3.operation:1";
      "sleep:retry.backoff:awskit.s3.operation:-";
      "start:awskit.s3.attempt:awskit.s3.operation:2";
      "start:awskit.credentials.resolve:awskit.s3.attempt:2";
      "finish:awskit.credentials.resolve:awskit.s3.attempt:2";
      "start:awskit.s3.signing:awskit.s3.attempt:2";
      "finish:awskit.s3.signing:awskit.s3.attempt:2";
      "start:awskit.http.attempt:awskit.s3.attempt:2";
      "finish:awskit.http.attempt:awskit.s3.attempt:2";
      "finish:awskit.s3.attempt:awskit.s3.operation:2";
      "finish:awskit.s3.operation:-:-";
    ]
    (coordination_timeline conn);
  let attempts = operation_completions conn "awskit.http.attempt" in
  Alcotest.(check int) "two HTTP attempts" 2 (List.length attempts);
  let first, second = (List.nth attempts 0, List.nth attempts 1) in
  (match measurement "http.connector_response_bytes" first with
  | Some (Int64 bytes)
    when Int64.equal bytes (Int64.of_int (String.length slow_down)) ->
      ()
  | _ -> Alcotest.fail "retry response connector bytes were not retained");
  (match measurement "http.connector_response_bytes" second with
  | Some (Int64 7L) -> ()
  | _ -> Alcotest.fail "successful response connector bytes were not retained");
  (match
     ( diagnostic "http.response.status_code" first,
       diagnostic "aws.request_id" first )
   with
  | Some (Int 503), Some (String "R1") -> ()
  | _ -> Alcotest.fail "first attempt status or request ID was not preserved");
  match
    ( diagnostic "http.response.status_code" second,
      diagnostic "aws.request_id" second )
  with
  | Some (Int 206), Some (String "R2") -> ()
  | _ -> Alcotest.fail "second attempt status or request ID was not preserved"

let test_invalid_provider_identifiers_are_omitted () =
  let control_request_id = "R1\tSECRET-AUTHORIZATION" in
  let oversized_host_id = String.make 1025 'H' in
  let conn =
    Recording.connect
      [
        Recording.response
          ~headers:
            [
              ("x-amz-request-id", control_request_id);
              ("x-amz-id-2", oversized_host_id);
            ]
          206 "payload";
      ]
  in
  ignore
    (get_string conn
    |> Protocol_support.ok_or_fail "response with invalid provider identifiers"
    );
  let completion =
    operation_completions conn "awskit.http.attempt" |> List.hd
  in
  (match diagnostic "http.response.status_code" completion with
  | Some (Int 206) -> ()
  | _ -> Alcotest.fail "valid status diagnostic was not preserved");
  Alcotest.(check bool)
    "control-bearing request ID omitted" true
    (Option.is_none (diagnostic "aws.request_id" completion));
  Alcotest.(check bool)
    "oversized extended request ID omitted" true
    (Option.is_none (diagnostic "aws.extended_request_id" completion));
  let diagnostic_strings =
    completion
    |> P.Operation.Completion.diagnostics
    |> List.filter_map (fun diagnostic ->
        match O.Diagnostic.Public.value diagnostic with
        | String value -> Some value
        | Bool _ | Int _ | Int64 _ | Float _ -> None)
  in
  List.iter
    (fun rejected ->
      Alcotest.(check bool)
        "rejected provider identifier absent from canonical completion" false
        (List.mem rejected diagnostic_strings))
    [ control_request_id; oversized_host_id ]

let test_put_convenience_paths_share_one_logical_operation () =
  let run label body =
    let conn = Recording.connect [ Recording.response 200 "" ] in
    let result =
      match body with
      | `String ->
          Recording.S3.Object.put_string conn
            ~bucket:(Protocol_support.bucket_name "bucket")
            ~key:(Protocol_support.object_key "object")
            ~contents:"body" ()
      | `Bytes ->
          Recording.S3.Object.put_bytes conn
            ~bucket:(Protocol_support.bucket_name "bucket")
            ~key:(Protocol_support.object_key "object")
            ~contents:(Bytes.of_string "body") ()
    in
    ignore (Protocol_support.ok_or_fail label result);
    let completions =
      check_logical_dimensions conn ~operation:"PutObject" "PutObject"
    in
    Alcotest.(check int)
      (label ^ " has one logical completion")
      1 (List.length completions);
    Alcotest.(check int)
      (label ^ " has one physical call")
      1
      (List.length (Recording.calls conn))
  in
  run "put_string" `String;
  run "put_bytes" `Bytes

let test_not_found_convenience_paths_share_one_logical_operation () =
  let missing_body =
    "<Error><Code>NoSuchKey</Code><Message>missing</Message></Error>"
  in
  let find_conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response 404 missing_body ]
  in
  let found =
    Recording.S3.Object.find_string find_conn
      ~bucket:(Protocol_support.bucket_name "bucket")
      ~key:(Protocol_support.object_key "missing")
      ~max_bytes:1024L ()
    |> Protocol_support.ok_or_fail "find_string missing"
  in
  Alcotest.(check bool)
    "find_string maps NoSuchKey to None" true (Option.is_none found);
  let find_completions =
    check_logical_dimensions find_conn ~operation:"GetObject" "GetObject"
  in
  Alcotest.(check int)
    "find_string has one logical completion" 1
    (List.length find_completions);

  let metadata_conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response 404 "" ]
  in
  let metadata =
    Recording.S3.Object.find_metadata metadata_conn
      ~bucket:(Protocol_support.bucket_name "bucket")
      ~key:(Protocol_support.object_key "missing")
      ()
    |> Protocol_support.ok_or_fail "find_metadata missing"
  in
  Alcotest.(check bool)
    "find_metadata maps status-only 404 to None" true (Option.is_none metadata);
  let metadata_completions =
    check_logical_dimensions metadata_conn ~operation:"HeadObject" "HeadObject"
  in
  Alcotest.(check int)
    "find_metadata has one logical completion" 1
    (List.length metadata_completions)

let list_page_one =
  "<ListBucketResult><Name>bucket</Name><KeyCount>1</KeyCount>"
  ^ "<IsTruncated>true</IsTruncated><NextContinuationToken>token-2</NextContinuationToken>"
  ^ "<Contents><Key>one</Key><Size>1</Size></Contents></ListBucketResult>"

let list_page_two =
  "<ListBucketResult><Name>bucket</Name><KeyCount>1</KeyCount>"
  ^ "<IsTruncated>false</IsTruncated>"
  ^ "<Contents><Key>two</Key><Size>2</Size></Contents></ListBucketResult>"

let versions_page_one =
  "<ListVersionsResult><Name>bucket</Name><IsTruncated>true</IsTruncated>"
  ^ "<NextKeyMarker>two</NextKeyMarker>"
  ^ "<Version><Key>one</Key><VersionId>v1</VersionId>"
  ^ "<IsLatest>true</IsLatest><Size>1</Size></Version></ListVersionsResult>"

let versions_page_two =
  "<ListVersionsResult><Name>bucket</Name><IsTruncated>false</IsTruncated>"
  ^ "<Version><Key>two</Key><VersionId>v2</VersionId>"
  ^ "<IsLatest>true</IsLatest><Size>2</Size></Version></ListVersionsResult>"

let test_paginators_observe_each_page_without_aggregate_completion () =
  let bucket = Protocol_support.bucket_name "bucket" in
  let list_conn =
    Recording.connect
      [
        Recording.response 200 list_page_one;
        Recording.response 200 list_page_two;
      ]
  in
  let pages =
    Recording.S3.Object.List.pages list_conn ~bucket ~max_pages:2 ()
    |> Protocol_support.ok_or_fail "list pages"
  in
  Alcotest.(check int) "ListObjectsV2 returns two pages" 2 (List.length pages);
  let list_completions =
    check_logical_dimensions list_conn ~operation:"ListObjectsV2"
      "ListObjectsV2"
  in
  Alcotest.(check int)
    "ListObjectsV2 has one logical completion per page" 2
    (List.length list_completions);
  Alcotest.(check int)
    "ListObjectsV2 has two physical calls" 2
    (List.length (Recording.calls list_conn));

  let versions_conn =
    Recording.connect
      [
        Recording.response 200 versions_page_one;
        Recording.response 200 versions_page_two;
      ]
  in
  let pages =
    Recording.S3.Object.Versions.pages versions_conn ~bucket ~max_pages:2 ()
    |> Protocol_support.ok_or_fail "version pages"
  in
  Alcotest.(check int)
    "ListObjectVersions returns two pages" 2 (List.length pages);
  let versions_completions =
    check_logical_dimensions versions_conn ~operation:"ListObjectVersions"
      "ListObjectVersions"
  in
  Alcotest.(check int)
    "ListObjectVersions has one logical completion per page" 2
    (List.length versions_completions);
  Alcotest.(check int)
    "ListObjectVersions has two physical calls" 2
    (List.length (Recording.calls versions_conn))

let test_non_get_validation_is_one_zero_attempt_completion () =
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response 200 "unused" ]
  in
  let options = { Object.List.default_options with max_keys = Some 0 } in
  (match
     Recording.S3.Object.list conn
       ~bucket:(Protocol_support.bucket_name "bucket")
       ~options ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid list max_keys unexpectedly succeeded");
  let completions =
    check_logical_dimensions conn ~operation:"ListObjectsV2" "ListObjectsV2"
  in
  Alcotest.(check int)
    "list validation has one logical completion" 1 (List.length completions);
  let completion = List.hd completions in
  (match measurement "attempts" completion with
  | Some (Int 0) -> ()
  | _ -> Alcotest.fail "list validation fabricated a physical attempt");
  Alcotest.(check int)
    "list validation made no transport call" 0
    (List.length (Recording.calls conn))

let test_body_failure_excludes_logical_response_bytes () =
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response ~read_error_after:0 206 "payload" ]
  in
  (match get_string conn with
  | Error error ->
      Alcotest.(check bool)
        "body error preserved" true
        (match Awskit.Error.kind error with Body _ -> true | _ -> false)
  | Ok _ -> Alcotest.fail "body failure unexpectedly succeeded");
  let logical = operation_completions conn "awskit.s3.operation" |> List.hd in
  Alcotest.(check bool)
    "no logical response byte measurement" true
    (Option.is_none (measurement "logical.response_bytes" logical));
  let consumption =
    operation_completions conn "awskit.http.response_body.consumption"
  in
  Alcotest.(check int) "one consumption scope" 1 (List.length consumption);
  Alcotest.(check string)
    "body phase error" "error"
    (consumption
    |> List.hd
    |> P.Operation.Completion.outcome
    |> O.Outcome.to_string)

let test_discarded_success_excludes_logical_response_bytes () =
  let response_body = "ignored response body" in
  let assert_discarded label operation ?(headers = []) run =
    let conn =
      Recording.connect ~retry_policy:Awskit.Retry.disabled
        [ Recording.response ~headers 200 response_body ]
    in
    ignore (run conn |> Protocol_support.ok_or_fail label);
    let logical =
      check_logical_dimensions conn ~operation operation |> List.hd
    in
    Alcotest.(check string)
      (label ^ " succeeds") "ok"
      (P.Operation.Completion.outcome logical |> O.Outcome.to_string);
    Alcotest.(check bool)
      (label ^ " has no logical response byte measurement")
      true
      (Option.is_none (measurement "logical.response_bytes" logical));
    let attempt = operation_completions conn "awskit.http.attempt" |> List.hd in
    (match measurement "http.connector_response_bytes" attempt with
    | Some (Int64 0L) -> ()
    | _ -> Alcotest.failf "%s counted discarded bytes as caller-consumed" label);
    match measurement "http.connector_drained_bytes" attempt with
    | Some (Int64 bytes)
      when Int64.equal bytes (Int64.of_int (String.length response_body)) ->
        ()
    | _ -> Alcotest.failf "%s did not retain the exact drained byte count" label
  in
  let bucket = Protocol_support.bucket_name "bucket" in
  let key = Protocol_support.object_key "object" in
  assert_discarded "PutObject" "PutObject" (fun conn ->
      put conn (Recording.S3.Body.of_string "request body"));
  assert_discarded "HeadObject" "HeadObject" (fun conn ->
      Recording.S3.Object.head conn ~bucket ~key ());
  assert_discarded "DeleteObject" "DeleteObject" (fun conn ->
      Recording.S3.Object.delete conn ~bucket ~key ());
  let upload =
    Multipart.Upload.resume ~bucket ~key
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-id")
  in
  assert_discarded "UploadPart" "UploadPart"
    ~headers:[ ("etag", "\"etag\"") ]
    (fun conn ->
      Recording.S3.Multipart.upload_part conn ~upload
        ~part_number:(Multipart.Part_number.of_int_exn 1)
        ~body:(Recording.S3.Body.of_string "request body")
        ())

let test_get_metadata_error_cleans_response_body () =
  let response_body = "unread payload" in
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [
        Recording.response
          ~headers:
            [
              ("etag", "");
              ("content-length", string_of_int (String.length response_body));
            ]
          200 response_body;
      ]
  in
  (match get_string conn with
  | Error error ->
      Alcotest.(check bool)
        "metadata decode error remains primary" true
        (Base.String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:"etag response header")
  | Ok _ -> Alcotest.fail "malformed metadata unexpectedly succeeded");
  let logical = operation_completions conn "awskit.s3.operation" |> List.hd in
  Alcotest.(check string)
    "logical metadata failure" "error"
    (P.Operation.Completion.outcome logical |> O.Outcome.to_string);
  let consumption =
    operation_completions conn "awskit.http.response_body.consumption"
  in
  let drain = operation_completions conn "awskit.http.response_body.drain" in
  Alcotest.(check int) "one consumption scope" 1 (List.length consumption);
  Alcotest.(check int) "one response cleanup scope" 1 (List.length drain);
  let attempt = operation_completions conn "awskit.http.attempt" |> List.hd in
  (match measurement "http.connector_response_bytes" attempt with
  | Some (Int64 0L) -> ()
  | _ -> Alcotest.fail "metadata failure counted cleanup bytes as consumed");
  match measurement "http.connector_drained_bytes" attempt with
  | Some (Int64 bytes)
    when Int64.equal bytes (Int64.of_int (String.length response_body)) ->
      ()
  | _ -> Alcotest.fail "metadata failure did not drain the response body"

let test_find_body_failure_excludes_logical_response_bytes () =
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response ~read_error_after:0 206 "payload" ]
  in
  (match
     Recording.S3.Object.find_string conn
       ~bucket:(Protocol_support.bucket_name "bucket")
       ~key:(Protocol_support.object_key "object")
       ~max_bytes:1024L ()
   with
  | Error error ->
      Alcotest.(check bool)
        "find body error preserved" true
        (match Awskit.Error.kind error with Body _ -> true | _ -> false)
  | Ok _ -> Alcotest.fail "find body failure unexpectedly succeeded");
  let logical = operation_completions conn "awskit.s3.operation" |> List.hd in
  Alcotest.(check string)
    "find logical outcome" "error"
    (P.Operation.Completion.outcome logical |> O.Outcome.to_string);
  Alcotest.(check bool)
    "find error has no logical response byte measurement" true
    (Option.is_none (measurement "logical.response_bytes" logical))

let test_non_replayable_stops_retry () =
  let conn =
    Recording.connect ~retry_policy:policy
      [ Recording.response 503 slow_down; Recording.response 200 "" ]
  in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    Recording.S3.Body.of_stream ~content_length:4L ~replayable:false
      ~write:(fun writer -> Recording.S3.Body.Writer.write_string writer "body")
    |> Protocol_support.ok_or_fail "non-replayable body"
  in
  ignore descriptor;
  (match put conn body with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "non-replayable 503 unexpectedly retried");
  Alcotest.(check int)
    "single physical call" 1
    (List.length (Recording.calls conn));
  Alcotest.(check int)
    "retry denied event" 1
    (List.length (events conn "awskit.s3.retry.denied"));
  Alcotest.(check int) "no backoff" 0 (List.length (Recording.sleeps conn))

let test_ambiguous_lost_response_retries_same_logical_operation () =
  let lost_response =
    Awskit.Error.Producer.transport ~retryable:true
      "connection closed after request body was sent"
  in
  let conn =
    Recording.connect ~retry_policy:policy
      [ Recording.failure lost_response; Recording.response 200 "" ]
  in
  ignore
    (put conn (Recording.S3.Body.of_string "body")
    |> Protocol_support.ok_or_fail "retry after ambiguous response loss");
  Alcotest.(check int)
    "two physical calls retain request history" 2
    (List.length (Recording.calls conn));
  Alcotest.(check int)
    "one retry event" 1
    (List.length (events conn "awskit.s3.retry.scheduled"));
  let completion =
    operation_completions conn "awskit.s3.operation" |> List.hd
  in
  Alcotest.(check string)
    "logical result" "ok"
    (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string);
  match measurement "attempts" completion with
  | Some (Int 2) -> ()
  | _ -> Alcotest.fail "logical completion did not record both attempts"

let test_timeout_completion_is_classified () =
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [
        Recording.failure
          (Awskit.Error.Producer.timeout ~operation:"PutObject"
             "request timed out");
      ]
  in
  (match put conn (Recording.S3.Body.of_string "body") with
  | Error error ->
      Alcotest.(check bool)
        "timeout preserved" true
        (Awskit.Error.is_timeout error)
  | Ok _ -> Alcotest.fail "timeout unexpectedly succeeded");
  let completion =
    operation_completions conn "awskit.s3.operation" |> List.hd
  in
  Alcotest.(check string)
    "timeout outcome" "timeout"
    (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string)

let test_validation_closes_one_zero_attempt_operation () =
  let conn =
    Recording.connect ~retry_policy:Awskit.Retry.disabled
      [ Recording.response 200 "unused" ]
  in
  (match
     Recording.S3.Object.get_string conn
       ~bucket:(Protocol_support.bucket_name "bucket")
       ~key:(Protocol_support.object_key "object")
       ~max_bytes:(-1L) ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid max_bytes unexpectedly succeeded");
  Alcotest.(check int)
    "validation starts and finishes one logical operation" 1
    (List.length (operation_completions conn "awskit.s3.operation"));
  let completion =
    operation_completions conn "awskit.s3.operation" |> List.hd
  in
  (match measurement "attempts" completion with
  | Some (Int 0) -> ()
  | _ -> Alcotest.fail "validation fabricated a physical attempt");
  Alcotest.(check int)
    "validation made no transport call" 0
    (List.length (Recording.calls conn));
  Alcotest.(check int)
    "validation emitted no retry event" 0
    (List.length (events conn "awskit.s3.retry.denied"))

let artifact_identity completion = dimension "aws.artifact_operation" completion
let artifact_text completion = Fmt.str "%a" P.Operation.Completion.pp completion

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.equal (String.sub text index substring_length) substring then
      true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let assert_artifact_observation_secrets_absent label completion =
  let text = artifact_text completion in
  List.iter
    (fun forbidden ->
      Alcotest.(check bool)
        (Fmt.str "%s does not contain %s" label forbidden)
        false (contains text forbidden))
    [ "https://"; "X-Amz-"; "AKID"; "SECRET"; "signature"; "canonical" ]

let run_presigned_artifact label expected call =
  let conn = Recording.connect [] in
  let result = call conn in
  ignore (Protocol_support.ok_or_fail label result);
  let parents = operation_completions conn "awskit.s3.artifact" in
  let signings = operation_completions conn "awskit.s3.artifact.signing" in
  Alcotest.(check int)
    (label ^ " has one artifact parent")
    1 (List.length parents);
  Alcotest.(check int)
    (label ^ " has one artifact signing child")
    1 (List.length signings);
  Alcotest.(check (option string))
    (label ^ " artifact identity")
    (Some expected)
    (artifact_identity (List.hd parents));
  Alcotest.(check (option string))
    (label ^ " signing identity")
    (Some expected)
    (artifact_identity (List.hd signings));
  Alcotest.(check int)
    (label ^ " has no S3 operation")
    0
    (List.length (operation_completions conn "awskit.s3.operation"));
  Alcotest.(check int)
    (label ^ " has no HTTP attempt")
    0
    (List.length (operation_completions conn "awskit.http.attempt"));
  Alcotest.(check (list string))
    (label ^ " topology")
    [
      "start:awskit.s3.artifact:-:-";
      "start:awskit.credentials.resolve:awskit.s3.artifact:-";
      "finish:awskit.credentials.resolve:awskit.s3.artifact:-";
      "start:awskit.s3.artifact.signing:awskit.s3.artifact:-";
      "finish:awskit.s3.artifact.signing:awskit.s3.artifact:-";
      "finish:awskit.s3.artifact:-:-";
    ]
    (Recording.timeline conn |> List.map timeline_label);
  assert_artifact_observation_secrets_absent (label ^ " parent")
    (List.hd parents);
  assert_artifact_observation_secrets_absent (label ^ " signing")
    (List.hd signings)

let test_presigned_artifact_identities_and_topology () =
  let bucket = Protocol_support.bucket_name "bucket" in
  let key = Protocol_support.object_key "object" in
  let upload =
    Multipart.Upload.resume ~bucket ~key
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-id")
  in
  let part_number = Multipart.Part_number.of_int_exn 1 in
  run_presigned_artifact "presign get" "PresignGetObject" (fun conn ->
      Recording.S3.Presigned.get_object conn ~bucket ~key ());
  run_presigned_artifact "presign put" "PresignPutObject" (fun conn ->
      Recording.S3.Presigned.put_object conn ~bucket ~key ());
  run_presigned_artifact "presign head" "PresignHeadObject" (fun conn ->
      Recording.S3.Presigned.head_object conn ~bucket ~key ());
  run_presigned_artifact "presign delete" "PresignDeleteObject" (fun conn ->
      Recording.S3.Presigned.delete_object conn ~bucket ~key ());
  run_presigned_artifact "presign upload part" "PresignUploadPart" (fun conn ->
      Recording.S3.Presigned.upload_part conn ~upload ~part_number ())

let test_presigned_validation_failure_keeps_signing_boundary () =
  let conn = Recording.connect [] in
  let options =
    {
      Presigned.Get_object.default_options with
      expires_in = Some Ptime.Span.zero;
    }
  in
  (match
     Recording.S3.Presigned.get_object conn
       ~bucket:(Protocol_support.bucket_name "bucket")
       ~key:(Protocol_support.object_key "object")
       ~options ()
   with
  | Error error ->
      Alcotest.(check bool)
        "presign validation error preserved" true
        (match Awskit.Error.kind error with Validation _ -> true | _ -> false)
  | Ok _ -> Alcotest.fail "invalid presign unexpectedly succeeded");
  let parents = operation_completions conn "awskit.s3.artifact" in
  let signings = operation_completions conn "awskit.s3.artifact.signing" in
  Alcotest.(check int)
    "validation has one artifact parent" 1 (List.length parents);
  Alcotest.(check int)
    "validation has one signing child" 1 (List.length signings);
  Alcotest.(check string)
    "artifact parent reports error" "error"
    (P.Operation.Completion.outcome (List.hd parents) |> O.Outcome.to_string);
  Alcotest.(check string)
    "artifact signing reports error" "error"
    (P.Operation.Completion.outcome (List.hd signings) |> O.Outcome.to_string);
  Alcotest.(check int)
    "validation has no HTTP attempt" 0
    (List.length (operation_completions conn "awskit.http.attempt"));
  assert_artifact_observation_secrets_absent "validation parent"
    (List.hd parents);
  assert_artifact_observation_secrets_absent "validation signing"
    (List.hd signings)

let test_presigned_credential_failure_skips_signing () =
  let credential_error =
    Awskit.Error.Producer.credentials "credential resolution failed"
  in
  let conn = Recording.connect ~credential_error:(Some credential_error) [] in
  (match
     Recording.S3.Presigned.get_object conn
       ~bucket:(Protocol_support.bucket_name "bucket")
       ~key:(Protocol_support.object_key "object")
       ()
   with
  | Error error ->
      Alcotest.(check bool)
        "credential error preserved" true
        (match Awskit.Error.kind error with
        | Credentials _ -> true
        | _ -> false)
  | Ok _ -> Alcotest.fail "credential failure unexpectedly succeeded");
  let parents = operation_completions conn "awskit.s3.artifact" in
  let credentials = operation_completions conn "awskit.credentials.resolve" in
  Alcotest.(check int)
    "credential failure has one artifact parent" 1 (List.length parents);
  Alcotest.(check int)
    "credential failure has one credential child" 1 (List.length credentials);
  Alcotest.(check int)
    "credential failure has no signing child" 0
    (List.length (operation_completions conn "awskit.s3.artifact.signing"));
  Alcotest.(check int)
    "credential failure has no HTTP attempt" 0
    (List.length (operation_completions conn "awskit.http.attempt"));
  Alcotest.(check string)
    "credential failure parent outcome" "error"
    (P.Operation.Completion.outcome (List.hd parents) |> O.Outcome.to_string);
  Alcotest.(check string)
    "credential failure credential outcome" "error"
    (P.Operation.Completion.outcome (List.hd credentials) |> O.Outcome.to_string);
  Alcotest.(check (list string))
    "credential failure topology"
    [
      "start:awskit.s3.artifact:-:-";
      "start:awskit.credentials.resolve:awskit.s3.artifact:-";
      "finish:awskit.credentials.resolve:awskit.s3.artifact:-";
      "finish:awskit.s3.artifact:-:-";
    ]
    (Recording.timeline conn |> List.map timeline_label);
  assert_artifact_observation_secrets_absent "credential failure parent"
    (List.hd parents)

let () =
  Alcotest.run "awskit-s3-observability-protocol"
    [
      ( "behavior:awskit-s3:observability",
        [
          Alcotest.test_case "logical retry timeline" `Quick test_retry_timeline;
          Alcotest.test_case "invalid provider identifiers" `Quick
            test_invalid_provider_identifiers_are_omitted;
          Alcotest.test_case "body failure byte accounting" `Quick
            test_body_failure_excludes_logical_response_bytes;
          Alcotest.test_case "discarded success byte accounting" `Quick
            test_discarded_success_excludes_logical_response_bytes;
          Alcotest.test_case "metadata failure response cleanup" `Quick
            test_get_metadata_error_cleans_response_body;
          Alcotest.test_case "find body failure byte accounting" `Quick
            test_find_body_failure_excludes_logical_response_bytes;
          Alcotest.test_case "non-replayable denial" `Quick
            test_non_replayable_stops_retry;
          Alcotest.test_case "ambiguous lost response" `Quick
            test_ambiguous_lost_response_retries_same_logical_operation;
          Alcotest.test_case "timeout classification" `Quick
            test_timeout_completion_is_classified;
          Alcotest.test_case "zero-attempt validation" `Quick
            test_validation_closes_one_zero_attempt_operation;
          Alcotest.test_case "put convenience paths" `Quick
            test_put_convenience_paths_share_one_logical_operation;
          Alcotest.test_case "not-found convenience paths" `Quick
            test_not_found_convenience_paths_share_one_logical_operation;
          Alcotest.test_case "paginator page operations" `Quick
            test_paginators_observe_each_page_without_aggregate_completion;
          Alcotest.test_case "non-Get zero-attempt validation" `Quick
            test_non_get_validation_is_one_zero_attempt_completion;
          Alcotest.test_case "presigned artifact identities" `Quick
            test_presigned_artifact_identities_and_topology;
          Alcotest.test_case "presigned validation boundary" `Quick
            test_presigned_validation_failure_keeps_signing_boundary;
          Alcotest.test_case "presigned credential failure" `Quick
            test_presigned_credential_failure_skips_signing;
        ] );
    ]
