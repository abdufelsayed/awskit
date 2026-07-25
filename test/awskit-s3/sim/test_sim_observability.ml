open Test_sim_contract_support
module O = Awskit.Observability
module P = O.For_projection
module Simulator = Awskit_s3_sim
module Operation = Awskit_s3.Operation

let dimension name completion =
  completion
  |> P.Operation.Completion.dimensions
  |> Base.List.find_map ~f:(fun value ->
      if String.equal name (P.Dimension.name value) then
        Some (P.Dimension.value value)
      else None)

let measurement name completion =
  completion
  |> P.Operation.Completion.measurements
  |> Base.List.find_map ~f:(fun value ->
      if String.equal name (P.Measurement.name value) then
        Some (P.Measurement.value value)
      else None)

let int64_measurement name completion =
  match measurement name completion with
  | None -> None
  | Some (P.Measurement.Int64 value) -> Some value
  | Some _ -> Alcotest.failf "%s is not an int64 measurement" name

let only_completion label conn =
  match Simulator.observations conn with
  | [ completion ] -> completion
  | completions ->
      Alcotest.failf "%s: expected one completion, got %d" label
        (Base.List.length completions)

let check_measurement_absent label name completion =
  Alcotest.(check bool)
    label true
    (Base.Option.is_none (measurement name completion))

let check_no_attempts label completion =
  check_measurement_absent label "attempts" completion

let check_logical_request_bytes label expected completion =
  Alcotest.(check (option int64))
    label (Some expected)
    (int64_measurement "logical.request_bytes" completion)

let check_logical_response_bytes label expected completion =
  Alcotest.(check (option int64))
    label (Some expected)
    (int64_measurement "logical.response_bytes" completion)

let completion_name completion =
  completion |> P.Operation.Completion.info |> P.Operation.Info.name

let completion_outcome completion =
  completion |> P.Operation.Completion.outcome |> O.Outcome.to_string

let check_exact_completion label conn ~operation ?request_bytes ?response_bytes
    ?(outcome = "ok") thunk =
  Simulator.clear_observations conn;
  let result = thunk () in
  let completion = only_completion label conn in
  Alcotest.(check (option string))
    (label ^ " operation")
    (Some (Awskit_s3.Operation.to_string operation))
    (dimension "aws.operation" completion);
  Alcotest.(check string)
    (label ^ " outcome") outcome
    (completion_outcome completion);
  (match request_bytes with
  | None ->
      check_measurement_absent
        (label ^ " request bytes absent")
        "logical.request_bytes" completion
  | Some expected ->
      check_logical_request_bytes (label ^ " request bytes") expected completion);
  (match response_bytes with
  | None ->
      check_measurement_absent
        (label ^ " response bytes absent")
        "logical.response_bytes" completion
  | Some expected ->
      check_logical_response_bytes
        (label ^ " response bytes")
        expected completion);
  check_no_attempts (label ^ " attempts absent") completion;
  result

let test_presigned_artifact_topology () =
  let conn = make_simulator () in
  let bucket = bucket_name "test-bucket" in
  let key = object_key "artifact.txt" in
  let artifact =
    Simulator.Presigned.get_object conn ~bucket ~key ()
    |> ok_or_fail "presign get object"
  in
  let raw_url = Awskit_s3.Presigned.reveal_url artifact in
  let completions = Simulator.observations conn in
  Alcotest.(check (list string))
    "presign completion order"
    [
      "awskit.credentials.resolve";
      "awskit.s3.artifact.signing";
      "awskit.s3.artifact";
    ]
    (List.map completion_name completions);
  Alcotest.(check (list string))
    "presign completion outcomes" [ "ok"; "ok"; "ok" ]
    (List.map completion_outcome completions);
  List.iteri
    (fun index completion ->
      Alcotest.(check bool)
        (Fmt.str "presign completion %d has no S3 operation" index)
        false
        (Option.is_some (dimension "aws.operation" completion));
      Alcotest.(check (list string))
        (Fmt.str "presign completion %d has no diagnostics" index)
        []
        (List.map O.Diagnostic.Public.name
           (P.Operation.Completion.diagnostics completion));
      let rendered = Fmt.str "%a" P.Operation.Completion.pp completion in
      List.iter
        (fun secret ->
          Alcotest.(check bool)
            (Fmt.str "presign completion %d omits %s" index secret)
            false
            (Base.String.is_substring rendered ~substring:secret))
        [ "AKID"; "SECRET"; "X-Amz-Signature"; "X-Amz-Credential" ];
      Alcotest.(check bool)
        (Fmt.str "presign completion %d omits raw URL" index)
        false
        (Base.String.is_substring rendered ~substring:raw_url))
    completions;
  let signing, parent =
    match completions with
    | [ _credentials; signing; parent ] -> (signing, parent)
    | _ -> Alcotest.fail "expected credential, signing, and parent completions"
  in
  List.iter
    (fun completion ->
      Alcotest.(check (option string))
        "artifact operation identity" (Some "PresignGetObject")
        (dimension "aws.artifact_operation" completion);
      Alcotest.(check (list string))
        "artifact has no measurements" []
        (List.map P.Measurement.name
           (P.Operation.Completion.measurements completion)))
    [ signing; parent ]

let test_presigned_artifact_failure_topology () =
  let conn = make_simulator () in
  let options =
    {
      Awskit_s3.Presigned.Get_object.default_options with
      expires_in = Some Ptime.Span.zero;
    }
  in
  (match
     Simulator.Presigned.get_object conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "artifact-failure.txt")
       ~options ()
   with
  | Ok _ -> Alcotest.fail "invalid expiry unexpectedly produced an artifact"
  | Error _ -> ());
  let completions = Simulator.observations conn in
  Alcotest.(check (list string))
    "failed presign completion order"
    [
      "awskit.credentials.resolve";
      "awskit.s3.artifact.signing";
      "awskit.s3.artifact";
    ]
    (List.map completion_name completions);
  Alcotest.(check (list string))
    "failed presign outcomes" [ "ok"; "error"; "error" ]
    (List.map completion_outcome completions)

let test_precondition_failure_completion () =
  let conn = make_simulator () in
  ignore (put_string conn "precondition.txt" "payload");
  Simulator.clear_observations conn;
  let preconditions =
    {
      Awskit_s3.Object.Preconditions.Read.none with
      if_match =
        Some
          (Awskit_s3.Object.Etag_condition.etag
             (Awskit_s3.Object.Etag.of_string_exn "different"));
    }
  in
  let options = { Awskit_s3.Object.Get.default_options with preconditions } in
  expect_service_code "precondition failure" "PreconditionFailed"
    (Simulator.Object.get conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "precondition.txt")
       ~options
       ~consume:(fun _reader -> Ok ())
       ());
  let completion = only_completion "precondition failure" conn in
  Alcotest.(check string)
    "precondition outcome" "conflict"
    (completion_outcome completion);
  check_measurement_absent "precondition response bytes"
    "logical.response_bytes" completion;
  check_no_attempts "precondition attempts absent" completion

let test_request_body_producer_failure_completion () =
  let conn = make_simulator () in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let producer_error = Awskit.Error.Producer.body "producer failed" in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error producer_error)
  in
  expect_body_error "producer failure"
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "producer-failure.txt")
       ~body ());
  let completion = only_completion "producer failure" conn in
  Alcotest.(check string)
    "producer failure outcome" "error"
    (completion_outcome completion);
  check_logical_request_bytes "producer logical request bytes" 4L completion;
  check_measurement_absent "producer response bytes" "logical.response_bytes"
    completion;
  check_no_attempts "producer attempts absent" completion

let test_paginator_validation_has_no_aggregate_completion () =
  let conn = make_simulator () in
  Simulator.clear_observations conn;
  expect_validation_field "invalid paginator bound" "max_pages"
    (Simulator.Object.List.pages conn
       ~bucket:(bucket_name "test-bucket")
       ~max_pages:0 ());
  Alcotest.(check int)
    "paginator validation emits no completion" 0
    (List.length (Simulator.observations conn))

let test_history_has_canonical_parity () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  Simulator.clear_history store;
  ignore (put_string conn "observed.txt" "body");
  ignore
    (Simulator.Object.get_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "observed.txt")
       ~max_bytes:32L ()
    |> ok_or_fail "get observed object");
  let history = Simulator.history store in
  let observations = Simulator.observations conn in
  Alcotest.(check int)
    "one completion per simulator operation" (List.length history)
    (List.length observations);
  let history_operations =
    List.map
      (fun (record : Simulator.operation_record) ->
        Operation.to_string record.op)
      history
  in
  let completion_operations =
    List.map
      (fun completion ->
        match dimension "aws.operation" completion with
        | Some operation -> operation
        | None -> Alcotest.fail "completion omitted canonical operation")
      observations
  in
  Alcotest.(check (list string))
    "history and completion operation order" history_operations
    completion_operations;
  List.iter
    (fun completion ->
      Alcotest.(check (option string))
        "runtime-specific dimension absent" None
        (dimension "runtime" completion);
      Alcotest.(check bool)
        "operation is bounded" true
        (Option.is_some (dimension "aws.operation" completion));
      check_no_attempts "simulator attempt count is absent" completion;
      let names =
        completion
        |> P.Operation.Completion.dimensions
        |> List.map P.Dimension.name
      in
      Alcotest.(check bool)
        "bucket not a metric dimension" false (List.mem "bucket" names);
      Alcotest.(check bool)
        "key not a metric dimension" false (List.mem "key" names))
    observations

let test_put_and_get_logical_bytes () =
  let conn = make_simulator () in
  Simulator.clear_observations conn;
  ignore (put_string conn "bytes.txt" "payload");
  let put_completion = only_completion "put bytes" conn in
  check_logical_request_bytes "put logical request bytes" 7L put_completion;
  check_measurement_absent "put response bytes absent" "logical.response_bytes"
    put_completion;
  check_no_attempts "put attempts absent" put_completion;
  Simulator.clear_observations conn;
  let result =
    Simulator.Object.get_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "bytes.txt") ~max_bytes:32L ()
    |> ok_or_fail "get logical bytes"
  in
  Alcotest.(check string) "get result" "payload" result.value;
  let get_completion = only_completion "get bytes" conn in
  check_measurement_absent "get request bytes absent" "logical.request_bytes"
    get_completion;
  check_logical_response_bytes "get logical response bytes" 7L get_completion;
  check_no_attempts "get attempts absent" get_completion

let test_partial_consumption_excludes_drain () =
  let conn = make_simulator () in
  ignore (put_string conn "partial.txt" "abcdef");
  Simulator.clear_observations conn;
  let found =
    Simulator.Object.find conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "partial.txt")
      ~consume:(fun reader ->
        let bytes = Bytes.create 2 in
        Simulator.Reader.read reader bytes ~off:0 ~len:2)
      ()
    |> ok_or_fail "partially consume object"
  in
  let result =
    match found with
    | Some result -> result
    | None -> Alcotest.fail "expected partial object"
  in
  Alcotest.(check int) "consumer result" 2 result.value;
  let completion = only_completion "partial find" conn in
  check_logical_response_bytes "only caller-consumed bytes" 2L completion;
  check_no_attempts "partial find attempts absent" completion

let test_consumer_failures_omit_response_bytes () =
  let conn = make_simulator () in
  ignore (put_string conn "consumer-failure.txt" "abcdef");
  Simulator.clear_observations conn;
  let consumer_error = Awskit.Error.Producer.body "consumer failed" in
  expect_body_error "consumer error"
    (Simulator.Object.get conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "consumer-failure.txt")
       ~consume:(fun reader ->
         let bytes = Bytes.create 2 in
         match Simulator.Reader.read reader bytes ~off:0 ~len:2 with
         | Error _ as error -> error
         | Ok _ -> Error consumer_error)
       ());
  let error_completion = only_completion "consumer error" conn in
  check_measurement_absent "consumer error response bytes absent"
    "logical.response_bytes" error_completion;
  check_no_attempts "consumer error attempts absent" error_completion;
  Simulator.clear_observations conn;
  let exception Consumer_failed in
  let raised = ref false in
  (try
     ignore
       (Simulator.Object.get conn
          ~bucket:(bucket_name "test-bucket")
          ~key:(object_key "consumer-failure.txt")
          ~consume:(fun reader ->
            let bytes = Bytes.create 1 in
            match Simulator.Reader.read reader bytes ~off:0 ~len:1 with
            | Error error -> Error error
            | Ok _ -> raise Consumer_failed)
          ())
   with Consumer_failed -> raised := true);
  Alcotest.(check bool) "consumer exception preserved" true !raised;
  let exception_completion = only_completion "consumer exception" conn in
  Alcotest.(check string)
    "consumer exception outcome" "exception"
    (exception_completion
    |> P.Operation.Completion.outcome
    |> O.Outcome.to_string);
  check_measurement_absent "consumer exception response bytes absent"
    "logical.response_bytes" exception_completion;
  check_no_attempts "consumer exception attempts absent" exception_completion

let test_not_found_omits_response_bytes () =
  let conn = make_simulator () in
  Simulator.clear_observations conn;
  let consumed = ref false in
  let result =
    Simulator.Object.find conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "missing.txt")
      ~consume:(fun _reader ->
        consumed := true;
        Ok ())
      ()
    |> ok_or_fail "find missing object"
  in
  Alcotest.(check bool) "missing find result" true (Option.is_none result);
  Alcotest.(check bool) "missing body is not consumed" false !consumed;
  let find_completion = only_completion "find missing" conn in
  check_measurement_absent "find missing response bytes absent"
    "logical.response_bytes" find_completion;
  check_no_attempts "find missing attempts absent" find_completion;
  Simulator.clear_observations conn;
  expect_service_code "get missing object" "NoSuchKey"
    (Simulator.Object.get conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing.txt")
       ~consume:(fun _reader -> Ok ())
       ());
  let get_completion = only_completion "get missing" conn in
  check_measurement_absent "get missing response bytes absent"
    "logical.response_bytes" get_completion;
  check_no_attempts "get missing attempts absent" get_completion

let test_multipart_upload_logical_request_bytes () =
  let conn = make_simulator () in
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "multipart.bin")
      ()
    |> ok_or_fail "create multipart upload"
  in
  Simulator.clear_observations conn;
  ignore
    (Simulator.Multipart.upload_part conn ~upload:created.upload
       ~part_number:(Awskit_s3.Multipart.Part_number.of_int_exn 1)
       ~body:(Simulator.Body.of_string "part")
       ()
    |> ok_or_fail "upload observed multipart part");
  let completion = only_completion "upload multipart part" conn in
  check_logical_request_bytes "multipart logical request bytes" 4L completion;
  check_measurement_absent "multipart response bytes absent"
    "logical.response_bytes" completion;
  check_no_attempts "multipart attempts absent" completion

let test_fault_outcome () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  Simulator.clear_history store;
  Simulator.inject_fault conn Simulator.Connection_reset;
  ignore
    (Simulator.Object.put_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "faulted.txt") ~contents:"body" ());
  let completion =
    match List.rev (Simulator.observations conn) with
    | [] -> Alcotest.fail "fault did not produce a simulator completion"
    | completion :: _ -> completion
  in
  Alcotest.(check string)
    "fault is an error completion" "error"
    (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string);
  Alcotest.(check (option string))
    "fault retains its bounded retry class" (Some "retryable")
    (dimension "retry.class" completion)

let test_connection_isolation () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn_a = Simulator.connect store ~credentials in
  let conn_b = Simulator.connect store ~credentials in
  ignore
    (Simulator.Bucket.create conn_a ~bucket:(bucket_name "shared") ()
    |> ok_or_fail "create shared bucket");
  Simulator.clear_observations conn_a;
  Simulator.clear_observations conn_b;
  ignore
    (Simulator.Object.put_string conn_a ~bucket:(bucket_name "shared")
       ~key:(object_key "a") ~contents:"body" ()
    |> ok_or_fail "put shared object");
  Alcotest.(check int)
    "connection A receives its completion" 1
    (List.length (Simulator.observations conn_a));
  Alcotest.(check int)
    "connection B remains isolated" 0
    (List.length (Simulator.observations conn_b));
  Alcotest.(check int)
    "backend history remains store-wide" 1
    (List.length (Simulator.history store))

let test_validation_and_callback_exception () =
  let conn = make_simulator () in
  expect_validation_field "invalid max_bytes completion" "max_bytes"
    (Simulator.Object.get_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing") ~max_bytes:(-1L) ());
  Alcotest.(check int)
    "validation completion" 1
    (List.length (Simulator.observations conn));
  ignore (put_string conn "callback.txt" "body");
  Simulator.clear_observations conn;
  let callback_raised = ref false in
  let callback_backtrace = ref None in
  let exception Callback_failed in
  (try
     ignore
       (Simulator.Object.get conn
          ~bucket:(bucket_name "test-bucket")
          ~key:(object_key "callback.txt")
          ~consume:(fun _reader ->
            let backtrace = Printexc.get_raw_backtrace () in
            callback_backtrace :=
              Some (Printexc.raw_backtrace_to_string backtrace);
            Printexc.raise_with_backtrace Callback_failed backtrace)
          ())
   with Callback_failed -> (
     callback_raised := true;
     let observed =
       Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string
     in
     match !callback_backtrace with
     | None -> Alcotest.fail "callback did not capture a backtrace"
     | Some expected ->
         let prefix_length = String.length expected in
         Alcotest.(check bool)
           "callback backtrace preserved" true
           (String.length observed >= prefix_length
           && String.equal (String.sub observed 0 prefix_length) expected)));
  Alcotest.(check bool) "callback exception preserved" true !callback_raised;
  Alcotest.(check int)
    "callback exception completion" 1
    (List.length (Simulator.observations conn));
  let completion = List.hd (Simulator.observations conn) in
  Alcotest.(check string)
    "callback exception outcome" "exception"
    (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string)

let test_leaf_operations_and_pagination () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let config = Simulator.config_exn ~max_list_keys:1 () in
  let store = Simulator.create_store ~config ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = bucket_name "leaves" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create leaves");
  Simulator.clear_observations conn;
  ignore
    (Simulator.Object.put_string conn ~bucket ~key:(object_key "one")
       ~contents:"one" ()
    |> ok_or_fail "put one");
  ignore
    (Simulator.Object.put_string conn ~bucket ~key:(object_key "two")
       ~contents:"two" ()
    |> ok_or_fail "put two");
  ignore
    (Simulator.Object.Tagging.put conn ~bucket ~key:(object_key "one")
       ~tags:Awskit_s3.Tag.Set.empty ()
    |> ok_or_fail "put object tags");
  ignore
    (Simulator.Object.Tagging.get conn ~bucket ~key:(object_key "one") ()
    |> ok_or_fail "get object tags");
  ignore
    (Simulator.Object.Tagging.delete conn ~bucket ~key:(object_key "one") ()
    |> ok_or_fail "delete object tags");
  ignore
    (Simulator.Bucket.Tagging.put conn ~bucket ~tags:Awskit_s3.Tag.Set.empty ()
    |> ok_or_fail "put bucket tags");
  ignore
    (Simulator.Bucket.Tagging.get conn ~bucket ()
    |> ok_or_fail "get bucket tags");
  ignore
    (Simulator.Bucket.Tagging.delete conn ~bucket ()
    |> ok_or_fail "delete bucket tags");
  ignore
    (Simulator.Bucket.Versioning.put conn ~bucket
       ~status:Awskit_s3.Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "put versioning");
  ignore
    (Simulator.Bucket.Versioning.get conn ~bucket ()
    |> ok_or_fail "get versioning");
  let encryption = { Awskit_s3.Bucket.Encryption.rules = [] } in
  ignore
    (Simulator.Bucket.Encryption.put conn ~bucket ~config:encryption ()
    |> ok_or_fail "put encryption");
  ignore
    (Simulator.Bucket.Encryption.get conn ~bucket ()
    |> ok_or_fail "get encryption");
  ignore
    (Simulator.Bucket.Encryption.delete conn ~bucket ()
    |> ok_or_fail "delete encryption");
  let cors = { Awskit_s3.Bucket.Cors.rules = [] } in
  ignore
    (Simulator.Bucket.Cors.put conn ~bucket ~config:cors ()
    |> ok_or_fail "put cors");
  ignore (Simulator.Bucket.Cors.get conn ~bucket () |> ok_or_fail "get cors");
  ignore
    (Simulator.Bucket.Cors.delete conn ~bucket () |> ok_or_fail "delete cors");
  ignore
    (Simulator.Bucket.Public_access_block.put conn ~bucket
       ~config:Awskit_s3.Bucket.Public_access_block.all_false ()
    |> ok_or_fail "put public access block");
  ignore
    (Simulator.Bucket.Public_access_block.get conn ~bucket ()
    |> ok_or_fail "get public access block");
  ignore
    (Simulator.Bucket.Public_access_block.delete conn ~bucket ()
    |> ok_or_fail "delete public access block");
  let ownership =
    {
      Awskit_s3.Bucket.Ownership_controls.object_ownership =
        Bucket_owner_enforced;
    }
  in
  ignore
    (Simulator.Bucket.Ownership_controls.put conn ~bucket ~config:ownership ()
    |> ok_or_fail "put ownership controls");
  ignore
    (Simulator.Bucket.Ownership_controls.get conn ~bucket ()
    |> ok_or_fail "get ownership controls");
  ignore
    (Simulator.Bucket.Ownership_controls.delete conn ~bucket ()
    |> ok_or_fail "delete ownership controls");
  let upload =
    Simulator.Multipart.create_upload conn ~bucket ~key:(object_key "multi") ()
    |> ok_or_fail "create multipart"
  in
  let body = Simulator.Body.of_string "part" in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:upload.upload
      ~part_number:(Awskit_s3.Multipart.Part_number.of_int_exn 1)
      ~body ()
    |> ok_or_fail "upload multipart part"
  in
  ignore
    (Simulator.Multipart.list_parts conn ~upload:upload.upload ()
    |> ok_or_fail "list multipart parts");
  ignore
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~parts:[ uploaded.part ] ()
    |> ok_or_fail "complete multipart");
  let operation_names =
    Simulator.observations conn |> List.filter_map (dimension "aws.operation")
  in
  let expected =
    [
      "GetObjectTagging";
      "PutObjectTagging";
      "DeleteObjectTagging";
      "GetBucketTagging";
      "PutBucketTagging";
      "DeleteBucketTagging";
      "GetBucketVersioning";
      "PutBucketVersioning";
      "GetBucketEncryption";
      "PutBucketEncryption";
      "DeleteBucketEncryption";
      "GetBucketCors";
      "PutBucketCors";
      "DeleteBucketCors";
      "GetPublicAccessBlock";
      "PutPublicAccessBlock";
      "DeletePublicAccessBlock";
      "GetBucketOwnershipControls";
      "PutBucketOwnershipControls";
      "DeleteBucketOwnershipControls";
      "CreateMultipartUpload";
      "UploadPart";
      "ListParts";
      "CompleteMultipartUpload";
    ]
  in
  List.iter
    (fun expected_name ->
      Alcotest.(check bool)
        ("leaf completion " ^ expected_name)
        true
        (List.mem expected_name operation_names))
    expected;
  ignore
    (Simulator.Object.List.pages conn ~bucket ~max_pages:3 ()
    |> ok_or_fail "list pages");
  let list_count =
    Simulator.observations conn
    |> List.filter (fun completion ->
        String.equal
          (Option.value ~default:"" (dimension "aws.operation" completion))
          "ListObjectsV2")
    |> List.length
  in
  Alcotest.(check int)
    "pagination observes one completion per page" 3 list_count

let test_core_policy_delete_many_abort_exact_completions () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = bucket_name "exact-leaves" in
  ignore
    (check_exact_completion "create bucket" conn
       ~operation:Operation.Create_bucket (fun () ->
         Simulator.Bucket.create conn ~bucket ())
    |> ok_or_fail "create exact-leaves bucket");
  ignore
    (check_exact_completion "head bucket" conn ~operation:Operation.Head_bucket
       (fun () -> Simulator.Bucket.head conn ~bucket ())
    |> ok_or_fail "head exact-leaves bucket");
  ignore
    (check_exact_completion "exists bucket" conn
       ~operation:Operation.Head_bucket (fun () ->
         Simulator.Bucket.exists conn ~bucket ())
    |> ok_or_fail "exists exact-leaves bucket");
  ignore
    (check_exact_completion "list buckets" conn
       ~operation:Operation.List_buckets (fun () -> Simulator.Bucket.list conn)
    |> ok_or_fail "list buckets");
  ignore
    (check_exact_completion "get bucket location" conn
       ~operation:Operation.Get_bucket_location (fun () ->
         Simulator.Bucket.get_location conn ~bucket ())
    |> ok_or_fail "get exact-leaves bucket location");
  let policy =
    Awskit_s3.Policy.of_json {|{"Version":"2012-10-17","Statement":[]}|}
    |> ok_or_fail "build bucket policy"
  in
  ignore
    (check_exact_completion "put bucket policy" conn
       ~operation:Operation.Put_bucket_policy (fun () ->
         Simulator.Bucket.Policy.put conn ~bucket ~policy ())
    |> ok_or_fail "put bucket policy");
  ignore
    (check_exact_completion "get bucket policy" conn
       ~operation:Operation.Get_bucket_policy (fun () ->
         Simulator.Bucket.Policy.get conn ~bucket ())
    |> ok_or_fail "get bucket policy");
  ignore
    (check_exact_completion "delete bucket policy" conn
       ~operation:Operation.Delete_bucket_policy (fun () ->
         Simulator.Bucket.Policy.delete conn ~bucket ())
    |> ok_or_fail "delete bucket policy");
  List.iter
    (fun (key, contents) ->
      ignore
        (Simulator.Object.put_string conn ~bucket ~key:(object_key key)
           ~contents ()
        |> ok_or_fail ("put " ^ key)))
    [ ("delete-one", "one"); ("delete-two", "two") ];
  let objects =
    [
      Awskit_s3.Object.Delete_many.object_ ~key:(object_key "delete-one") ();
      Awskit_s3.Object.Delete_many.object_ ~key:(object_key "delete-two") ();
    ]
  in
  let delete_many =
    check_exact_completion "delete objects" conn
      ~operation:Operation.Delete_objects (fun () ->
        Simulator.Object.delete_objects conn ~bucket ~objects ())
    |> ok_or_fail "delete objects"
  in
  Alcotest.(check int)
    "delete objects deleted count" 2
    (List.length delete_many.deleted);
  let upload =
    check_exact_completion "create multipart" conn
      ~operation:Operation.Create_multipart_upload (fun () ->
        Simulator.Multipart.create_upload conn ~bucket
          ~key:(object_key "abort-me") ())
    |> ok_or_fail "create abort upload"
  in
  ignore
    (check_exact_completion "abort multipart" conn
       ~operation:Operation.Abort_multipart_upload (fun () ->
         Simulator.Multipart.abort_upload conn ~upload:upload.upload ())
    |> ok_or_fail "abort multipart");
  ignore
    (check_exact_completion "delete bucket" conn
       ~operation:Operation.Delete_bucket (fun () ->
         Simulator.Bucket.delete conn ~bucket ())
    |> ok_or_fail "delete exact-leaves bucket")

let suite =
  [
    ( "behavior:awskit-s3-sim:observability",
      [
        Alcotest.test_case "history and canonical completion parity" `Quick
          test_history_has_canonical_parity;
        Alcotest.test_case "put and get logical bytes" `Quick
          test_put_and_get_logical_bytes;
        Alcotest.test_case "partial consumption excludes drain" `Quick
          test_partial_consumption_excludes_drain;
        Alcotest.test_case "consumer failures omit response bytes" `Quick
          test_consumer_failures_omit_response_bytes;
        Alcotest.test_case "not found omits response bytes" `Quick
          test_not_found_omits_response_bytes;
        Alcotest.test_case "multipart logical request bytes" `Quick
          test_multipart_upload_logical_request_bytes;
        Alcotest.test_case "fault outcome" `Quick test_fault_outcome;
        Alcotest.test_case "connection isolation" `Quick
          test_connection_isolation;
        Alcotest.test_case "validation and callback exception" `Quick
          test_validation_and_callback_exception;
        Alcotest.test_case "leaf operations and pagination" `Quick
          test_leaf_operations_and_pagination;
        Alcotest.test_case "core policy delete-many abort exact completions"
          `Quick test_core_policy_delete_many_abort_exact_completions;
        Alcotest.test_case "presigned artifact topology" `Quick
          test_presigned_artifact_topology;
        Alcotest.test_case "presigned artifact failure topology" `Quick
          test_presigned_artifact_failure_topology;
        Alcotest.test_case "precondition failure completion" `Quick
          test_precondition_failure_completion;
        Alcotest.test_case "request producer failure completion" `Quick
          test_request_body_producer_failure_completion;
        Alcotest.test_case "paginator validation has no aggregate completion"
          `Quick test_paginator_validation_has_no_aggregate_completion;
      ] );
  ]
