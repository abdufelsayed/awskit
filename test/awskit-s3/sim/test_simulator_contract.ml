open Awskit_s3
open Awskit_s3_test
open Support

module Simulator_subject = struct
  type connection = Simulator.t
  type 'a io = 'a
  type request_body = Simulator.Body.t
  type response_body_reader = Simulator.Reader.t

  module Runtime = Simulator.Runtime

  module Body = struct
    type 'a io = 'a

    include Simulator.Body
  end

  module Reader = struct
    type 'a io = 'a

    include Simulator.Reader
  end

  module Object = struct
    type connection = Simulator.t
    type 'a io = 'a
    type request_body = Body.t
    type response_body_reader = Reader.t

    include Simulator.Object
  end

  module Bucket = struct
    type connection = Simulator.t
    type 'a io = 'a

    include Simulator.Bucket
  end

  module Multipart = struct
    type connection = Simulator.t
    type 'a io = 'a
    type request_body = Body.t

    include Simulator.Multipart
  end

  module Presigned = struct
    type connection = Simulator.t
    type 'a io = 'a

    include Simulator.Presigned
  end

  let bucket = bucket_name "contract-bucket"
  let capabilities = S3_contract.strict_capabilities
  let expected_capability_differences = []

  let fresh () =
    let clock = Simulator.Clock.create ~now:test_time () in
    let store = Simulator.create_store ~clock () in
    Simulator.connect store ~credentials

  let cleanup _conn = ()
  let run value = value
  let read_response_body = Simulator.Reader.read
end

module Simulator_contract = S3_contract.Make (Simulator_subject)

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let test_simulator_slow_down_fault () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "contract-bucket") ()
    |> ok_or_fail "create bucket");
  Simulator.inject_fault conn Simulator.Slow_down;
  (match
     Simulator.Object.put conn
       ~bucket:(bucket_name "contract-bucket")
       ~key:(object_key "fault")
       ~body:(Simulator.Body.of_string "body")
       ()
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected fault: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected injected SlowDown");
  match Simulator.history (Simulator.store conn) with
  | [ record ] ->
      Alcotest.(check bool) "faulted" true record.faulted;
      Alcotest.(check string) "fault key" "fault" (Option.get record.key)
  | _ -> Alcotest.fail "expected one faulted history record"

let test_simulator_response_lost_fault () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "contract-bucket") ()
    |> ok_or_fail "create bucket");
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "contract-bucket")
       ~key:(object_key "body")
       ~body:(Simulator.Body.of_string "abcdef")
       ()
    |> ok_or_fail "put body");
  Simulator.inject_fault conn Simulator.Response_lost;
  match
    Simulator.Object.get conn
      ~bucket:(bucket_name "contract-bucket")
      ~key:(object_key "body")
      ~consume:(Simulator.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected response-lost body error"

let test_s3_operation_context_for_validation_error () =
  let conn = Simulator_subject.fresh () in
  let bucket = bucket_name "test-bucket" in
  let key = object_key "k" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = None;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        Simulator.Runtime.Request_body.write_string writer "body")
  in
  match Simulator.Object.put conn ~bucket ~key ~body () with
  | Ok _ -> Alcotest.fail "expected content-length validation"
  | Error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions operation" true
        (string_contains text ~substring:"PutObject");
      Alcotest.(check bool)
        "mentions resource" true
        (string_contains text ~substring:"s3://test-bucket/k")

let test_find_metadata_missing_object_returns_none () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "create bucket");
  match
    Simulator.Object.find_metadata conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "missing") ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected None for missing object"
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error

let test_find_metadata_missing_bucket_returns_error () =
  let conn = Simulator_subject.fresh () in
  match
    Simulator.Object.find_metadata conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when Error.is_no_such_bucket error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok None -> Alcotest.fail "expected missing bucket error, got None"
  | Ok (Some _) -> Alcotest.fail "expected missing bucket error"

let test_exists_missing_object_returns_false () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "create bucket");
  match
    Simulator.Object.exists conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "missing") ()
  with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "expected false for missing object"
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error

let test_exists_missing_bucket_returns_error () =
  let conn = Simulator_subject.fresh () in
  match
    Simulator.Object.exists conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file") ()
  with
  | Error error when Error.is_no_such_bucket error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok false -> Alcotest.fail "expected missing bucket error, got false"
  | Ok true -> Alcotest.fail "expected missing bucket error"

let test_find_missing_object_returns_none () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "create bucket");
  match
    Simulator.Object.find conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "missing")
      ~consume:(fun _reader -> Ok "unused")
      ()
  with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "expected None for missing object"
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error

let test_find_missing_bucket_returns_error () =
  let conn = Simulator_subject.fresh () in
  match
    Simulator.Object.find conn
      ~bucket:(bucket_name "missing-bucket")
      ~key:(object_key "file")
      ~consume:(fun _reader -> Ok "unused")
      ()
  with
  | Error error when Error.is_no_such_bucket error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok None -> Alcotest.fail "expected missing bucket error, got None"
  | Ok (Some _) -> Alcotest.fail "expected missing bucket error"

let test_find_preserves_consumer_not_found_error () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "create bucket");
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "present")
       ~body:(Simulator.Body.of_string "body")
       ()
    |> ok_or_fail "put object");
  let consumer_error =
    Awskit.Error.Producer.service ~status:404 ~code:"NoSuchKey"
      ~message:"consumer-owned missing resource" ~headers:[] ()
  in
  match
    Simulator.Object.find conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "present")
      ~consume:(fun _reader -> Error consumer_error)
      ()
  with
  | Error error when error == consumer_error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok None -> Alcotest.fail "expected consumer error, got None"
  | Ok (Some _) -> Alcotest.fail "expected consumer error"

let suite =
  List.map
    (fun (name, cases) -> ("simulator " ^ name, cases))
    Simulator_contract.suites
  @ [
      ( "simulator faults",
        [
          Alcotest.test_case "slow down" `Quick test_simulator_slow_down_fault;
          Alcotest.test_case "response lost" `Quick
            test_simulator_response_lost_fault;
          Alcotest.test_case "operation context for validation" `Quick
            test_s3_operation_context_for_validation_error;
          Alcotest.test_case "find metadata missing object returns none" `Quick
            test_find_metadata_missing_object_returns_none;
          Alcotest.test_case "find metadata missing bucket returns error" `Quick
            test_find_metadata_missing_bucket_returns_error;
          Alcotest.test_case "exists missing object returns false" `Quick
            test_exists_missing_object_returns_false;
          Alcotest.test_case "exists missing bucket returns error" `Quick
            test_exists_missing_bucket_returns_error;
          Alcotest.test_case "find missing object returns none" `Quick
            test_find_missing_object_returns_none;
          Alcotest.test_case "find missing bucket returns error" `Quick
            test_find_missing_bucket_returns_error;
          Alcotest.test_case "find preserves consumer not found error" `Quick
            test_find_preserves_consumer_not_found_error;
        ] );
    ]
