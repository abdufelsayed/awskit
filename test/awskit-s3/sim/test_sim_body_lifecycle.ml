open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let stream_body ?content_length write =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ?content_length
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  Simulator.Runtime.Request_body.of_stream descriptor ~write

let test_request_body_requires_known_length () =
  let conn = make_simulator () in
  let body =
    stream_body (fun writer ->
        Simulator.Runtime.Request_body.write_string writer "body")
  in
  expect_validation_field "unknown length put" "content_length"
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "unknown") ~body ());
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "large.bin") ()
    |> ok_or_fail "create upload"
  in
  expect_validation_field "unknown length part" "content_length"
    (Simulator.Multipart.upload_part conn ~upload:created.upload ~part_number:1
       ~body ())

let test_request_body_rejects_length_mismatch () =
  let conn = make_simulator () in
  let short =
    stream_body ~content_length:4L (fun writer ->
        Simulator.Runtime.Request_body.write_string writer "ab")
  in
  expect_body_error "short body"
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "short") ~body:short ());
  let long =
    stream_body ~content_length:4L (fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "abcd" with
        | Error _ as error -> error
        | Ok () -> Simulator.Runtime.Request_body.write_string writer "e")
  in
  expect_body_error "long body"
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "long") ~body:long ())

let test_stream_error_does_not_store_part () =
  let conn = make_simulator () in
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "stream-error.bin")
      ()
    |> ok_or_fail "create upload"
  in
  let stream_error = Awskit.Error.Producer.body "multipart stream failed" in
  let body =
    stream_body ~content_length:4L (fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  expect_body_error "part stream error"
    (Simulator.Multipart.upload_part conn ~upload:created.upload ~part_number:1
       ~body ());
  let listed =
    Simulator.Multipart.list_parts conn ~upload:created.upload ()
    |> ok_or_fail "list parts"
  in
  Alcotest.(check int) "stored parts" 0 (List.length listed.parts)

let test_stream_error_does_not_store_object () =
  let conn = make_simulator () in
  let stream_error = Awskit.Error.Producer.body "put stream failed" in
  let body =
    stream_body ~content_length:4L (fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  expect_body_error "put stream error"
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "stream-error")
       ~body ());
  Alcotest.(check (list (pair string string)))
    "stored objects" []
    (Simulator.objects_as_strings (Simulator.store conn)
       ~bucket:(bucket_name "test-bucket"))

let test_get_string_max_bytes_preserves_object () =
  let conn = make_simulator () in
  ignore (put_string conn "oversized" "abcdef");
  expect_body_error "max bytes"
    (Simulator.Object.get_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "oversized") ~max_bytes:3L ());
  let result =
    Simulator.Object.get_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "oversized") ~max_bytes:16L ()
    |> ok_or_fail "get after oversized read"
  in
  Alcotest.(check string) "body" "abcdef" result.value

let test_invalid_string_inputs_do_not_record_history () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  Simulator.clear_history store;
  expect_validation_field "invalid put bucket" "bucket"
    (Simulator.Object.put_string conn ~bucket:"Invalid" ~key:"file.txt"
       ~contents:"body" ());
  expect_validation_field "invalid create upload key" "key"
    (Simulator.Multipart.create_upload conn ~bucket:"test-bucket" ~key:"" ());
  expect_validation_field "invalid bucket create" "bucket"
    (Simulator.Bucket.create conn ~bucket:"Invalid" ());
  Alcotest.(check int)
    "recorded operations" 0
    (List.length (Simulator.history store))

let test_response_reader_cannot_escape_scope () =
  let conn = make_simulator () in
  ignore (put_string conn "reader" "abc");
  let escaped = ref None in
  ignore
    (Simulator.Object.get conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "reader")
       ~consume:(fun reader ->
         escaped := Some reader;
         Ok ())
       ()
    |> ok_or_fail "get reader");
  match !escaped with
  | None -> Alcotest.fail "expected escaped reader"
  | Some reader ->
      let bytes = Bytes.create 1 in
      expect_body_error "escaped reader"
        (Simulator.Reader.read reader bytes ~off:0 ~len:1)

let suite =
  [
    ( "contract:awskit-s3-sim:body-lifecycle",
      [
        Alcotest.test_case "requires known length" `Quick
          test_request_body_requires_known_length;
        Alcotest.test_case "rejects length mismatch" `Quick
          test_request_body_rejects_length_mismatch;
        Alcotest.test_case "stream error leaves no part" `Quick
          test_stream_error_does_not_store_part;
        Alcotest.test_case "stream error leaves no object" `Quick
          test_stream_error_does_not_store_object;
        Alcotest.test_case "max-bytes failure preserves object" `Quick
          test_get_string_max_bytes_preserves_object;
        Alcotest.test_case "invalid strings record no history" `Quick
          test_invalid_string_inputs_do_not_record_history;
        Alcotest.test_case "reader scope closes" `Quick
          test_response_reader_cannot_escape_scope;
      ] );
  ]
