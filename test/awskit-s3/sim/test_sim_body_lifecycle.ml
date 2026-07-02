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

let tag_set = Tag.Set.of_list_exn [ Tag.create_exn ~key:"env" ~value:"test" ]

let bucket_policy =
  Policy.of_json {|{"Version":"2012-10-17","Statement":[]}|}
  |> ok_or_fail "bucket policy"

let bucket_encryption =
  {
    Bucket.Encryption.rules =
      [
        {
          sse_algorithm = Some Bucket.Encryption.Algorithm.Aes256;
          kms_master_key_id = None;
          bucket_key_enabled = None;
          blocked_encryption_types = [];
        };
      ];
  }

let bucket_cors =
  {
    Bucket.Cors.rules =
      [
        {
          id = None;
          allowed_origins = [ "https://example.com" ];
          allowed_methods = [ Bucket.Cors.Method.Get ];
          allowed_headers = [];
          expose_headers = [];
          max_age_seconds = None;
        };
      ];
  }

let bucket_ownership_controls =
  {
    Bucket.Ownership_controls.object_ownership =
      Bucket.Ownership_controls.Object_ownership.Bucket_owner_enforced;
  }

let test_invalid_string_inputs_do_not_record_history () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  ignore (put_string conn "tagged.txt" "body");
  Simulator.clear_history store;
  expect_validation_field "invalid put bucket" "bucket"
    (Simulator.Object.put_string conn ~bucket:"Invalid" ~key:"file.txt"
       ~contents:"body" ());
  expect_validation_field "invalid create upload key" "key"
    (Simulator.Multipart.create_upload conn ~bucket:"test-bucket" ~key:"" ());
  expect_validation_field ~operation:"CreateBucket" "invalid bucket create"
    "bucket"
    (Simulator.Bucket.create conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetObjectTagging"
    "invalid object tagging bucket" "bucket"
    (Simulator.Object.Tagging.get conn ~bucket:"Invalid" ~key:"tagged.txt" ());
  expect_validation_field ~operation:"PutObjectTagging"
    "invalid object tagging key" "key"
    (Simulator.Object.Tagging.put conn ~bucket:"test-bucket" ~key:""
       ~tags:tag_set ());
  expect_validation_field ~operation:"DeleteObjectTagging"
    "invalid delete object tagging key" "key"
    (Simulator.Object.Tagging.delete conn ~bucket:"test-bucket" ~key:"" ());
  expect_validation_field ~operation:"HeadBucket" "invalid bucket head" "bucket"
    (Simulator.Bucket.head conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"DeleteBucket" "invalid bucket delete"
    "bucket"
    (Simulator.Bucket.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"HeadBucket" "invalid bucket exists"
    "bucket"
    (Simulator.Bucket.exists conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketLocation"
    "invalid bucket location" "bucket"
    (Simulator.Bucket.get_location conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketPolicy" "invalid policy get"
    "bucket"
    (Simulator.Bucket.Policy.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketPolicy" "invalid policy put"
    "bucket"
    (Simulator.Bucket.Policy.put conn ~bucket:"Invalid" ~policy:bucket_policy ());
  expect_validation_field ~operation:"DeleteBucketPolicy"
    "invalid policy delete" "bucket"
    (Simulator.Bucket.Policy.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketVersioning"
    "invalid versioning get" "bucket"
    (Simulator.Bucket.Versioning.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketVersioning"
    "invalid versioning put" "bucket"
    (Simulator.Bucket.Versioning.put conn ~bucket:"Invalid"
       ~status:Bucket.Versioning.Status.Enabled ());
  expect_validation_field ~operation:"GetBucketTagging"
    "invalid bucket tagging get" "bucket"
    (Simulator.Bucket.Tagging.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketTagging"
    "invalid bucket tagging put" "bucket"
    (Simulator.Bucket.Tagging.put conn ~bucket:"Invalid" ~tags:tag_set ());
  expect_validation_field ~operation:"DeleteBucketTagging"
    "invalid bucket tagging delete" "bucket"
    (Simulator.Bucket.Tagging.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketEncryption"
    "invalid encryption get" "bucket"
    (Simulator.Bucket.Encryption.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketEncryption"
    "invalid encryption put" "bucket"
    (Simulator.Bucket.Encryption.put conn ~bucket:"Invalid"
       ~config:bucket_encryption ());
  expect_validation_field ~operation:"DeleteBucketEncryption"
    "invalid encryption delete" "bucket"
    (Simulator.Bucket.Encryption.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketCors" "invalid cors get" "bucket"
    (Simulator.Bucket.Cors.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketCors" "invalid cors put" "bucket"
    (Simulator.Bucket.Cors.put conn ~bucket:"Invalid" ~config:bucket_cors ());
  expect_validation_field ~operation:"DeleteBucketCors" "invalid cors delete"
    "bucket"
    (Simulator.Bucket.Cors.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetPublicAccessBlock"
    "invalid access block get" "bucket"
    (Simulator.Bucket.Public_access_block.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutPublicAccessBlock"
    "invalid access block put" "bucket"
    (Simulator.Bucket.Public_access_block.put conn ~bucket:"Invalid"
       ~config:Bucket.Public_access_block.all_false ());
  expect_validation_field ~operation:"DeletePublicAccessBlock"
    "invalid access block delete" "bucket"
    (Simulator.Bucket.Public_access_block.delete conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"GetBucketOwnershipControls"
    "invalid ownership controls get" "bucket"
    (Simulator.Bucket.Ownership_controls.get conn ~bucket:"Invalid" ());
  expect_validation_field ~operation:"PutBucketOwnershipControls"
    "invalid ownership controls put" "bucket"
    (Simulator.Bucket.Ownership_controls.put conn ~bucket:"Invalid"
       ~config:bucket_ownership_controls ());
  expect_validation_field ~operation:"DeleteBucketOwnershipControls"
    "invalid ownership controls delete" "bucket"
    (Simulator.Bucket.Ownership_controls.delete conn ~bucket:"Invalid" ());
  Alcotest.(check int)
    "recorded operations" 0
    (List.length (Simulator.history store))

let delete_object key = Object.Delete_objects.object_exn ~key ()

let expect_only_object conn label =
  Alcotest.(check (list (pair string string)))
    label
    [ ("kept.txt", "body") ]
    (Simulator.objects_as_strings (Simulator.store conn)
       ~bucket:(bucket_name "test-bucket"))

let oversized_delete_objects () =
  delete_object "kept.txt"
  :: List.init Object.Delete_objects.max_objects (fun index ->
      delete_object (Fmt.str "too-many-%04d" index))

let test_delete_objects_count_validation_preserves_state () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  ignore (put_string conn "kept.txt" "body");
  Simulator.clear_history store;
  Simulator.inject_fault conn Simulator.Internal_error;
  expect_validation_field "empty delete objects" "objects"
    (Simulator.Object.delete_objects conn ~bucket:"Invalid" ~objects:[] ());
  expect_only_object conn "after empty batch";
  Alcotest.(check int)
    "empty batch recorded operations" 0
    (List.length (Simulator.history store));
  expect_validation_field "oversized delete objects" "objects"
    (Simulator.Object.delete_objects conn
       ~bucket:(bucket_name "test-bucket")
       ~objects:(oversized_delete_objects ())
       ());
  expect_only_object conn "after oversized batch";
  Alcotest.(check int)
    "oversized batch recorded operations" 0
    (List.length (Simulator.history store));
  expect_service_code "fault still queued" "InternalError"
    (Simulator.Object.delete_objects conn
       ~bucket:(bucket_name "test-bucket")
       ~objects:[ delete_object "kept.txt" ]
       ());
  expect_only_object conn "after queued fault"

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
        Alcotest.test_case "delete objects count validation preserves state"
          `Quick test_delete_objects_count_validation_preserves_state;
        Alcotest.test_case "reader scope closes" `Quick
          test_response_reader_cannot_escape_scope;
      ] );
  ]
