open Awskit_s3
open Awskit_s3_test

let test_sim_request_body_requires_known_length () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket:"test-bucket" ()
    |> ok_or_fail "bucket");
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
  (match
     Simulator.Object.put conn ~bucket:"test-bucket" ~key:"unknown" ~body ()
   with
  | Error (Awskit.Error.Validation { field = Some "content_length"; _ }) -> ()
  | Error error ->
      Alcotest.failf "unexpected sim unknown-length put error: %a" Error.pp
        error
  | Ok _ -> Alcotest.fail "expected sim unknown-length object failure");
  (match Simulator.Object.head conn ~bucket:"test-bucket" ~key:"unknown" () with
  | Error error when Error.is_not_found error -> ()
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown-length object to be absent");
  let created =
    Simulator.Multipart.create_upload conn ~bucket:"test-bucket"
      ~key:"large.bin" ()
    |> ok_or_fail "create multipart upload"
  in
  let upload_id = created.upload.upload_id in
  (match
     Simulator.Multipart.upload_part conn ~bucket:"test-bucket" ~key:"large.bin"
       ~upload_id ~part_number:1 ~body ()
   with
  | Error (Awskit.Error.Validation { field = Some "content_length"; _ }) -> ()
  | Error error ->
      Alcotest.failf "unexpected sim unknown-length part error: %a" Error.pp
        error
  | Ok _ -> Alcotest.fail "expected sim unknown-length multipart failure");
  let listed =
    Simulator.Multipart.list_parts conn ~bucket:"test-bucket" ~key:"large.bin"
      ~upload_id ()
    |> ok_or_fail "list multipart parts"
  in
  Alcotest.(check int) "stored parts" 0 (List.length listed.parts)

let test_sim_stream_request_body_error_propagates () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = "stream-error-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let stream_error = Awskit.Error.body "sim stream request body failed" in
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some 4L;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  (match Simulator.Object.put conn ~bucket ~key:"bad" ~body () with
  | Error error when Awskit.Error.equal error stream_error -> ()
  | Error error -> Alcotest.failf "unexpected put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream request body error");
  match Simulator.Object.head conn ~bucket ~key:"bad" () with
  | Error error when Error.is_not_found error -> ()
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream error object to be absent"

let test_sim_stream_request_body_rejects_length_mismatch () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = "stream-length-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some 4L;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let short_body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        Simulator.Runtime.Request_body.write_string writer "ab")
  in
  (match Simulator.Object.put conn ~bucket ~key:"short" ~body:short_body () with
  | Error (Awskit.Error.Body _) -> ()
  | Error error ->
      Alcotest.failf "unexpected short put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected short request body error");
  let long_body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "abcd" with
        | Error _ as error -> error
        | Ok () -> Simulator.Runtime.Request_body.write_string writer "e")
  in
  (match Simulator.Object.put conn ~bucket ~key:"long" ~body:long_body () with
  | Error (Awskit.Error.Body _) -> ()
  | Error error -> Alcotest.failf "unexpected long put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected long request body error");
  (match Simulator.Object.head conn ~bucket ~key:"short" () with
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "unexpected short head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected short body object to be absent");
  match Simulator.Object.head conn ~bucket ~key:"long" () with
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "unexpected long head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected long body object to be absent"

let test_sim_multipart_upload_part_stream_error_does_not_store_part () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket:"test-bucket" ()
    |> ok_or_fail "bucket");
  let created =
    Simulator.Multipart.create_upload conn ~bucket:"test-bucket"
      ~key:"large.bin" ()
    |> ok_or_fail "create multipart upload"
  in
  let upload_id = created.upload.upload_id in
  let stream_error = Awskit.Error.body "sim multipart request body failed" in
  let descriptor : Awskit.Body.Request.descriptor =
    {
      content_length = Some 4L;
      payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
      replayable = false;
    }
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  (match
     Simulator.Multipart.upload_part conn ~bucket:"test-bucket" ~key:"large.bin"
       ~upload_id ~part_number:1 ~body ()
   with
  | Error error when Error.equal error stream_error -> ()
  | Error error ->
      Alcotest.failf "unexpected upload part error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream request body error");
  let listed =
    Simulator.Multipart.list_parts conn ~bucket:"test-bucket" ~key:"large.bin"
      ~upload_id ()
    |> ok_or_fail "list multipart parts"
  in
  Alcotest.(check int) "stored parts" 0 (List.length listed.parts)

let test_sim_public_helper_surface () =
  let conn = make_sim () in
  let store = Simulator.store conn in
  let put =
    Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"ok.txt" "hello"
    |> ok_or_fail "put ok"
  in
  (match
     (Simulator.object_metadata store ~bucket:"test-bucket" ~key:"ok.txt"
       : Simulator.object_metadata option)
   with
  | None -> Alcotest.fail "expected object metadata"
  | Some metadata -> (
      Alcotest.(check (option int64)) "metadata size" (Some 5L) metadata.size;
      Alcotest.(check (option string))
        "metadata etag"
        (Option.map Object.Etag.to_string put.etag)
        (Option.map Object.Etag.to_string metadata.etag);
      match metadata.last_modified with
      | Some last_modified when Ptime.equal last_modified test_time -> ()
      | _ -> Alcotest.fail "expected metadata last modified"));
  Alcotest.(check (list (pair string string)))
    "objects as strings"
    [ ("ok.txt", "hello") ]
    (Simulator.objects_as_strings store ~bucket:"test-bucket");
  Alcotest.(check bool)
    "missing object metadata" true
    (Option.is_none
       (Simulator.object_metadata store ~bucket:"test-bucket" ~key:"missing"));
  Simulator.enable_random_faults conn ~seed:7 ~prob:1.0;
  (match
     Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"faulted.txt"
       "faulted"
   with
  | Error error when Error.service_code error = Some "InternalError" -> ()
  | Error error -> Alcotest.failf "unexpected random fault: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected random fault");
  Simulator.disable_random_faults conn;
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"after.txt"
       "after"
    |> ok_or_fail "put after disabling random faults");
  Alcotest.(check (list (pair string string)))
    "objects after random fault"
    [ ("after.txt", "after"); ("ok.txt", "hello") ]
    (Simulator.objects_as_strings store ~bucket:"test-bucket")

let sim_operation_name (_ : Simulator.operation_record) = function
  | `Put_object | `Get_object | `Head_object | `Delete_object | `List_objects_v2
  | `List_object_versions | `Copy_object | `Delete_objects
  | `Create_multipart_upload | `Upload_part | `Complete_multipart_upload
  | `Abort_multipart_upload | `List_parts ->
      ()

let test_sim_history_uses_operation_names () =
  let conn = make_sim () in
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"history.txt"
       "history"
    |> ok_or_fail "put history");
  match Simulator.history (Simulator.store conn) with
  | record :: _ -> sim_operation_name record record.op
  | [] -> Alcotest.fail "expected simulator history record"

let test_sim_buffer_roundtrip () =
  let conn = make_sim () in
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
      value = "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=";
    }
  in
  let put =
    Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~options:
        {
          Put_object.default_options with
          content_type = Some "text/plain";
          checksum = Some checksum;
        }
      "hello"
    |> ok_or_fail "put"
  in
  Alcotest.(check bool) "etag" true (Option.is_some put.etag);
  check_checksum "put checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
  let info, body =
    Simulator.Object.get_as_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~max_bytes:16L ()
    |> ok_or_fail "get"
  in
  Alcotest.(check string) "body" "hello" body;
  Alcotest.(check (option string))
    "content-type" (Some "text/plain") info.content_type;
  check_checksum "get checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" info.checksum;
  let head =
    Simulator.Object.head conn ~bucket:"test-bucket" ~key:"hello.txt" ()
    |> ok_or_fail "head checksum"
  in
  check_checksum "head checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" head.checksum;
  let page =
    Simulator.Object.list conn ~bucket:"test-bucket" ()
    |> ok_or_fail "list checksum"
  in
  match page.objects with
  | [ object_ ] ->
      Alcotest.(check string) "listed key" "hello.txt" object_.key;
      Alcotest.(check (option int64)) "listed size" (Some 5L) object_.size
  | _ -> Alcotest.fail "expected one listed object"

let test_sim_rejects_unknown_checksum_writes () =
  let conn = make_sim () in
  let checksum : Object.Checksum.value =
    {
      Object.Checksum.algorithm = Object.Checksum.Algorithm.Unknown "FUTURE";
      value = "value";
    }
  in
  let expect_checksum_validation label = function
    | Error (Awskit.Error.Validation { field = Some "checksum_algorithm"; _ })
      ->
        ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected checksum validation" label
  in
  expect_checksum_validation "sim put"
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"bad.txt"
       ~options:{ Put_object.default_options with checksum = Some checksum }
       "body");
  let copy_options =
    {
      Copy_object.default_options with
      checksum_algorithm = Some (Object.Checksum.Algorithm.Unknown "FUTURE");
    }
  in
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"source.txt"
       "body"
    |> ok_or_fail "source");
  expect_checksum_validation "sim copy"
    (Simulator.Object.copy conn ~source_bucket:"test-bucket"
       ~source_key:"source.txt" ~destination_bucket:"test-bucket"
       ~destination_key:"copy.txt" ~options:copy_options ());
  let upload =
    Simulator.Multipart.create_upload conn ~bucket:"test-bucket" ~key:"bad.bin"
      ()
    |> ok_or_fail "create upload"
  in
  let upload_id = upload.upload.upload_id in
  let upload_part_options =
    { Upload_part.checksum = Some checksum; expected_bucket_owner = None }
  in
  expect_checksum_validation "sim upload part"
    (Simulator.Multipart.upload_part conn ~bucket:"test-bucket" ~key:"bad.bin"
       ~upload_id ~part_number:1
       ~body:(Simulator.Runtime.Request_body.of_string "body")
       ~options:upload_part_options ());
  let part =
    Multipart.Part.create_exn ~part_number:1
      ~etag:(Object.Etag.of_string_exn "\"etag\"")
      ()
  in
  let complete_options =
    {
      Complete_multipart_upload.expected_bucket_owner = None;
      checksum = Some checksum;
      checksum_type = None;
      multipart_object_size = None;
    }
  in
  expect_checksum_validation "sim complete checksum"
    (Simulator.Multipart.complete_upload conn ~bucket:"test-bucket"
       ~key:"bad.bin" ~upload_id ~options:complete_options [ part ]);
  let complete_options =
    {
      Complete_multipart_upload.default_options with
      checksum_type = Some (Object.Checksum.Type.Unknown "FUTURE");
    }
  in
  match
    Simulator.Multipart.complete_upload conn ~bucket:"test-bucket"
      ~key:"bad.bin" ~upload_id ~options:complete_options [ part ]
  with
  | Error (Awskit.Error.Validation { field = Some "checksum_type"; _ }) -> ()
  | Error error ->
      Alcotest.failf "sim complete checksum type: unexpected error: %a" Error.pp
        error
  | Ok _ -> Alcotest.fail "sim complete checksum type: expected validation"

let test_sim_streaming_get () =
  let conn = make_sim () in
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"stream"
       "abcdef"
    |> ok_or_fail "put");
  let consume reader =
    let bytes = Bytes.create 3 in
    match Simulator.Runtime.Response_body.read reader bytes ~off:0 ~len:3 with
    | Error _ as error -> error
    | Ok read -> Ok (Bytes.sub_string bytes 0 read)
  in
  let _info, body =
    Simulator.Object.get conn ~bucket:"test-bucket" ~key:"stream" ~consume ()
    |> ok_or_fail "stream get"
  in
  Alcotest.(check string) "partial body" "abc" body

let test_buffer_limit () =
  let conn = make_sim () in
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"large"
       "abcdef"
    |> ok_or_fail "put");
  match
    Simulator.Object.get_as_string conn ~bucket:"test-bucket" ~key:"large"
      ~max_bytes:3L ()
  with
  | Error (Awskit.Error.Body _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_bytes failure"

let test_sim_paginator_keys () =
  let conn = make_sim () in
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"logs/a.txt"
       "a"
    |> ok_or_fail "put a");
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"logs/b.txt"
       "b"
    |> ok_or_fail "put b");
  ignore
    (Simulator.Object.put_string conn ~bucket:"test-bucket" ~key:"other.txt"
       "other"
    |> ok_or_fail "put other");
  let options =
    {
      List_objects_v2.default_options with
      prefix = Some "logs/";
      max_keys = Some 1;
    }
  in
  let keys =
    Simulator.Object.List_objects_v2.keys conn ~bucket:"test-bucket" ~options ()
    |> ok_or_fail "sim paginator keys"
  in
  Alcotest.(check (list string)) "keys" [ "logs/a.txt"; "logs/b.txt" ] keys

let suite =
  [
    ( "simulator",
      [
        Alcotest.test_case "sim request body requires known length" `Quick
          test_sim_request_body_requires_known_length;
        Alcotest.test_case "sim stream request body error propagates" `Quick
          test_sim_stream_request_body_error_propagates;
        Alcotest.test_case "sim stream request body rejects length mismatch"
          `Quick test_sim_stream_request_body_rejects_length_mismatch;
        Alcotest.test_case
          "sim multipart upload part stream error does not store part" `Quick
          test_sim_multipart_upload_part_stream_error_does_not_store_part;
        Alcotest.test_case "sim public helper surface" `Quick
          test_sim_public_helper_surface;
        Alcotest.test_case "sim history uses operation names" `Quick
          test_sim_history_uses_operation_names;
        Alcotest.test_case "sim in-memory roundtrip" `Quick
          test_sim_buffer_roundtrip;
        Alcotest.test_case "sim rejects unknown checksum writes" `Quick
          test_sim_rejects_unknown_checksum_writes;
        Alcotest.test_case "sim streaming get" `Quick test_sim_streaming_get;
        Alcotest.test_case "in-memory helper limit" `Quick test_buffer_limit;
        Alcotest.test_case "sim paginator keys" `Quick test_sim_paginator_keys;
      ] );
  ]
