open Awskit_s3
open Awskit_s3_test
open Support

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let expect_body_error label = function
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body error" label

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let test_simulator_request_body_requires_known_length () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "bucket");
  let descriptor : Awskit.Body.Request.descriptor =
    Awskit.Body.Request.descriptor_exn
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        Simulator.Runtime.Request_body.write_string writer "body")
  in
  (match
     Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "unknown") ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error ->
      Alcotest.failf "unexpected simulator unknown-length put error: %a"
        Error.pp error
  | Ok _ -> Alcotest.fail "expected simulator unknown-length object failure");
  (match
     Simulator.Object.head conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "unknown") ()
   with
  | Error error when Error.is_not_found error -> ()
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown-length object to be absent");
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "large.bin") ()
    |> ok_or_fail "create multipart upload"
  in
  (match
     Simulator.Multipart.upload_part conn ~upload:created.upload
       ~part_number:(Multipart.Part_number.of_int_exn 1)
       ~body ()
   with
  | Error error when is_validation_field "content_length" error -> ()
  | Error error ->
      Alcotest.failf "unexpected simulator unknown-length part error: %a"
        Error.pp error
  | Ok _ -> Alcotest.fail "expected simulator unknown-length multipart failure");
  let listed =
    Simulator.Multipart.list_parts conn ~upload:created.upload ()
    |> ok_or_fail "list multipart parts"
  in
  Alcotest.(check int) "stored parts" 0 (List.length listed.parts)

let test_simulator_bucket_validation_matches_core_rules () =
  let invalid_names =
    [ "001.002.003.004"; "xn--reserved"; "bucket--x-s3"; "bucket-s3alias" ]
  in
  List.iter
    (fun bucket ->
      match Bucket_name.of_string bucket with
      | Ok _ -> Alcotest.failf "%s should be invalid in core validation" bucket
      | Error _ -> ())
    invalid_names

let test_simulator_stream_request_body_error_propagates () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = bucket_name "stream-error-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let stream_error =
    Awskit.Error.Producer.body "simulator stream request body failed"
  in
  let descriptor : Awskit.Body.Request.descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  (match Simulator.Object.put conn ~bucket ~key:(object_key "bad") ~body () with
  | Error error
    when is_body_error error
         && string_contains
              (Awskit.Error.to_string_hum error)
              ~substring:"simulator stream request body failed" ->
      ()
  | Error error -> Alcotest.failf "unexpected put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream request body error");
  match Simulator.Object.head conn ~bucket ~key:(object_key "bad") () with
  | Error error when Error.is_not_found error -> ()
  | Error error -> Alcotest.failf "unexpected head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream error object to be absent"

let test_simulator_stream_request_body_rejects_length_mismatch () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  let bucket = bucket_name "stream-length-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let descriptor : Awskit.Body.Request.descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let short_body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        Simulator.Runtime.Request_body.write_string writer "ab")
  in
  (match
     Simulator.Object.put conn ~bucket ~key:(object_key "short")
       ~body:short_body ()
   with
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "unexpected short put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected short request body error");
  let long_body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "abcd" with
        | Error _ as error -> error
        | Ok () -> Simulator.Runtime.Request_body.write_string writer "e")
  in
  (match
     Simulator.Object.put conn ~bucket ~key:(object_key "long") ~body:long_body
       ()
   with
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "unexpected long put error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected long request body error");
  (match Simulator.Object.head conn ~bucket ~key:(object_key "short") () with
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "unexpected short head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected short body object to be absent");
  match Simulator.Object.head conn ~bucket ~key:(object_key "long") () with
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "unexpected long head error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected long body object to be absent"

let test_simulator_runtime_invalid_body_bounds_return_body_errors () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  let bucket = bucket_name "invalid-body-bounds-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  let descriptor : Awskit.Body.Request.descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:1L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        Simulator.Runtime.Request_body.write_subbytes writer
          (Bytes.of_string "abc") ~off:(-1) ~len:1)
  in
  expect_body_error "invalid write_subbytes"
    (Simulator.Object.put conn ~bucket
       ~key:(object_key "invalid-body")
       ~body ());
  ignore
    (Simulator.Object.put conn ~bucket ~key:(object_key "reader")
       ~body:(Simulator.Body.of_string "abc")
       ()
    |> ok_or_fail "put reader object");
  let invalid_read =
    Simulator.Object.get conn ~bucket ~key:(object_key "reader")
      ~consume:(fun reader ->
        let bytes = Bytes.create 2 in
        Simulator.Reader.read reader bytes ~off:1 ~len:2)
      ()
  in
  expect_body_error "invalid response read" invalid_read

let test_simulator_response_body_exception_runs_cleanup () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  let bucket = bucket_name "exception-cleanup-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  ignore
    (Simulator.Object.put conn ~bucket ~key:(object_key "reader")
       ~body:(Simulator.Body.of_string "abc")
       ()
    |> ok_or_fail "put reader object");
  let callback_exn = Failure "simulator consumer exploded" in
  let escaped = ref None in
  (try
     ignore
       (Simulator.Object.get conn ~bucket ~key:(object_key "reader")
          ~consume:(fun reader ->
            escaped := Some reader;
            let bytes = Bytes.create 1 in
            ignore
              (Simulator.Reader.read reader bytes ~off:0 ~len:1
                : (int, Awskit.Error.t) result);
            raise callback_exn)
          ()
         : (unit Object.Get.result, Awskit.Error.t) result);
     Alcotest.fail "expected consumer exception"
   with
  | exn when Stdlib.( == ) exn callback_exn -> ()
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn));
  match !escaped with
  | None -> Alcotest.fail "expected escaped reader"
  | Some reader -> (
      let bytes = Bytes.create 1 in
      match Simulator.Reader.read reader bytes ~off:0 ~len:1 with
      | Error error when is_body_error error -> ()
      | Error error ->
          Alcotest.failf "unexpected cleanup read error: %a" Error.pp error
      | Ok _ -> Alcotest.fail "escaped reader read succeeded after exception")

let test_simulator_response_body_reader_cannot_escape_scope () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  let bucket = bucket_name "escaped-reader-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  ignore
    (Simulator.Object.put conn ~bucket ~key:(object_key "reader")
       ~body:(Simulator.Body.of_string "abc")
       ()
    |> ok_or_fail "put reader object");
  let escaped = ref None in
  (match
     Simulator.Object.get conn ~bucket ~key:(object_key "reader")
       ~consume:(fun reader ->
         escaped := Some reader;
         Ok ())
       ()
   with
  | Ok _ -> ()
  | Error error -> Alcotest.failf "unexpected get error: %a" Error.pp error);
  match !escaped with
  | None -> Alcotest.fail "expected escaped reader"
  | Some reader -> (
      let bytes = Bytes.create 1 in
      match Simulator.Reader.read reader bytes ~off:0 ~len:1 with
      | Error error when is_body_error error -> ()
      | Error error ->
          Alcotest.failf "unexpected escaped reader error: %a" Error.pp error
      | Ok _ -> Alcotest.fail "escaped reader read succeeded")

let test_simulator_multipart_upload_part_stream_error_does_not_store_part () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "bucket");
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "large.bin") ()
    |> ok_or_fail "create multipart upload"
  in
  let stream_error =
    Awskit.Error.Producer.body "simulator multipart request body failed"
  in
  let descriptor : Awskit.Body.Request.descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    Simulator.Runtime.Request_body.of_stream descriptor ~write:(fun writer ->
        match Simulator.Runtime.Request_body.write_string writer "ab" with
        | Error _ as error -> error
        | Ok () -> Error stream_error)
  in
  (match
     Simulator.Multipart.upload_part conn ~upload:created.upload
       ~part_number:(Multipart.Part_number.of_int_exn 1)
       ~body ()
   with
  | Error error when Error.equal error stream_error -> ()
  | Error error ->
      Alcotest.failf "unexpected upload part error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected stream request body error");
  let listed =
    Simulator.Multipart.list_parts conn ~upload:created.upload ()
    |> ok_or_fail "list multipart parts"
  in
  Alcotest.(check int) "stored parts" 0 (List.length listed.parts)

let test_simulator_public_surface () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  let put =
    Simulator.Object.put conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "ok.txt")
      ~body:(Simulator.Body.of_string "hello")
      ()
    |> ok_or_fail "put ok"
  in
  (match
     (Simulator.object_metadata store
        ~bucket:(bucket_name "test-bucket")
        ~key:(object_key "ok.txt")
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
    (Simulator.objects_as_strings store ~bucket:(bucket_name "test-bucket"));
  Alcotest.(check bool)
    "missing object metadata" true
    (Option.is_none
       (Simulator.object_metadata store
          ~bucket:(bucket_name "test-bucket")
          ~key:(object_key "missing")));
  Simulator.enable_random_faults conn ~seed:7 ~prob:1.0;
  (match
     Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "faulted.txt")
       ~body:(Simulator.Body.of_string "faulted")
       ()
   with
  | Error error when Error.service_code error = Some "InternalError" -> ()
  | Error error -> Alcotest.failf "unexpected random fault: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected random fault");
  Simulator.disable_random_faults conn;
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "after.txt")
       ~body:(Simulator.Body.of_string "after")
       ()
    |> ok_or_fail "put after disabling random faults");
  Alcotest.(check (list (pair string string)))
    "objects after random fault"
    [ ("after.txt", "after"); ("ok.txt", "hello") ]
    (Simulator.objects_as_strings store ~bucket:(bucket_name "test-bucket"))

let test_simulator_config_constructor () =
  (match Simulator.config ~max_list_keys:0 () with
  | Error error when is_validation_field "max_list_keys" error -> ()
  | Error error -> Alcotest.failf "unexpected config error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected config validation");
  let config = Simulator.config_exn ~max_list_keys:2 () in
  Alcotest.(check int) "config cap" 2 config.max_list_keys;
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~config ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  let bucket = bucket_name "config-bucket" in
  ignore (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  List.iter
    (fun key ->
      ignore
        (Simulator.Object.put_string conn ~bucket ~key:(object_key key)
           ~contents:key ()
        |> ok_or_fail ("put " ^ key)))
    [ "a"; "b"; "c" ];
  let page =
    Simulator.Object.list conn ~bucket () |> ok_or_fail "list config cap"
  in
  Alcotest.(check int) "configured page size" 2 (List.length page.objects);
  Alcotest.(check bool) "configured page truncates" true page.is_truncated

let simulator_operation_name (_ : Simulator.operation_record) = function
  | `Put_object | `Get_object | `Head_object | `Delete_object | `List_objects_v2
  | `List_object_versions | `Copy_object | `Delete_objects
  | `Create_multipart_upload | `Upload_part | `Complete_multipart_upload
  | `Abort_multipart_upload | `List_parts ->
      ()

let test_simulator_history_uses_operation_names () =
  let conn = make_simulator () in
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "history.txt")
       ~body:(Simulator.Body.of_string "history")
       ()
    |> ok_or_fail "put history");
  match Simulator.history (Simulator.store conn) with
  | record :: _ -> simulator_operation_name record record.op
  | [] -> Alcotest.fail "expected simulator history record"

let test_simulator_buffer_roundtrip () =
  let conn = make_simulator () in
  let checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ="
  in
  let put =
    Simulator.Object.put conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "hello.txt")
      ~options:
        {
          Object.Put.default_options with
          content_type = Some (content_type "text/plain");
          checksum = Some checksum;
        }
      ~body:(Simulator.Body.of_string "hello")
      ()
    |> ok_or_fail "put"
  in
  Alcotest.(check bool) "etag" true (Option.is_some put.etag);
  check_checksum "put checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
  let result =
    Simulator.Object.get conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "hello.txt")
      ~consume:(Simulator.Reader.to_string ~max_bytes:16L)
      ()
    |> ok_or_fail "get"
  in
  let body = result.Object.Get.value in
  Alcotest.(check string) "body" "hello" body;
  Alcotest.(check (option string))
    "content-type" (Some "text/plain")
    (Option.map Content_type.to_string result.content_type);
  check_checksum "get checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" result.checksum;
  let head =
    Simulator.Object.head conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "hello.txt") ()
    |> ok_or_fail "head checksum"
  in
  check_checksum "head checksum" Object.Checksum.Algorithm.Sha256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" head.checksum;
  let page =
    Simulator.Object.list conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "list checksum"
  in
  match page.objects with
  | [ object_ ] ->
      Alcotest.(check string)
        "listed key" "hello.txt"
        (Object_key.to_string object_.key);
      Alcotest.(check (option int64)) "listed size" (Some 5L) object_.size
  | _ -> Alcotest.fail "expected one listed object"

let test_simulator_convenience_roundtrip () =
  let conn = make_simulator () in
  ignore
    (Simulator.Object.put_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "hello.txt") ~contents:"hello" ()
    |> ok_or_fail "put string");
  let result =
    Simulator.Object.get_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "hello.txt") ~max_bytes:16L ()
    |> ok_or_fail "get string"
  in
  Alcotest.(check string) "string body" "hello" result.value;
  let payload = Bytes.of_string "\000\255bytes" in
  ignore
    (Simulator.Object.put_bytes conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "blob.bin") ~contents:payload ()
    |> ok_or_fail "put bytes");
  let found =
    Simulator.Object.find_bytes conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "blob.bin") ~max_bytes:16L ()
    |> ok_or_fail "find bytes"
  in
  match found with
  | Some result ->
      Alcotest.(check string)
        "bytes body" (Bytes.to_string payload)
        (Bytes.to_string result.value)
  | None -> Alcotest.fail "expected bytes object"

let test_simulator_conveniences_validate_max_bytes_before_lookup () =
  let conn = make_simulator () in
  let store = Simulator.store conn in
  let expect_max_bytes label result =
    match result with
    | Error error when is_validation_field "max_bytes" error -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected max_bytes validation" label
  in
  expect_max_bytes "get string"
    (Simulator.Object.get_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing.txt") ~max_bytes:(-1L) ());
  Alcotest.(check int)
    "history after get string" 0
    (List.length (Simulator.history store));
  expect_max_bytes "find string"
    (Simulator.Object.find_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing.txt") ~max_bytes:(-1L) ());
  Alcotest.(check int)
    "history after find string" 0
    (List.length (Simulator.history store));
  expect_max_bytes "get bytes"
    (Simulator.Object.get_bytes conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing.txt") ~max_bytes:(-1L) ());
  Alcotest.(check int)
    "history after get bytes" 0
    (List.length (Simulator.history store));
  expect_max_bytes "find bytes"
    (Simulator.Object.find_bytes conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "missing.txt") ~max_bytes:(-1L) ());
  Alcotest.(check int)
    "history after find bytes" 0
    (List.length (Simulator.history store))

let test_simulator_rejects_unknown_checksum_writes () =
  let conn = make_simulator () in
  let expect_checksum_validation label = function
    | Error error when is_validation_field "checksum_algorithm" error -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected checksum validation" label
  in
  expect_checksum_validation "checksum constructor"
    (Object.Checksum.value
       ~algorithm:(Object.Checksum.Algorithm.Unknown "FUTURE") ~value:"value");
  let copy_options =
    {
      Object.Copy.default_options with
      checksum_algorithm = Some (Object.Checksum.Algorithm.Unknown "FUTURE");
    }
  in
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "source.txt")
       ~body:(Simulator.Body.of_string "body")
       ()
    |> ok_or_fail "source");
  expect_checksum_validation "simulator copy"
    (Simulator.Object.copy conn
       ~source_bucket:(bucket_name "test-bucket")
       ~source_key:(object_key "source.txt")
       ~destination_bucket:(bucket_name "test-bucket")
       ~destination_key:(object_key "copy.txt") ~options:copy_options ());
  let upload =
    Multipart.Upload.resume
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "bad.bin")
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")
  in
  let part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~etag:(Object.Etag.of_string_exn "\"etag\"")
      ()
  in
  let complete_options =
    {
      Multipart.Complete.default_options with
      checksum_type = Some (Object.Checksum.Type.Unknown "FUTURE");
    }
  in
  match
    Simulator.Multipart.complete_upload conn ~upload ~options:complete_options
      ~parts:[ part ] ()
  with
  | Error error when is_validation_field "checksum_type" error -> ()
  | Error error ->
      Alcotest.failf "simulator complete checksum type: unexpected error: %a"
        Error.pp error
  | Ok _ ->
      Alcotest.fail "simulator complete checksum type: expected validation"

let expect_validation_field label field = function
  | Error error when is_validation_field field error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation field %s" label field

let test_simulator_multipart_complete_validates_sizes () =
  let conn = make_simulator () in
  let upload =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "sized.bin") ()
    |> ok_or_fail "create sized upload"
  in
  let upload_part part_number body =
    Simulator.Multipart.upload_part conn ~upload:upload.upload
      ~part_number:(Multipart.Part_number.of_int_exn part_number)
      ~body:(Simulator.Body.of_string body)
      ()
    |> ok_or_fail ("upload part " ^ string_of_int part_number)
  in
  let small_part = upload_part 1 "a" in
  Alcotest.(check (option int64))
    "simulator upload part size" (Some 1L)
    (Multipart.Part.size small_part.part);
  let negative_options =
    {
      Multipart.Complete.default_options with
      multipart_object_size = Some (-1L);
    }
  in
  expect_validation_field "negative multipart object size"
    "multipart_object_size"
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~options:negative_options ~parts:[ small_part.part ] ());
  let small_part_2 = upload_part 2 "b" in
  expect_validation_field "undersized non-final simulator part" "parts"
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~parts:[ small_part.part; small_part_2.part ]
       ());
  let large_part = upload_part 1 (String.make Transfer.min_part_size 'x') in
  let final_part = upload_part 2 "z" in
  let mismatch_options =
    { Multipart.Complete.default_options with multipart_object_size = Some 1L }
  in
  expect_validation_field "simulator multipart object size mismatch"
    "multipart_object_size"
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~options:mismatch_options
       ~parts:[ large_part.part; final_part.part ]
       ())

let test_simulator_multipart_complete_validates_part_checksums () =
  let conn = make_simulator () in
  let upload =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "checksummed.bin")
      ()
    |> ok_or_fail "create checksum upload"
  in
  let checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"part-sha256"
  in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:upload.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Simulator.Body.of_string "part")
      ~options:(Multipart.Upload_part.options_exn ~checksum ())
      ()
    |> ok_or_fail "upload checksummed part"
  in
  let wrong_checksum : Object.Checksum.value =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"wrong-sha256"
  in
  let wrong_part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part.part_number uploaded.part)
      ~etag:(Multipart.Part.etag uploaded.part)
      ~checksum:wrong_checksum
      ?size:(Multipart.Part.size uploaded.part)
      ()
  in
  expect_status "complete with wrong part checksum" 400
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~parts:[ wrong_part ] ());
  let missing_checksum_part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part.part_number uploaded.part)
      ~etag:(Multipart.Part.etag uploaded.part)
      ?size:(Multipart.Part.size uploaded.part)
      ()
  in
  expect_status "complete with missing part checksum" 400
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~parts:[ missing_checksum_part ] ());
  ignore
    (Simulator.Multipart.complete_upload conn ~upload:upload.upload
       ~parts:[ uploaded.part ] ()
    |> ok_or_fail "complete checksummed part")

let test_simulator_multipart_abort_removes_upload () =
  let conn = make_simulator () in
  let upload =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "abort-twice.bin")
      ()
    |> ok_or_fail "create abort upload"
  in
  let aborted =
    Simulator.Multipart.abort_upload conn ~upload:upload.upload ()
    |> ok_or_fail "abort upload"
  in
  Alcotest.(check int)
    "abort response status" 204
    (Awskit.Response.status aborted.response);
  match Simulator.Multipart.abort_upload conn ~upload:upload.upload () with
  | Error error when Error.service_code error = Some "NoSuchUpload" -> ()
  | Error error ->
      Alcotest.failf "unexpected repeated simulator abort error: %a" Error.pp
        error
  | Ok _ -> Alcotest.fail "expected repeated simulator abort to fail"

let test_simulator_streaming_get () =
  let conn = make_simulator () in
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "stream")
       ~body:(Simulator.Body.of_string "abcdef")
       ()
    |> ok_or_fail "put");
  let consume reader =
    let bytes = Bytes.create 3 in
    match Simulator.Runtime.Response_body.read reader bytes ~off:0 ~len:3 with
    | Error _ as error -> error
    | Ok read -> Ok (Bytes.sub_string bytes 0 read)
  in
  let body =
    Simulator.Object.get conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "stream") ~consume ()
    |> ok_or_fail "stream get"
    |> fun result -> result.Object.Get.value
  in
  Alcotest.(check string) "partial body" "abc" body

let test_buffer_limit () =
  let conn = make_simulator () in
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "large")
       ~body:(Simulator.Body.of_string "abcdef")
       ()
    |> ok_or_fail "put");
  match
    Simulator.Object.get conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "large")
      ~consume:(Simulator.Reader.to_string ~max_bytes:3L)
      ()
  with
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_bytes failure"

let test_simulator_paginator_keys () =
  let conn = make_simulator () in
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "logs/a.txt")
       ~body:(Simulator.Body.of_string "a")
       ()
    |> ok_or_fail "put a");
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "logs/b.txt")
       ~body:(Simulator.Body.of_string "b")
       ()
    |> ok_or_fail "put b");
  ignore
    (Simulator.Object.put conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "other.txt")
       ~body:(Simulator.Body.of_string "other")
       ()
    |> ok_or_fail "put other");
  let options =
    Object.List.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~max_keys:1 ()
  in
  let keys =
    Simulator.Object.List.keys conn
      ~bucket:(bucket_name "test-bucket")
      ~options ~max_pages:10 ()
    |> ok_or_fail "simulator paginator keys"
    |> List.map Object_key.to_string
  in
  Alcotest.(check (list string)) "keys" [ "logs/a.txt"; "logs/b.txt" ] keys

let test_simulator_list_common_prefixes () =
  let conn = make_simulator () in
  let put key body =
    Simulator.Object.put conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key key)
      ~body:(Simulator.Body.of_string body)
      ()
    |> ok_or_fail ("put " ^ key)
  in
  ignore (put "logs/a" "a");
  ignore (put "logs/2026/a" "nested");
  ignore (put "reports/a" "report");
  let options =
    Object.List.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~delimiter:Object.List.Delimiter.slash ()
  in
  let page =
    Simulator.Object.list conn ~bucket:(bucket_name "test-bucket") ~options ()
    |> ok_or_fail "list with delimiter"
  in
  Alcotest.(check (list string))
    "direct objects" [ "logs/a" ]
    (List.map
       (fun (object_ : Object.List.object_summary) ->
         Object_key.to_string object_.key)
       page.objects);
  Alcotest.(check (list string))
    "common prefixes" [ "logs/2026/" ]
    (List.map Object_key.Prefix.to_string page.common_prefixes)

let test_simulator_list_token_handles_control_key () =
  let conn = make_simulator () in
  let put key body =
    Simulator.Object.put conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key key)
      ~body:(Simulator.Body.of_string body)
      ()
    |> ok_or_fail ("put " ^ key)
  in
  ignore (put "logs/a\nx" "a");
  ignore (put "logs/b" "b");
  let options =
    Object.List.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~max_keys:1 ()
  in
  let keys =
    Simulator.Object.List.keys conn
      ~bucket:(bucket_name "test-bucket")
      ~options ~max_pages:10 ()
    |> ok_or_fail "paginate control key"
    |> List.map Object_key.to_string
  in
  Alcotest.(check (list string)) "keys" [ "logs/a\nx"; "logs/b" ] keys

let test_simulator_version_list_common_prefixes () =
  let conn = make_simulator () in
  ignore
    (Simulator.Bucket.Versioning.put conn
       ~bucket:(bucket_name "test-bucket")
       ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "enable versioning");
  let put key body =
    Simulator.Object.put conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key key)
      ~body:(Simulator.Body.of_string body)
      ()
    |> ok_or_fail ("put " ^ key)
  in
  ignore (put "logs/a" "a");
  ignore (put "logs/2026/a" "nested");
  ignore (put "reports/a" "report");
  let options =
    Object.Versions.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~delimiter:Object.Versions.Delimiter.slash ()
  in
  let page =
    Simulator.Object.list_versions conn
      ~bucket:(bucket_name "test-bucket")
      ~options ()
    |> ok_or_fail "list versions with delimiter"
  in
  Alcotest.(check (list string))
    "direct versions" [ "logs/a" ]
    (List.map
       (fun (version : Object.Versions.object_version) ->
         Object_key.to_string version.key)
       page.versions);
  Alcotest.(check (list string))
    "common prefixes" [ "logs/2026/" ]
    (List.map Object_key.Prefix.to_string page.common_prefixes)

let suite =
  [
    ( "simulator",
      [
        Alcotest.test_case "simulator request body requires known length" `Quick
          test_simulator_request_body_requires_known_length;
        Alcotest.test_case "simulator bucket validation matches core rules"
          `Quick test_simulator_bucket_validation_matches_core_rules;
        Alcotest.test_case "simulator stream request body error propagates"
          `Quick test_simulator_stream_request_body_error_propagates;
        Alcotest.test_case
          "simulator stream request body rejects length mismatch" `Quick
          test_simulator_stream_request_body_rejects_length_mismatch;
        Alcotest.test_case
          "simulator runtime invalid body bounds return body errors" `Quick
          test_simulator_runtime_invalid_body_bounds_return_body_errors;
        Alcotest.test_case "simulator response body exception runs cleanup"
          `Quick test_simulator_response_body_exception_runs_cleanup;
        Alcotest.test_case "simulator response body reader cannot escape scope"
          `Quick test_simulator_response_body_reader_cannot_escape_scope;
        Alcotest.test_case
          "simulator multipart upload part stream error does not store part"
          `Quick
          test_simulator_multipart_upload_part_stream_error_does_not_store_part;
        Alcotest.test_case "simulator public surface" `Quick
          test_simulator_public_surface;
        Alcotest.test_case "simulator config constructor" `Quick
          test_simulator_config_constructor;
        Alcotest.test_case "simulator history uses operation names" `Quick
          test_simulator_history_uses_operation_names;
        Alcotest.test_case "simulator in-memory roundtrip" `Quick
          test_simulator_buffer_roundtrip;
        Alcotest.test_case "simulator convenience roundtrip" `Quick
          test_simulator_convenience_roundtrip;
        Alcotest.test_case
          "simulator convenience max_bytes preflight validation" `Quick
          test_simulator_conveniences_validate_max_bytes_before_lookup;
        Alcotest.test_case "simulator rejects unknown checksum writes" `Quick
          test_simulator_rejects_unknown_checksum_writes;
        Alcotest.test_case "simulator validates multipart complete sizes" `Quick
          test_simulator_multipart_complete_validates_sizes;
        Alcotest.test_case "simulator validates multipart complete checksums"
          `Quick test_simulator_multipart_complete_validates_part_checksums;
        Alcotest.test_case "simulator abort removes multipart upload" `Quick
          test_simulator_multipart_abort_removes_upload;
        Alcotest.test_case "simulator streaming get" `Quick
          test_simulator_streaming_get;
        Alcotest.test_case "in-memory helper limit" `Quick test_buffer_limit;
        Alcotest.test_case "simulator paginator keys" `Quick
          test_simulator_paginator_keys;
        Alcotest.test_case "simulator list common prefixes" `Quick
          test_simulator_list_common_prefixes;
        Alcotest.test_case "simulator list token handles control key" `Quick
          test_simulator_list_token_handles_control_key;
        Alcotest.test_case "simulator version list common prefixes" `Quick
          test_simulator_version_list_common_prefixes;
      ] );
  ]
