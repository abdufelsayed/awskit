open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let create_upload conn key =
  Simulator.Multipart.create_upload conn
    ~bucket:(bucket_name "test-bucket")
    ~key:(object_key key) ()
  |> ok_or_fail "create upload"

let upload_part conn upload part_number body =
  Simulator.Multipart.upload_part conn ~upload
    ~part_number:(Multipart.Part_number.of_int_exn part_number)
    ~body:(Simulator.Body.of_string body)
    ()
  |> ok_or_fail ("upload part " ^ string_of_int part_number)

let part_number (part : Multipart.List_parts.part_info) =
  Multipart.Part_number.to_int part.part_number

let checksum_value algorithm value = Object.Checksum.value_exn ~algorithm ~value
let sha256 value = checksum_value Object.Checksum.Algorithm.Sha256 value
let md5 value = checksum_value Object.Checksum.Algorithm.Md5 value
let crc32 value = checksum_value Object.Checksum.Algorithm.Crc32 value

let single_checksum_value label algorithm (checksum : Object.Checksum.response)
    =
  match
    List.find_opt
      (fun (value : Object.Checksum.observed_value) ->
        value.algorithm = Object.Checksum.Algorithm.Known algorithm)
      checksum.values
  with
  | Some value -> value.value
  | None -> Alcotest.failf "%s: missing checksum value" label

let etag_string label = function
  | Some etag -> Object.Etag.to_string etag
  | None -> Alcotest.failf "%s: missing etag" label

let test_put_rejects_bad_checksum () =
  let conn = make_simulator () in
  let options =
    Object.Put.options_exn ~checksum:(sha256 "not-the-sha256-of-hello") ()
  in
  expect_service_code "put bad checksum" "BadDigest"
    (Simulator.Object.put_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "bad-put.bin") ~contents:"hello" ~options ());
  let options =
    Object.Put.options_exn
      ~checksum:(sha256 "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=")
      ()
  in
  let result =
    Simulator.Object.put_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "good-put.bin")
      ~contents:"hello" ~options ()
    |> ok_or_fail "put good checksum"
  in
  Alcotest.(check string)
    "stored checksum" "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ="
    (single_checksum_value "put checksum" Object.Checksum.Algorithm.Sha256
       result.checksum)

let test_upload_part_rejects_bad_checksum () =
  let conn = make_simulator () in
  let created = create_upload conn "bad-part.bin" in
  let options =
    Multipart.Upload_part.options_exn
      ~checksum:(sha256 "not-the-sha256-of-part")
      ()
  in
  expect_service_code "upload part bad checksum" "BadDigest"
    (Simulator.Multipart.upload_part conn ~upload:created.upload
       ~part_number:(Multipart.Part_number.of_int_exn 1)
       ~body:(Simulator.Body.of_string "part")
       ~options ());
  let options =
    Multipart.Upload_part.options_exn
      ~checksum:(sha256 "N6aAEzvQk0L5NK+43Sx9nhtiTaXzXjo4rbED43wFXtE=")
      ()
  in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:created.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Simulator.Body.of_string "part")
      ~options ()
    |> ok_or_fail "upload part good checksum"
  in
  Alcotest.(check string)
    "part checksum" "N6aAEzvQk0L5NK+43Sx9nhtiTaXzXjo4rbED43wFXtE="
    (single_checksum_value "part checksum" Object.Checksum.Algorithm.Sha256
       uploaded.checksum)

let test_complete_rejects_bad_full_object_checksum () =
  let conn = make_simulator () in
  let created = create_upload conn "bad-complete.bin" in
  let uploaded = upload_part conn created.upload 1 "final" in
  let options =
    Multipart.Complete.options_exn
      ~checksum:(sha256 "not-the-sha256-of-final")
      ()
  in
  expect_service_code "complete bad checksum" "BadDigest"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload ~options
       ~parts:[ uploaded.part ] ())

let test_complete_uses_multipart_etag () =
  let conn = make_simulator () in
  let created = create_upload conn "multipart-etag.bin" in
  let uploaded = upload_part conn created.upload 1 "final" in
  let completed =
    Simulator.Multipart.complete_upload conn ~upload:created.upload
      ~parts:[ uploaded.part ] ()
    |> ok_or_fail "complete multipart etag"
  in
  Alcotest.(check string)
    "multipart etag" "\"a9dc24586fb8422b0be7706d73cd1eaf-1\""
    (etag_string "complete etag" completed.etag);
  let stored =
    Simulator.Object.head conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "multipart-etag.bin")
      ()
    |> ok_or_fail "head multipart etag"
  in
  Alcotest.(check string)
    "stored multipart etag" "\"a9dc24586fb8422b0be7706d73cd1eaf-1\""
    (etag_string "stored etag" stored.etag)

let test_checksum_algorithms_are_computed_or_rejected () =
  let conn = make_simulator () in
  expect_validation_field "unsupported create checksum algorithm"
    "checksum_algorithm"
    (Simulator.Multipart.create_upload conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "crc32.bin")
       ~options:
         (Multipart.Create.options_exn
            ~checksum_algorithm:Object.Checksum.Algorithm.Crc32 ())
       ());
  expect_validation_field "unsupported put checksum value" "checksum_algorithm"
    (Simulator.Object.put_string conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "crc32-put.bin")
       ~contents:"hello"
       ~options:(Object.Put.options_exn ~checksum:(crc32 "aaaa") ())
       ());
  let created =
    Simulator.Multipart.create_upload conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "sha512.bin")
      ~options:
        (Multipart.Create.options_exn
           ~checksum_algorithm:Object.Checksum.Algorithm.Sha512 ())
      ()
    |> ok_or_fail "create sha512 upload"
  in
  let uploaded = upload_part conn created.upload 1 "part" in
  let completed =
    Simulator.Multipart.complete_upload conn ~upload:created.upload
      ~parts:[ uploaded.part ] ()
    |> ok_or_fail "complete sha512 upload"
  in
  Alcotest.(check string)
    "computed sha512"
    "GhqvZuOtT6dLeD6B6eSAp+8Qo3MZrkfbGkzIqfUaMM5iH5j5vFd1NNZY3q1k3Uuksy9pUYion0/prI/ElqaQdw=="
    (single_checksum_value "complete sha512" Object.Checksum.Algorithm.Sha512
       completed.checksum);
  let created = create_upload conn "md5-part.bin" in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:created.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Simulator.Body.of_string "part")
      ~options:
        (Multipart.Upload_part.options_exn
           ~checksum:(md5 "9Mk4XxkC9zNLALm07NFk3g==")
           ())
      ()
    |> ok_or_fail "upload md5 part"
  in
  Alcotest.(check string)
    "computed md5" "9Mk4XxkC9zNLALm07NFk3g=="
    (single_checksum_value "part md5" Object.Checksum.Algorithm.Md5
       uploaded.checksum)

let test_abort_removes_upload () =
  let conn = make_simulator () in
  let created = create_upload conn "abort.bin" in
  ignore
    (Simulator.Multipart.abort_upload conn ~upload:created.upload ()
    |> ok_or_fail "abort upload");
  expect_service_code "list parts after abort" "NoSuchUpload"
    (Simulator.Multipart.list_parts conn ~upload:created.upload ());
  expect_service_code "abort again" "NoSuchUpload"
    (Simulator.Multipart.abort_upload conn ~upload:created.upload ())

let test_complete_success_removes_upload () =
  let conn = make_simulator () in
  let created = create_upload conn "complete.bin" in
  let uploaded = upload_part conn created.upload 1 "final" in
  ignore
    (Simulator.Multipart.complete_upload conn ~upload:created.upload
       ~parts:[ uploaded.part ] ()
    |> ok_or_fail "complete upload");
  expect_service_code "list parts after complete" "NoSuchUpload"
    (Simulator.Multipart.list_parts conn ~upload:created.upload ());
  let stored =
    Simulator.Object.get_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "complete.bin")
      ~max_bytes:16L ()
    |> ok_or_fail "get completed object"
  in
  Alcotest.(check string) "completed body" "final" stored.value

let test_complete_validation_failure_keeps_upload () =
  let conn = make_simulator () in
  let created = create_upload conn "retryable.bin" in
  let uploaded =
    upload_part conn created.upload 1 (String.make Transfer.min_part_size 'x')
  in
  let missing =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn 2)
      ~etag:(Multipart.Part.etag uploaded.part)
      ?size:(Multipart.Part.size uploaded.part)
      ()
  in
  expect_service_code "missing part" "InvalidPart"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload
       ~parts:[ uploaded.part; missing ] ());
  let listed =
    Simulator.Multipart.list_parts conn ~upload:created.upload ()
    |> ok_or_fail "list parts after failed complete"
  in
  Alcotest.(check (list int))
    "remaining parts" [ 1 ]
    (List.map part_number listed.parts)

let test_complete_rejects_undersized_nonfinal_part () =
  let conn = make_simulator () in
  let created = create_upload conn "undersized.bin" in
  let first = upload_part conn created.upload 1 "a" in
  let final = upload_part conn created.upload 2 "b" in
  expect_validation_field "undersized non-final" "parts"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload
       ~parts:[ first.part; final.part ] ());
  let large =
    upload_part conn created.upload 1 (String.make Transfer.min_part_size 'x')
  in
  let final = upload_part conn created.upload 2 "z" in
  let options = Multipart.Complete.options_exn ~multipart_object_size:1L () in
  expect_validation_field "object size mismatch" "multipart_object_size"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload ~options
       ~parts:[ large.part; final.part ] ())

let test_complete_rejects_part_checksum_mismatch () =
  let conn = make_simulator () in
  let created = create_upload conn "checksummed.bin" in
  let checksum = sha256 "N6aAEzvQk0L5NK+43Sx9nhtiTaXzXjo4rbED43wFXtE=" in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:created.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Simulator.Body.of_string "part")
      ~options:(Multipart.Upload_part.options_exn ~checksum ())
      ()
    |> ok_or_fail "upload checksummed part"
  in
  let wrong_checksum = sha256 "pMSuuSwgUA82SxKzdx7zoRGT4s8E0PKJVqgpdJmTs58=" in
  let wrong_part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part.part_number uploaded.part)
      ~etag:(Multipart.Part.etag uploaded.part)
      ~checksum:wrong_checksum
      ?size:(Multipart.Part.size uploaded.part)
      ()
  in
  expect_service_code "wrong checksum" "InvalidPart"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload
       ~parts:[ wrong_part ] ());
  let missing_checksum_part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part.part_number uploaded.part)
      ~etag:(Multipart.Part.etag uploaded.part)
      ?size:(Multipart.Part.size uploaded.part)
      ()
  in
  expect_service_code "missing checksum" "InvalidPart"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload
       ~parts:[ missing_checksum_part ] ())

let test_list_parts_pagination () =
  let conn = make_simulator () in
  let created = create_upload conn "parts.bin" in
  ignore (upload_part conn created.upload 3 "three");
  ignore (upload_part conn created.upload 1 "one");
  ignore (upload_part conn created.upload 2 "two");
  let options = Multipart.List_parts.options_exn ~max_parts:1 () in
  let pages =
    Simulator.Multipart.List_parts.pages conn ~upload:created.upload ~options
      ~max_pages:4 ()
    |> ok_or_fail "list part pages"
  in
  Alcotest.(check int) "page count" 3 (List.length pages);
  Alcotest.(check (list (list int)))
    "part pages" [ [ 1 ]; [ 2 ]; [ 3 ] ]
    (List.map
       (fun (page : Multipart.List_parts.page) ->
         List.map part_number page.parts)
       pages)

let suite =
  [
    ( "contract:awskit-s3-sim:multipart-validation",
      [
        Alcotest.test_case "abort removes upload" `Quick
          test_abort_removes_upload;
        Alcotest.test_case "put rejects bad checksum" `Quick
          test_put_rejects_bad_checksum;
        Alcotest.test_case "upload-part rejects bad checksum" `Quick
          test_upload_part_rejects_bad_checksum;
        Alcotest.test_case "complete rejects bad full-object checksum" `Quick
          test_complete_rejects_bad_full_object_checksum;
        Alcotest.test_case "complete uses multipart etag" `Quick
          test_complete_uses_multipart_etag;
        Alcotest.test_case "checksum algorithms are explicit" `Quick
          test_checksum_algorithms_are_computed_or_rejected;
        Alcotest.test_case "complete removes upload" `Quick
          test_complete_success_removes_upload;
        Alcotest.test_case "failed complete keeps upload" `Quick
          test_complete_validation_failure_keeps_upload;
        Alcotest.test_case "rejects undersized non-final part" `Quick
          test_complete_rejects_undersized_nonfinal_part;
        Alcotest.test_case "rejects checksum mismatch" `Quick
          test_complete_rejects_part_checksum_mismatch;
        Alcotest.test_case "list-parts pagination" `Quick
          test_list_parts_pagination;
      ] );
  ]
