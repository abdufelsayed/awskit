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
  let options =
    { Multipart.Complete.default_options with multipart_object_size = Some 1L }
  in
  expect_validation_field "object size mismatch" "multipart_object_size"
    (Simulator.Multipart.complete_upload conn ~upload:created.upload ~options
       ~parts:[ large.part; final.part ] ())

let test_complete_rejects_part_checksum_mismatch () =
  let conn = make_simulator () in
  let created = create_upload conn "checksummed.bin" in
  let checksum =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"part-sha256"
  in
  let uploaded =
    Simulator.Multipart.upload_part conn ~upload:created.upload
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~body:(Simulator.Body.of_string "part")
      ~options:(Multipart.Upload_part.options_exn ~checksum ())
      ()
    |> ok_or_fail "upload checksummed part"
  in
  let wrong_checksum =
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
