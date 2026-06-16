open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let test_object_paginator_follows_tokens () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_page ~next_continuation_token:"token-1" ~truncated:true
             [ "a.txt" ]);
        response 200
          (list_page ~continuation_token:"token-1" ~truncated:false [ "b.txt" ]);
      ]
  in
  let options = { List_objects_v2.default_options with max_keys = Some 1 } in
  let keys =
    Recording_s3.Object.List_objects_v2.keys conn ~bucket:"my-bucket" ~options
      ()
    |> ok_or_fail "paginator keys"
  in
  Alcotest.(check (list string)) "keys" [ "a.txt"; "b.txt" ] keys;
  let calls = List.rev conn.calls in
  Alcotest.(check int) "calls" 2 (List.length calls);
  match calls with
  | [ _first; second ] ->
      Alcotest.(check (option (list string)))
        "continuation token" (Some [ "token-1" ])
        (List.assoc_opt "continuation-token" second.request.target.query)
  | _ -> Alcotest.fail "expected two calls"

let test_object_paginator_max_pages () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_page ~next_continuation_token:"token-1" ~truncated:true
             [ "a.txt" ]);
        response 200 (list_page ~truncated:false [ "b.txt" ]);
      ]
  in
  let pages =
    Recording_s3.Object.List_objects_v2.pages conn ~bucket:"my-bucket"
      ~max_pages:1 ()
    |> ok_or_fail "paginator pages"
  in
  Alcotest.(check int) "page count" 1 (List.length pages);
  Alcotest.(check int) "calls" 1 (List.length conn.calls)

let test_multipart_paginator_follows_markers () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_parts_page ~next_part_number_marker:1 ~truncated:true [ 1 ]);
        response 200 (list_parts_page ~truncated:false [ 2 ]);
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let parts =
    Recording_s3.Multipart.List_parts.parts conn ~bucket:"my-bucket"
      ~key:"large.bin" ~upload_id ()
    |> ok_or_fail "multipart paginator parts"
  in
  Alcotest.(check (list int))
    "parts" [ 1; 2 ]
    (List.map (fun (part : List_parts.part_info) -> part.part_number) parts);
  let calls = List.rev conn.calls in
  Alcotest.(check int) "calls" 2 (List.length calls);
  match calls with
  | [ _first; second ] ->
      Alcotest.(check (option (list string)))
        "part marker" (Some [ "1" ])
        (List.assoc_opt "part-number-marker" second.request.target.query)
  | _ -> Alcotest.fail "expected two calls"

let test_multipart_list_parts_rejects_malformed_known_fields () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<ListPartsResult><Part><PartNumber>not-int</PartNumber></Part></ListPartsResult>";
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  match
    Recording_s3.Multipart.List_parts.parts conn ~bucket:"my-bucket"
      ~key:"large.bin" ~upload_id ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions PartNumber" true
        (string_contains text ~substring:"PartNumber")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed PartNumber decode error"

let test_multipart_list_parts_rejects_invalid_numeric_fields () =
  let cases =
    [
      ( "PartNumber",
        "<ListPartsResult><Part><PartNumber>-1</PartNumber></Part></ListPartsResult>"
      );
      ( "PartNumber",
        "<ListPartsResult><Part><PartNumber>10001</PartNumber></Part></ListPartsResult>"
      );
      ( "Size",
        "<ListPartsResult><Part><PartNumber>1</PartNumber><Size>-1</Size></Part></ListPartsResult>"
      );
    ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  List.iter
    (fun (field, body) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match
        Recording_s3.Multipart.List_parts.parts conn ~bucket:"my-bucket"
          ~key:"large.bin" ~upload_id ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            ("mentions " ^ field) true
            (string_contains text ~substring:field)
      | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
      | Ok _ -> Alcotest.failf "expected invalid %s decode error" field)
    cases

let suite =
  [
    ( "paginator",
      [
        Alcotest.test_case "object paginator follows tokens" `Quick
          test_object_paginator_follows_tokens;
        Alcotest.test_case "object paginator max pages" `Quick
          test_object_paginator_max_pages;
        Alcotest.test_case "multipart paginator follows markers" `Quick
          test_multipart_paginator_follows_markers;
        Alcotest.test_case "multipart list parts rejects malformed fields"
          `Quick test_multipart_list_parts_rejects_malformed_known_fields;
        Alcotest.test_case "multipart list parts rejects invalid numbers" `Quick
          test_multipart_list_parts_rejects_invalid_numeric_fields;
      ] );
  ]
