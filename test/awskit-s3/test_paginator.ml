open Awskit_s3
open Awskit_s3_test

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
      ] );
  ]
