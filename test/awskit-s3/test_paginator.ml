open Awskit_s3
open Awskit_s3_test

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let qcheck_seed = 0xA5111
let to_alcotest = Awskit_test.Qcheck.to_alcotest ~seed:qcheck_seed

let chunk size values =
  let rec take n acc = function
    | rest when n = 0 -> (List.rev acc, rest)
    | [] -> (List.rev acc, [])
    | value :: rest -> take (n - 1) (value :: acc) rest
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | values ->
        let chunk, rest = take size [] values in
        loop (chunk :: acc) rest
  in
  loop [] values

let list_pages_for_keys ~page_size keys =
  let chunks = if keys = [] then [ [] ] else chunk page_size keys in
  let last_index = List.length chunks - 1 in
  List.mapi
    (fun index keys ->
      let continuation_token =
        if index = 0 then None else Some (Fmt.str "token-%d" index)
      in
      let next_continuation_token =
        if index = last_index then None
        else Some (Fmt.str "token-%d" (index + 1))
      in
      list_page ?continuation_token ?next_continuation_token
        ~truncated:(index < last_index) keys)
    chunks

let prop_list_paginator_collects_ordered_keys =
  let gen = QCheck.Gen.(pair (int_range 0 30) (int_range 1 7)) in
  QCheck.Test.make ~count:100
    ~name:"list paginator collects ordered generated pages"
    (QCheck.make
       ~print:(fun (count, page_size) ->
         Fmt.str "count=%d page_size=%d" count page_size)
       gen)
    (fun (count, page_size) ->
      let keys = List.init count (Fmt.str "key-%03d") in
      let responses =
        list_pages_for_keys ~page_size keys |> List.map (response 200)
      in
      let conn = Recording_runtime.connect responses in
      match
        Recording_s3.Object.List.keys conn ~bucket:(bucket_name "my-bucket")
          ~max_pages:(max 1 (List.length responses))
          ()
      with
      | Ok actual ->
          List.equal String.equal keys (List.map Object_key.to_string actual)
      | Error _ -> false)

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
  let options = Object.List.options_exn ~max_keys:1 () in
  let keys =
    Recording_s3.Object.List.keys conn ~bucket:(bucket_name "my-bucket")
      ~options ~max_pages:10 ()
    |> ok_or_fail "paginator keys"
    |> List.map Object_key.to_string
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
  match
    Recording_s3.Object.List.pages conn ~bucket:(bucket_name "my-bucket")
      ~max_pages:1 ()
  with
  | Error error when is_validation_field "max_pages" error ->
      Alcotest.(check int) "calls" 1 (List.length conn.calls)
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_pages bound error"

let version_page ?next_key_marker ~truncated keys =
  let next_marker_xml =
    match next_key_marker with
    | None -> ""
    | Some key ->
        Fmt.str
          "<NextKeyMarker>%s</NextKeyMarker><NextVersionIdMarker>version-next</NextVersionIdMarker>"
          key
  in
  let versions =
    keys
    |> List.map (fun key ->
        Fmt.str
          "<Version><Key>%s</Key><VersionId>version-%s</VersionId></Version>"
          key key)
    |> String.concat ""
  in
  Fmt.str
    "<ListVersionsResult><Name>my-bucket</Name><IsTruncated>%b</IsTruncated>%s%s</ListVersionsResult>"
    truncated next_marker_xml versions

let test_object_paginator_early_stop () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_page ~next_continuation_token:"token-1" ~truncated:true
             [ "a.txt" ]);
        response 200 (list_page ~truncated:false [ "b.txt" ]);
      ]
  in
  let keys =
    Recording_s3.Object.List.fold_pages_until conn
      ~bucket:(bucket_name "my-bucket") ~init:[]
      ~f:(fun keys (page : Object.List.page) ->
        let keys =
          List.rev_append
            (List.map
               (fun (object_ : Object.List.object_summary) ->
                 Object_key.to_string object_.key)
               page.objects)
            keys
        in
        Ok (Recording_s3.Object.List.Stop keys))
      ()
    |> ok_or_fail "early stop list"
    |> List.rev
  in
  Alcotest.(check (list string)) "keys" [ "a.txt" ] keys;
  Alcotest.(check int) "calls" 1 (List.length conn.calls)

let test_version_paginator_early_stop () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (version_page ~next_key_marker:"b.txt" ~truncated:true [ "a.txt" ]);
        response 200 (version_page ~truncated:false [ "b.txt" ]);
      ]
  in
  let keys =
    Recording_s3.Object.Versions.fold_pages_until conn
      ~bucket:(bucket_name "my-bucket") ~init:[]
      ~f:(fun keys (page : Object.Versions.page) ->
        let keys =
          List.rev_append
            (List.map
               (fun (version : Object.Versions.object_version) ->
                 Object_key.to_string version.key)
               page.versions)
            keys
        in
        Ok (Recording_s3.Object.Versions.Stop keys))
      ()
    |> ok_or_fail "early stop versions"
    |> List.rev
  in
  Alcotest.(check (list string)) "keys" [ "a.txt" ] keys;
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
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  let parts =
    Recording_s3.Multipart.List_parts.parts conn ~upload ()
    |> ok_or_fail "multipart paginator parts"
  in
  Alcotest.(check (list int))
    "parts" [ 1; 2 ]
    (List.map
       (fun (part : Multipart.List_parts.part_info) ->
         Multipart.Part_number.to_int part.part_number)
       parts);
  let calls = List.rev conn.calls in
  Alcotest.(check int) "calls" 2 (List.length calls);
  match calls with
  | [ _first; second ] ->
      Alcotest.(check (option (list string)))
        "part marker" (Some [ "1" ])
        (List.assoc_opt "part-number-marker" second.request.target.query)
  | _ -> Alcotest.fail "expected two calls"

let multipart_upload () =
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
    ~key:(object_key "large.bin") ~upload_id

let test_multipart_list_parts_pages_max_pages_error () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_parts_page ~next_part_number_marker:1 ~truncated:true [ 1 ]);
        response 200 (list_parts_page ~truncated:false [ 2 ]);
      ]
  in
  let upload = multipart_upload () in
  match
    Recording_s3.Multipart.List_parts.pages conn ~upload ~max_pages:1 ()
  with
  | Error error when is_validation_field "max_pages" error ->
      Alcotest.(check int) "calls" 1 (List.length conn.calls)
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_pages bound error"

let test_multipart_list_parts_parts_max_pages_error () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_parts_page ~next_part_number_marker:1 ~truncated:true [ 1 ]);
        response 200 (list_parts_page ~truncated:false [ 2 ]);
      ]
  in
  let upload = multipart_upload () in
  match
    Recording_s3.Multipart.List_parts.parts conn ~upload ~max_pages:1 ()
  with
  | Error error when is_validation_field "max_pages" error ->
      Alcotest.(check int) "calls" 1 (List.length conn.calls)
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_pages bound error"

let test_multipart_list_parts_accepts_zero_marker () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_parts_page ~next_part_number_marker:0 ~truncated:false [ 1 ]);
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  let page =
    Recording_s3.Multipart.list_parts conn ~upload ()
    |> ok_or_fail "multipart list parts"
  in
  Alcotest.(check (option int))
    "zero next marker" (Some 0)
    (Option.map Multipart.Part_number_marker.to_int page.next_part_number_marker)

let test_multipart_list_parts_rejects_malformed_known_fields () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          "<ListPartsResult><Part><PartNumber>not-int</PartNumber></Part></ListPartsResult>";
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  match Recording_s3.Multipart.List_parts.parts conn ~upload () with
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
  let upload =
    Multipart.Upload.resume ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "large.bin") ~upload_id
  in
  List.iter
    (fun (field, body) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match Recording_s3.Multipart.List_parts.parts conn ~upload () with
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
    ("pbt:paginator", [ to_alcotest prop_list_paginator_collects_ordered_keys ]);
    ( "paginator",
      [
        Alcotest.test_case "object paginator follows tokens" `Quick
          test_object_paginator_follows_tokens;
        Alcotest.test_case "object paginator max pages" `Quick
          test_object_paginator_max_pages;
        Alcotest.test_case "object paginator early stop" `Quick
          test_object_paginator_early_stop;
        Alcotest.test_case "version paginator early stop" `Quick
          test_version_paginator_early_stop;
        Alcotest.test_case "multipart paginator follows markers" `Quick
          test_multipart_paginator_follows_markers;
        Alcotest.test_case "multipart pages max pages errors" `Quick
          test_multipart_list_parts_pages_max_pages_error;
        Alcotest.test_case "multipart parts max pages errors" `Quick
          test_multipart_list_parts_parts_max_pages_error;
        Alcotest.test_case "multipart list parts accepts zero marker" `Quick
          test_multipart_list_parts_accepts_zero_marker;
        Alcotest.test_case "multipart list parts rejects malformed fields"
          `Quick test_multipart_list_parts_rejects_malformed_known_fields;
        Alcotest.test_case "multipart list parts rejects invalid numbers" `Quick
          test_multipart_list_parts_rejects_invalid_numeric_fields;
      ] );
  ]
