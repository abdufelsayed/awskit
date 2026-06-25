open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let object_keys objects =
  List.map
    (fun (object_ : Object.List.object_summary) ->
      Object_key.to_string object_.key)
    objects

let version_summary = function
  | `Object (version : Object.Versions.object_version) ->
      Printf.sprintf "object:%s:latest=%b:size=%s"
        (Object_key.to_string version.key)
        (Option.value ~default:false version.is_latest)
        (Option.fold ~none:"-" ~some:Int64.to_string version.size)
  | `Delete_marker (marker : Object.Versions.delete_marker) ->
      Printf.sprintf "marker:%s:latest=%b"
        (Object_key.to_string marker.key)
        (Option.value ~default:false marker.is_latest)

let version_page_summary (page : Object.Versions.page) =
  List.map (fun version -> `Object version) page.versions
  @ List.map (fun marker -> `Delete_marker marker) page.delete_markers
  |> List.map version_summary

let test_delimiter_common_prefixes () =
  let conn = make_simulator () in
  ignore (put_string conn "logs/a" "a");
  ignore (put_string conn "logs/2026/a" "nested");
  ignore (put_string conn "reports/a" "report");
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
    "objects" [ "logs/a" ] (object_keys page.objects);
  Alcotest.(check (list string))
    "common prefixes" [ "logs/2026/" ]
    (List.map Object_key.Prefix.to_string page.common_prefixes)

let test_start_after_continuation_token () =
  let conn = make_simulator () in
  ignore (put_string conn "a.txt" "a");
  ignore (put_string conn "b.txt" "b");
  ignore (put_string conn "c.txt" "c");
  let options =
    Object.List.options_exn ~start_after:(object_key "a.txt") ~max_keys:1 ()
  in
  let first =
    Simulator.Object.list conn ~bucket:(bucket_name "test-bucket") ~options ()
    |> ok_or_fail "first page"
  in
  Alcotest.(check (list string))
    "first page" [ "b.txt" ]
    (object_keys first.objects);
  Alcotest.(check bool) "first truncated" true first.is_truncated;
  let options =
    Object.List.options_exn ~start_after:(object_key "a.txt") ~max_keys:1
      ?continuation_token:first.next_continuation_token ()
  in
  let second =
    Simulator.Object.list conn ~bucket:(bucket_name "test-bucket") ~options ()
    |> ok_or_fail "second page"
  in
  Alcotest.(check (list string))
    "second page" [ "c.txt" ]
    (object_keys second.objects);
  Alcotest.(check bool) "second truncated" false second.is_truncated

let test_paginator_keys () =
  let conn = make_simulator () in
  ignore (put_string conn "logs/a.txt" "a");
  ignore (put_string conn "logs/b.txt" "b");
  ignore (put_string conn "other.txt" "other");
  let options =
    Object.List.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~max_keys:1 ()
  in
  let keys =
    Simulator.Object.List.keys conn
      ~bucket:(bucket_name "test-bucket")
      ~options ~max_pages:10 ()
    |> ok_or_fail "paginate keys"
    |> List.map Object_key.to_string
  in
  Alcotest.(check (list string)) "keys" [ "logs/a.txt"; "logs/b.txt" ] keys

let test_control_character_continuation_token () =
  let conn = make_simulator () in
  ignore (put_string conn "logs/a\nx" "a");
  ignore (put_string conn "logs/b" "b");
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

let test_version_paginator_delete_markers () =
  let conn = make_simulator () in
  ignore
    (Simulator.Bucket.Versioning.put conn
       ~bucket:(bucket_name "test-bucket")
       ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "enable versioning");
  ignore (put_string conn "logs/a.txt" "one");
  ignore (put_string conn "logs/a.txt" "two-two");
  ignore
    (Simulator.Object.delete conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "logs/a.txt") ()
    |> ok_or_fail "delete current");
  ignore (put_string conn "logs/b.txt" "b");
  let options = Object.Versions.options_exn ~max_keys:1 () in
  let pages =
    Simulator.Object.Versions.pages conn
      ~bucket:(bucket_name "test-bucket")
      ~options ~max_pages:8 ()
    |> ok_or_fail "version pages"
  in
  Alcotest.(check int) "page count" 4 (List.length pages);
  Alcotest.(check (list string))
    "versions"
    [
      "marker:logs/a.txt:latest=true";
      "object:logs/a.txt:latest=false:size=7";
      "object:logs/a.txt:latest=false:size=3";
      "object:logs/b.txt:latest=true:size=1";
    ]
    (List.concat_map version_page_summary pages)

let test_version_delimiter_common_prefixes () =
  let conn = make_simulator () in
  ignore
    (Simulator.Bucket.Versioning.put conn
       ~bucket:(bucket_name "test-bucket")
       ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "enable versioning");
  ignore (put_string conn "logs/2026/a.txt" "nested");
  ignore
    (Simulator.Object.delete conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "logs/2026/a.txt")
       ()
    |> ok_or_fail "delete nested");
  ignore (put_string conn "logs/root.txt" "root");
  let options =
    Object.Versions.options_exn
      ~prefix:(Object_key.Prefix.of_string_exn "logs/")
      ~delimiter:Object.Versions.Delimiter.slash ()
  in
  let page =
    Simulator.Object.list_versions conn
      ~bucket:(bucket_name "test-bucket")
      ~options ()
    |> ok_or_fail "version delimiter"
  in
  Alcotest.(check (list string))
    "versions"
    [ "object:logs/root.txt:latest=true:size=4" ]
    (version_page_summary page);
  Alcotest.(check (list string))
    "common prefixes" [ "logs/2026/" ]
    (List.map Object_key.Prefix.to_string page.common_prefixes)

let suite =
  [
    ( "contract:awskit-s3-sim:list-pagination",
      [
        Alcotest.test_case "delimiter common prefixes" `Quick
          test_delimiter_common_prefixes;
        Alcotest.test_case "start-after continuation token" `Quick
          test_start_after_continuation_token;
        Alcotest.test_case "paginator keys" `Quick test_paginator_keys;
        Alcotest.test_case "control token key" `Quick
          test_control_character_continuation_token;
        Alcotest.test_case "version paginator delete markers" `Quick
          test_version_paginator_delete_markers;
        Alcotest.test_case "version delimiter common prefixes" `Quick
          test_version_delimiter_common_prefixes;
      ] );
  ]
