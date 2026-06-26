let object_body = function
  | None -> None
  | Some (object_ : S3_model.object_) -> Some object_.body

let range_bytes start finish = Awskit_s3.Range.bytes_exn ~start ~finish
let range_from start = Awskit_s3.Range.from_exn start
let range_suffix length = Awskit_s3.Range.suffix_exn length

let content_range_summary_to_string
    (summary : S3_model.content_range_summary option) =
  match summary with
  | None -> None
  | Some { start; finish; complete_length } ->
      let complete_length =
        match complete_length with
        | None -> "*"
        | Some length -> Int64.to_string length
      in
      Some (Printf.sprintf "bytes %Ld-%Ld/%s" start finish complete_length)

let expect_get_summary ~label ~body ~content_range result =
  match result with
  | S3_model.Get_ok summary ->
      Alcotest.(check string) (label ^ " body") body summary.read_body;
      Alcotest.(check (option int64))
        (label ^ " content length")
        (Some (Int64.of_int (String.length body)))
        summary.read_content_length;
      Alcotest.(check (option string))
        (label ^ " content range") content_range
        (content_range_summary_to_string summary.read_content_range)
  | result ->
      Alcotest.failf "%s expected get-ok, got %s" label
        (S3_model.operation_result_kind result)

let expect_result_kind ~label expected result =
  Alcotest.(check string) label expected (S3_model.operation_result_kind result)

let test_put_get_delete () =
  let model = S3_model.put "a.txt" "alpha" [] S3_model.empty in
  Alcotest.(check (option string))
    "put stores object" (Some "alpha")
    (object_body (S3_model.find "a.txt" model));
  let model = S3_model.delete "a.txt" model in
  Alcotest.(check (option string))
    "delete removes object" None
    (object_body (S3_model.find "a.txt" model))

let test_versioning_promotes_existing_objects () =
  let model = S3_model.put "a.txt" "alpha" [] S3_model.empty in
  let model =
    S3_model.put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled model
  in
  Alcotest.(check bool)
    "current object has version id" true
    (match S3_model.find "a.txt" model with
    | None -> false
    | Some object_ -> object_.has_version_id);
  Alcotest.(check bool)
    "listed null version exists" true
    (List.exists
       (fun (version : S3_model.listed_version) ->
         String.equal version.key "a.txt"
         && version.kind = `Object
         && version.has_version_id
         && version.is_latest)
       (S3_model.listed_versions model))

let test_list_keys_page_respects_prefix_and_limit () =
  let model =
    S3_model.empty
    |> S3_model.put "logs/b.txt" "bravo" []
    |> S3_model.put "a.txt" "alpha" []
    |> S3_model.put "logs/a.txt" "log" []
  in
  Alcotest.(check (list string))
    "first prefixed page" [ "logs/a.txt" ]
    (S3_model.list_keys_page ~prefix:"logs/" ~max_keys:1 model)

let listed_version_summary (version : S3_model.listed_version) =
  Printf.sprintf "%s:%s:version_id=%b:latest=%b:size=%s" version.key
    (match version.kind with `Object -> "object" | `Delete_marker -> "marker")
    version.has_version_id version.is_latest
    (match version.size with None -> "-" | Some size -> Int64.to_string size)

let test_list_versions_page_respects_limit () =
  let model =
    S3_model.empty
    |> S3_model.put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled
    |> S3_model.put "a.txt" "alpha" []
    |> S3_model.delete "a.txt"
  in
  Alcotest.(check (list string))
    "first version page"
    [ "a.txt:marker:version_id=true:latest=true:size=-" ]
    (S3_model.list_versions_page ~max_keys:1 model
    |> List.map listed_version_summary)

let test_expected_result_describes_core_operations () =
  let model =
    S3_model.empty
    |> S3_model.put "a.txt" "alpha" [ ("env", "dev") ]
    |> S3_model.put_bucket_tags [ ("team", "storage") ]
  in
  expect_result_kind ~label:"put result" "put-ok"
    (S3_model.expected_result
       (S3_command.Put_string ("b.txt", "bravo", []))
       model);
  expect_result_kind ~label:"get result" "get-ok"
    (S3_model.expected_result (S3_command.Get_string "a.txt") model);
  expect_result_kind ~label:"find result" "find-ok"
    (S3_model.expected_result (S3_command.Find_string "missing.txt") model);
  expect_result_kind ~label:"head result" "head-ok"
    (S3_model.expected_result (S3_command.Head_object "a.txt") model);
  expect_result_kind ~label:"exists result" "exists-ok"
    (S3_model.expected_result (S3_command.Exists_object "missing.txt") model);
  expect_result_kind ~label:"list result" "list-keys-ok"
    (S3_model.expected_result S3_command.List_keys model);
  expect_result_kind ~label:"object tags result" "object-tags-ok"
    (S3_model.expected_result (S3_command.Get_object_tags "a.txt") model);
  expect_result_kind ~label:"bucket tags result" "bucket-tags-ok"
    (S3_model.expected_result S3_command.Get_bucket_tags model);
  expect_result_kind ~label:"versioning result" "versioning-ok"
    (S3_model.expected_result S3_command.Get_versioning model)

let test_range_get_expected_result_boundaries () =
  let model =
    S3_model.empty
    |> S3_model.put "a.txt" "abcdef" []
    |> S3_model.put "empty.txt" "" []
  in
  expect_get_summary ~label:"bytes interior" ~body:"cde"
    ~content_range:(Some "bytes 2-4/6")
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_bytes 2L 4L))
       model);
  expect_get_summary ~label:"bytes clipped finish" ~body:"ef"
    ~content_range:(Some "bytes 4-5/6")
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_bytes 4L 99L))
       model);
  expect_get_summary ~label:"from offset" ~body:"def"
    ~content_range:(Some "bytes 3-5/6")
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_from 3L))
       model);
  expect_get_summary ~label:"suffix subset" ~body:"ef"
    ~content_range:(Some "bytes 4-5/6")
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_suffix 2L))
       model);
  expect_get_summary ~label:"suffix whole object" ~body:"abcdef"
    ~content_range:(Some "bytes 0-5/6")
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_suffix 99L))
       model);
  expect_result_kind ~label:"from eof is invalid" "invalid-range"
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_from 6L))
       model);
  expect_result_kind ~label:"bytes past eof is invalid" "invalid-range"
    (S3_model.expected_result
       (S3_command.Get_range ("a.txt", range_bytes 6L 9L))
       model);
  expect_result_kind ~label:"suffix on empty object is invalid" "invalid-range"
    (S3_model.expected_result
       (S3_command.Get_range ("empty.txt", range_suffix 1L))
       model);
  expect_result_kind ~label:"missing object is not found" "not-found"
    (S3_model.expected_result
       (S3_command.Get_range ("missing.txt", range_suffix 1L))
       model)

let test_minio_profile_excludes_versioning_after_current_objects () =
  Alcotest.(check bool)
    "strict profile keeps AWS versioning-after-put history" true
    (S3_history.history_supported S3_history.strict_default
       [
         S3_command.Put_string ("a.txt", "", []);
         S3_command.Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
       ]);
  Alcotest.(check bool)
    "minio profile excludes versioning-after-put history" false
    (S3_history.history_supported S3_history.minio_default
       [
         S3_command.Put_string ("a.txt", "", []);
         S3_command.Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
       ]);
  Alcotest.(check bool)
    "minio profile can enable versioning before writes" true
    (S3_history.history_supported S3_history.minio_default
       [
         S3_command.Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
         S3_command.Put_string ("a.txt", "", []);
       ]);
  Alcotest.(check bool)
    "minio profile keeps ranged reads supported" true
    (S3_history.history_supported S3_history.minio_default
       [
         S3_command.Put_string ("a.txt", "abcdef", []);
         Get_range ("a.txt", range_bytes 1L 3L);
       ])

let require_bins ~label ~required observed =
  let missing = List.filter (fun bin -> not (List.mem bin observed)) required in
  match missing with
  | [] -> ()
  | _ :: _ ->
      Alcotest.failf "%s missing bins:\n%s\n\nobserved:\n%s" label
        (String.concat "\n" missing)
        (String.concat "\n" observed)

let test_history_transition_bins_name_common_s3_flows () =
  let commands =
    [
      S3_command.Put_string ("a.txt", "alpha", []);
      Get_string "a.txt";
      Put_string ("b.txt", "bravo", []);
      Delete_object "b.txt";
      Put_string ("logs/a.txt", "log-a", []);
      Get_range ("logs/a.txt", range_bytes 0L 2L);
      Put_string ("logs/b.txt", "log-b", []);
      Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
      Delete_object "logs/a.txt";
      Put_object_tags ("a.txt", [ ("env", "dev") ]);
      Get_object_tags "a.txt";
      Copy_object ("a.txt", "photos/2026.jpg");
      Head_object "photos/2026.jpg";
      List_keys_page { prefix = None; max_keys = 1 };
    ]
  in
  require_bins ~label:"history transition coverage"
    ~required:
      [
        "s3.command.get-range";
        "s3.history.put-read";
        "s3.history.put-range-read";
        "s3.history.put-delete";
        "s3.history.versioning-enabled-after-existing-object";
        "s3.history.versioning-enabled-delete";
        "s3.history.object-tag-mutation-read";
        "s3.history.copy-destination-read";
        "s3.history.list-page-after-multiple-visible-keys";
      ]
    (S3_history.coverage_bins commands)

let test_model_state_bins_name_generated_history_states () =
  let commands =
    [
      S3_command.Put_string ("a.txt", "alpha", [ ("env", "dev") ]);
      Put_string ("b.txt", "bravo", []);
      Put_bucket_tags [ ("team", "storage") ];
      Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
      Delete_object "a.txt";
      Put_versioning Awskit_s3.Bucket.Versioning.Status.Suspended;
    ]
  in
  require_bins ~label:"model state coverage"
    ~required:
      [
        "s3.state.empty";
        "s3.state.one-object";
        "s3.state.multiple-objects";
        "s3.state.versioning-enabled";
        "s3.state.versioning-suspended";
        "s3.state.delete-marker-present";
        "s3.state.bucket-tags-present";
        "s3.state.object-tags-present";
      ]
    (S3_history.coverage_bins commands)

let test_replay_round_trips_generated_commands () =
  let commands =
    [
      S3_command.Put_string
        ( "space key.txt",
          "line 1\n\000line 2",
          [ ("path/key", "x@y"); ("empty", "") ] );
      Put_string_metadata
        ( "unicode-\206\180.txt",
          "",
          [],
          [ ("trace-id", "sim-1"); ("multi", "line\nvalue") ] );
      Get_string "space key.txt";
      Get_range ("space key.txt", range_bytes 2L 5L);
      Get_range ("unicode-\206\180.txt", range_from 0L);
      Get_range ("logs/a.txt", range_suffix 3L);
      Find_string "missing key.txt";
      Head_object "unicode-\206\180.txt";
      Exists_object "logs/a.txt";
      Delete_object "logs/b.txt";
      List_keys;
      List_prefix "logs/";
      List_keys_page { prefix = None; max_keys = 1 };
      List_keys_page { prefix = Some "photos/"; max_keys = 2 };
      List_versions_page { max_keys = 3 };
      Copy_object ("copy/source-object", "copy/destination-object");
      Copy_object_metadata
        ("copy/source-object", "copy/destination-object", Copy_source_metadata);
      Copy_object_metadata
        ( "copy/source-object",
          "copy/destination-object",
          Replace_metadata [ ("author", "awskit"); ("purpose", "stateful-pbt") ]
        );
      Put_object_tags ("a.txt", [ ("env", "dev"); ("owner", "sdk") ]);
      Get_object_tags "a.txt";
      Delete_object_tags "a.txt";
      Put_bucket_tags [ ("team", "storage"); ("mode", "pbt") ];
      Get_bucket_tags;
      Delete_bucket_tags;
      Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled;
      Put_versioning Awskit_s3.Bucket.Versioning.Status.Suspended;
      Get_versioning;
    ]
  in
  match S3_replay.decode (S3_replay.encode commands) with
  | Ok decoded -> Alcotest.(check bool) "commands" true (decoded = commands)
  | Error error -> Alcotest.fail (S3_replay.parse_error_to_string error)

let test_replay_rejects_unknown_versioning_status () =
  match S3_replay.decode "replay-v1 put-versioning h556e6b6e6f776e" with
  | Ok _ -> Alcotest.fail "expected replay parser to reject Unknown status"
  | Error _ -> ()

let suite =
  [
    ( "contract:awskit-s3:model",
      [
        Alcotest.test_case "put get delete" `Quick test_put_get_delete;
        Alcotest.test_case "versioning promotes existing objects" `Quick
          test_versioning_promotes_existing_objects;
        Alcotest.test_case "list keys page respects prefix and limit" `Quick
          test_list_keys_page_respects_prefix_and_limit;
        Alcotest.test_case "list versions page respects limit" `Quick
          test_list_versions_page_respects_limit;
        Alcotest.test_case "expected result describes core operations" `Quick
          test_expected_result_describes_core_operations;
        Alcotest.test_case "range get expected result boundaries" `Quick
          test_range_get_expected_result_boundaries;
        Alcotest.test_case
          "minio profile excludes versioning after current objects" `Quick
          test_minio_profile_excludes_versioning_after_current_objects;
        Alcotest.test_case "history transition bins name common S3 flows" `Quick
          test_history_transition_bins_name_common_s3_flows;
        Alcotest.test_case "model state bins name generated history states"
          `Quick test_model_state_bins_name_generated_history_states;
        Alcotest.test_case "replay round trips generated commands" `Quick
          test_replay_round_trips_generated_commands;
        Alcotest.test_case "replay rejects unknown versioning status" `Quick
          test_replay_rejects_unknown_versioning_status;
      ] );
  ]

let () = Alcotest.run "awskit-s3-model-contracts" suite
