let object_body = function
  | None -> None
  | Some (object_ : S3_model.object_) -> Some object_.body

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
       ])

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
        Alcotest.test_case
          "minio profile excludes versioning after current objects" `Quick
          test_minio_profile_excludes_versioning_after_current_objects;
        Alcotest.test_case "replay round trips generated commands" `Quick
          test_replay_round_trips_generated_commands;
        Alcotest.test_case "replay rejects unknown versioning status" `Quick
          test_replay_rejects_unknown_versioning_status;
      ] );
  ]

let () = Alcotest.run "awskit-s3-model-contracts" suite
