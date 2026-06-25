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
      ] );
  ]

let () = Alcotest.run "awskit-s3-model-contracts" suite
