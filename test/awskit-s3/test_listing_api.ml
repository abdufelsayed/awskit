open Awskit_s3
open Awskit_s3_test

let test_typed_list_objects_api () =
  let bucket = bucket_name "my-bucket" in
  let owner = account_id "123456789012" in
  let prefix = Object_key.Prefix.of_string_exn "logs/" in
  let delimiter = Object.List.Delimiter.slash in
  let start_after = object_key "logs/0001.txt" in
  let continuation_token =
    Object.List.Continuation_token.of_string_exn "opaque-token"
  in
  let options =
    Object.List.options_exn ~prefix ~delimiter ~max_keys:100 ~start_after
      ~continuation_token ~expected_bucket_owner:owner ()
  in
  let listed_object : Object.List.object_summary =
    {
      key = object_key "logs/0002.txt";
      size = Some 1L;
      etag = None;
      last_modified = None;
      storage_class = None;
      checksum = Object.Checksum.empty_summary;
    }
  in
  let page : Object.List.page =
    {
      bucket = Some bucket;
      prefix = Some prefix;
      delimiter = Some delimiter;
      objects = [ listed_object ];
      common_prefixes = [ Object_key.Prefix.of_string_exn "logs/2026/" ];
      key_count = Some 1;
      is_truncated = true;
      continuation_token = Some continuation_token;
      next_continuation_token =
        Some (Object.List.Continuation_token.of_string_exn "next-token");
      response = Awskit.Response.create_exn ~status:200 ();
    }
  in
  ignore (options : Object.List.options);
  ignore
    (page.next_continuation_token : Object.List.Continuation_token.t option);
  ignore (listed_object.key : Object_key.t);
  ignore
    (Recording_s3.Object.List.keys
       Recording_runtime.(connect [])
       ~bucket ~max_pages:2 ()
      : (Object_key.t list, Error.t) result)

let test_typed_list_versions_api () =
  let bucket = bucket_name "my-bucket" in
  let owner = account_id "123456789012" in
  let prefix = Object_key.Prefix.of_string_exn "logs/" in
  let delimiter = Object.Versions.Delimiter.slash in
  let key_marker = object_key "logs/0001.txt" in
  let version_id_marker = Object.Version_id.of_string_exn "version-1" in
  let options =
    Object.Versions.options_exn ~prefix ~delimiter ~max_keys:100 ~key_marker
      ~version_id_marker ~expected_bucket_owner:owner ()
  in
  let version : Object.Versions.object_version =
    {
      key = object_key "logs/0002.txt";
      version_id = Some (Object.Version_id.of_string_exn "version-2");
      is_latest = Some true;
      last_modified = None;
      etag = None;
      size = Some 1L;
      storage_class = None;
      owner = None;
      checksum = Object.Checksum.empty_summary;
    }
  in
  let page : Object.Versions.page =
    {
      bucket = Some bucket;
      prefix = Some prefix;
      delimiter = Some delimiter;
      versions = [ version ];
      delete_markers = [];
      common_prefixes = [ Object_key.Prefix.of_string_exn "logs/2026/" ];
      is_truncated = true;
      key_marker = Some key_marker;
      version_id_marker = Some version_id_marker;
      next_key_marker = Some (object_key "logs/0003.txt");
      next_version_id_marker =
        Some (Object.Version_id.of_string_exn "version-3");
      response = Awskit.Response.create_exn ~status:200 ();
    }
  in
  ignore (options : Object.Versions.options);
  ignore (page.next_key_marker : Object_key.t option);
  ignore (version.key : Object_key.t);
  ignore
    (Recording_s3.Object.Versions.object_versions
       Recording_runtime.(connect [])
       ~bucket ~max_pages:2 ()
      : (Object.Versions.object_version list, Error.t) result)

let suite =
  [
    ( "listing api",
      [
        Alcotest.test_case "typed list objects api" `Quick
          test_typed_list_objects_api;
        Alcotest.test_case "typed list versions api" `Quick
          test_typed_list_versions_api;
      ] );
  ]
