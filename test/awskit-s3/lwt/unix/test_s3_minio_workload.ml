module S3 = Awskit_s3_lwt_unix
open S3_model
module Bucket = Awskit_s3.Bucket
module Bucket_name = Awskit_s3.Bucket_name
module Command = S3_command
module Model = S3_model
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key
module Tag = Awskit_s3.Tag

let getenv_default name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

let endpoint = getenv_default "AWSKIT_S3_MINIO_ENDPOINT" "http://127.0.0.1:9000"
let unsafe_http = getenv_default "AWSKIT_S3_MINIO_UNSAFE_HTTP" ""
let access_key = getenv_default "AWSKIT_S3_MINIO_ACCESS_KEY_ID" "minioadmin"
let secret_key = getenv_default "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY" "minioadmin"
let region = getenv_default "AWSKIT_S3_MINIO_REGION" "us-east-1"

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:access_key
    ~secret_access_key:secret_key ()

let bucket =
  Bucket_name.of_string_exn
    (Printf.sprintf "awskit-minio-%d-workload" (Unix.getpid ()))

let object_key value = Object_key.of_string_exn value

let tags_to_set tags =
  tags
  |> List.map (fun (key, value) -> Tag.create_exn ~key ~value)
  |> Tag.Set.of_list_exn

let compare_string_pair (left_key, left_value) (right_key, right_value) =
  match String.compare left_key right_key with
  | 0 -> String.compare left_value right_value
  | value -> value

let normalize_tags tags = List.sort compare_string_pair tags

let tags_of_set tags =
  Tag.Set.to_list tags
  |> List.map (fun tag -> (Tag.key tag, Tag.value tag))
  |> normalize_tags

let endpoint_config () =
  let endpoint =
    match Awskit.Endpoint.of_string endpoint with
    | Ok endpoint -> endpoint
    | Error error ->
        Alcotest.failf "minio endpoint: %a" Awskit_s3.Error.pp error
  in
  let ok_config = function
    | Ok config -> config
    | Error error ->
        Alcotest.failf "minio endpoint policy: %a" Awskit_s3.Error.pp error
  in
  let signing_region = Awskit.Region.of_string_exn region in
  match Awskit.Endpoint.scheme endpoint with
  | `Https ->
      Awskit_s3.Endpoint_config.s3_compatible ~endpoint ~signing_region
        ~addressing_style:`Path ~tls_policy:`Https_required
        ~feature_policy:`S3_compatible ()
      |> ok_config
  | `Http when String.equal unsafe_http "1" ->
      Awskit_s3.Endpoint_config.unsafe_plaintext ~endpoint ~signing_region
        ~addressing_style:`Path ()
  | `Http -> (
      match
        Awskit_s3.Endpoint_config.local_plaintext ~endpoint ~signing_region
          ~addressing_style:`Path ()
      with
      | Ok config -> config
      | Error _ ->
          Alcotest.fail
            "non-local HTTP MinIO endpoint requires \
             AWSKIT_S3_MINIO_UNSAFE_HTTP=1")

let connect () =
  match
    S3.create ~endpoint_config:(endpoint_config ()) ~region ~credentials
      ~clock:Ptime_clock.now ()
  with
  | Ok conn -> conn
  | Error error -> Alcotest.failf "connect: %a" Awskit_s3.Error.pp error

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let await_result _label promise = Lwt_main.run promise
let await_ok label promise = Lwt_main.run promise |> ok_or_fail label
let delete_object_key key = Object.Delete_many.object_ ~key ()

let delete_object_version key version_id =
  match version_id with
  | Some version_id -> Object.Delete_many.object_ ~key ~version_id ()
  | None -> delete_object_key key

let cleanup_context bucket error =
  Awskit.Error.Producer.with_context
    (Printf.sprintf "cleaning MinIO bucket %s" (Bucket_name.to_string bucket))
    error

let delete_item_cleanup_error (error : Object.Delete_many.item_error) =
  Awskit.Error.Producer.service ~status:200 ~code:error.code
    ?message:error.message ~headers:[] ()
  |> Awskit.Error.Producer.with_context
       (Printf.sprintf "delete_objects item failed for key %s"
          (Object_key.to_string error.key))

let cleanup_delete_objects conn ~bucket objects =
  let open Lwt.Syntax in
  match objects with
  | [] -> Lwt.return_ok ()
  | _ -> (
      let* result = S3.Object.delete_objects conn ~bucket ~objects () in
      match result with
      | Error error ->
          Lwt.return_error
            (Awskit.Error.Producer.with_context "deleting MinIO cleanup objects"
               error)
      | Ok ({ errors = []; _ } : Object.Delete_many.result) -> Lwt.return_ok ()
      | Ok result ->
          Lwt.return_error
            (List.map delete_item_cleanup_error result.errors
            |> Awskit.Error.Producer.multiple
            |> Awskit.Error.Producer.with_context
                 "deleting MinIO cleanup objects"))

let cleanup_objects conn ~bucket =
  let open Lwt.Syntax in
  let* versions_result =
    S3.Object.Versions.pages conn ~bucket ~max_pages:100 ()
  in
  match versions_result with
  | Ok pages ->
      let objects =
        List.concat_map
          (fun (page : Object.Versions.page) ->
            let versions =
              List.map
                (fun (version : Object.Versions.object_version) ->
                  delete_object_version version.key version.version_id)
                page.versions
            in
            let markers =
              List.map
                (fun (marker : Object.Versions.delete_marker) ->
                  delete_object_version marker.key marker.version_id)
                page.delete_markers
            in
            versions @ markers)
          pages
      in
      cleanup_delete_objects conn ~bucket objects
  | Error error when Awskit_s3.Error.is_no_such_bucket error -> Lwt.return_ok ()
  | Error versions_error -> (
      let* keys_result = S3.Object.List.keys conn ~bucket ~max_pages:100 () in
      match keys_result with
      | Ok keys ->
          let objects = List.map delete_object_key keys in
          cleanup_delete_objects conn ~bucket objects
      | Error error when Awskit_s3.Error.is_no_such_bucket error ->
          Lwt.return_ok ()
      | Error keys_error ->
          Lwt.return_error
            (Awskit.Error.Producer.multiple [ versions_error; keys_error ]
            |> Awskit.Error.Producer.with_context
                 "listing MinIO cleanup objects"))

let cleanup_bucket_result conn ~bucket =
  let open Lwt.Syntax in
  let* objects_result = cleanup_objects conn ~bucket in
  let* bucket_result = S3.Bucket.delete conn ~bucket () in
  match (objects_result, bucket_result) with
  | Ok (), Ok _ -> Lwt.return_ok ()
  | Ok (), Error error when Awskit_s3.Error.is_no_such_bucket error ->
      Lwt.return_ok ()
  | Ok (), Error error ->
      Lwt.return_error
        (Awskit.Error.Producer.with_context "deleting MinIO cleanup bucket"
           error
        |> cleanup_context bucket)
  | Error error, Ok _ -> Lwt.return_error (cleanup_context bucket error)
  | Error error, Error bucket_error
    when Awskit_s3.Error.is_no_such_bucket bucket_error ->
      Lwt.return_error (cleanup_context bucket error)
  | Error object_error, Error bucket_error ->
      Lwt.return_error
        (Awskit.Error.Producer.multiple
           [
             object_error;
             Awskit.Error.Producer.with_context "deleting MinIO cleanup bucket"
               bucket_error;
           ]
        |> cleanup_context bucket)

let cleanup_bucket_or_fail conn ~bucket =
  match Lwt_main.run (cleanup_bucket_result conn ~bucket) with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "MinIO cleanup failed: %a" Awskit_s3.Error.pp error

let report_cleanup_after_primary_failure error =
  Format.eprintf
    "@[<v>MinIO cleanup failed after the primary workload failure; preserving \
     the primary failure.@;\
     %a@]@."
    Awskit_s3.Error.pp error

let protect_with_bucket_cleanup conn ~bucket f =
  match f () with
  | value ->
      cleanup_bucket_or_fail conn ~bucket;
      value
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (match Lwt_main.run (cleanup_bucket_result conn ~bucket) with
      | Ok () -> ()
      | Error error -> report_cleanup_after_primary_failure error);
      Printexc.raise_with_backtrace exn backtrace

let error_shape error =
  if Awskit_s3.Error.is_no_such_bucket error then "service:NoSuchBucket"
  else if Awskit_s3.Error.is_no_such_key error then "service:NoSuchKey"
  else
    match Awskit.Error.kind error with
    | Validation _ ->
        Printf.sprintf "validation:%s"
          (Option.value ~default:"<none>" (Awskit.Error.validation_field error))
    | Service _ ->
        Printf.sprintf "service:%s:%s"
          (Option.value ~default:"<none>"
             (Awskit.Error.service_status error |> Option.map string_of_int))
          (Option.value ~default:"<none>" (Awskit.Error.service_code error))
    | Body _ -> "body"
    | Decode _ -> "decode"
    | Transport _ -> "transport"
    | Credentials _ -> "credentials"
    | Signing _ -> "signing"
    | Endpoint _ -> "endpoint"
    | Timeout _ -> "timeout"
    | Cancelled _ -> "cancelled"
    | Retry_exhausted _ -> "retry-exhausted"
    | Not_supported _ -> "not-supported"
    | Multiple _ -> "multiple"

let is_no_such_tag_set error =
  match Awskit.Error.service_code error with
  | Some "NoSuchTagSet" -> true
  | _ -> false

let is_absent_object_error error =
  Awskit_s3.Error.is_no_such_key error
  ||
  match
    (Awskit.Error.service_status error, Awskit.Error.service_code error)
  with
  | Some 404, None -> true
  | _ -> false

let fail command_index command message =
  QCheck.Test.fail_reportf "command #%d %s: %s" command_index
    (Command.to_string command)
    message

let fail_error command_index command label error =
  fail command_index command
    (Printf.sprintf "%s unexpected error shape %s" label (error_shape error))

let expect_no_such_key command_index command label = function
  | Error error when is_absent_object_error error -> ()
  | Error error -> fail_error command_index command label error
  | Ok _ -> fail command_index command (label ^ " expected NoSuchKey")

let expect_ok command_index command label = function
  | Ok value -> value
  | Error error -> fail_error command_index command label error

let check_equal command_index command testable label expected actual =
  Alcotest.check testable
    (Printf.sprintf "command #%d %s: %s" command_index
       (Command.to_string command)
       label)
    expected actual

let check_version_id_presence command_index command label expected version_id =
  check_equal command_index command Alcotest.bool label expected
    (Option.is_some version_id)

let check_tags command_index command label expected actual =
  check_equal command_index command
    Alcotest.(list (pair string string))
    label (normalize_tags expected) (tags_of_set actual)

let assert_get command_index command conn key expected =
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get_string"
          (await_result "get_string"
             (S3.Object.get_string conn ~bucket ~key:(object_key key)
                ~max_bytes:64L ()))
      in
      check_equal command_index command Alcotest.string "get body" object_.body
        result.value;
      check_version_id_presence command_index command "get version id"
        object_.has_version_id result.version_id
  | None ->
      expect_no_such_key command_index command "get_string"
        (await_result "get_string"
           (S3.Object.get_string conn ~bucket ~key:(object_key key)
              ~max_bytes:64L ()))

let assert_find command_index command conn key expected =
  match
    await_result "find_string"
      (S3.Object.find_string conn ~bucket ~key:(object_key key) ~max_bytes:64L
         ())
  with
  | Ok (Some result) -> (
      match expected with
      | Some object_ ->
          check_equal command_index command Alcotest.string "find body"
            object_.body result.value;
          check_version_id_presence command_index command "find version id"
            object_.has_version_id result.version_id
      | None -> fail command_index command "find_string expected Ok None")
  | Ok None -> (
      match expected with
      | None -> ()
      | Some _ -> fail command_index command "find_string expected object")
  | Error error -> fail_error command_index command "find_string" error

let assert_head command_index command conn key expected =
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "head"
          (await_result "head"
             (S3.Object.head conn ~bucket ~key:(object_key key) ()))
      in
      check_equal command_index command
        Alcotest.(option int64)
        "head content length"
        (Some (Int64.of_int (String.length object_.body)))
        result.content_length;
      check_version_id_presence command_index command "head version id"
        object_.has_version_id result.version_id
  | None ->
      expect_no_such_key command_index command "head"
        (await_result "head"
           (S3.Object.head conn ~bucket ~key:(object_key key) ()))

let assert_exists command_index command conn key expected =
  let result =
    expect_ok command_index command "exists"
      (await_result "exists"
         (S3.Object.exists conn ~bucket ~key:(object_key key) ()))
  in
  check_equal command_index command Alcotest.bool "exists"
    (Option.is_some expected) result

let assert_object_tags command_index command conn key expected =
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get object tags"
          (await_result "get object tags"
             (S3.Object.Tagging.get conn ~bucket ~key:(object_key key) ()))
      in
      check_tags command_index command "object tags" object_.tags result.tags
  | None ->
      expect_no_such_key command_index command "get object tags"
        (await_result "get object tags"
           (S3.Object.Tagging.get conn ~bucket ~key:(object_key key) ()))

let assert_put_object_tags command_index command conn key tags expected =
  match expected with
  | Some _ ->
      ignore
        (expect_ok command_index command "put object tags"
           (await_result "put object tags"
              (S3.Object.Tagging.put conn ~bucket ~key:(object_key key)
                 ~tags:(tags_to_set tags) ()))
          : Awskit.Response.t)
  | None ->
      expect_no_such_key command_index command "put object tags"
        (await_result "put object tags"
           (S3.Object.Tagging.put conn ~bucket ~key:(object_key key)
              ~tags:(tags_to_set tags) ()))

let assert_delete_object_tags command_index command conn key expected =
  match expected with
  | Some _ ->
      ignore
        (expect_ok command_index command "delete object tags"
           (await_result "delete object tags"
              (S3.Object.Tagging.delete conn ~bucket ~key:(object_key key) ()))
          : Awskit.Response.t)
  | None ->
      expect_no_such_key command_index command "delete object tags"
        (await_result "delete object tags"
           (S3.Object.Tagging.delete conn ~bucket ~key:(object_key key) ()))

let assert_bucket_tags command_index command conn expected =
  match
    await_result "get bucket tags" (S3.Bucket.Tagging.get conn ~bucket ())
  with
  | Ok result ->
      check_tags command_index command "bucket tags" expected result.tags
  | Error error when expected = [] && is_no_such_tag_set error -> ()
  | Error error -> fail_error command_index command "get bucket tags" error

let assert_put_bucket_tags command_index command conn tags =
  ignore
    (expect_ok command_index command "put bucket tags"
       (await_result "put bucket tags"
          (S3.Bucket.Tagging.put conn ~bucket ~tags:(tags_to_set tags) ()))
      : Awskit.Response.t)

let assert_delete_bucket_tags command_index command conn =
  ignore
    (expect_ok command_index command "delete bucket tags"
       (await_result "delete bucket tags"
          (S3.Bucket.Tagging.delete conn ~bucket ()))
      : Awskit.Response.t)

let assert_get_versioning command_index command conn expected =
  let status_to_string = Option.map Bucket.Versioning.Status.to_string in
  let result =
    expect_ok command_index command "get versioning"
      (await_result "get versioning" (S3.Bucket.Versioning.get conn ~bucket ()))
  in
  check_equal command_index command
    Alcotest.(option string)
    "versioning status"
    (status_to_string expected)
    (status_to_string result.status)

let assert_put_versioning command_index command conn status =
  ignore
    (expect_ok command_index command "put versioning"
       (await_result "put versioning"
          (S3.Bucket.Versioning.put conn ~bucket ~status ()))
      : Awskit.Response.t)

let assert_delete command_index command conn key ~keeps_history expected =
  let result =
    expect_ok command_index command "delete"
      (await_result "delete"
         (S3.Object.delete conn ~bucket ~key:(object_key key) ()))
  in
  let expects_delete_marker = keeps_history && Option.is_some expected in
  check_version_id_presence command_index command "delete version id"
    expects_delete_marker result.version_id;
  check_equal command_index command Alcotest.bool "delete marker"
    expects_delete_marker
    (Option.value ~default:false result.delete_marker)

let list_keys command_index command conn ?options () =
  expect_ok command_index command "list keys"
    (await_result "list keys"
       (S3.Object.List.keys conn ~bucket ?options ~max_pages:8 ()))
  |> List.map Object_key.to_string

let assert_list_keys command_index command conn model =
  check_equal command_index command
    Alcotest.(list string)
    "list keys" (Model.keys model)
    (list_keys command_index command conn ())

let assert_list_prefix command_index command conn prefix model =
  let options =
    Object.List.options_exn ~prefix:(Object_key.Prefix.of_string_exn prefix) ()
  in
  check_equal command_index command
    Alcotest.(list string)
    "list prefix keys"
    (Model.keys_with_prefix prefix model)
    (list_keys command_index command conn ~options ())

let assert_copy command_index command conn ~source_key ~destination_key
    ~destination_has_version_id expected =
  match expected with
  | Some _source ->
      let result =
        expect_ok command_index command "copy"
          (await_result "copy"
             (S3.Object.copy conn ~source_bucket:bucket
                ~source_key:(object_key source_key) ~destination_bucket:bucket
                ~destination_key:(object_key destination_key)
                ()))
      in
      check_version_id_presence command_index command
        "copy destination version id" destination_has_version_id
        result.version_id
  | None ->
      expect_no_such_key command_index command "copy"
        (await_result "copy"
           (S3.Object.copy conn ~source_bucket:bucket
              ~source_key:(object_key source_key) ~destination_bucket:bucket
              ~destination_key:(object_key destination_key)
              ()))

let check_store command_index command conn model =
  assert_list_keys command_index command conn model;
  List.iter
    (fun (key, body) ->
      let result =
        expect_ok command_index command "store get"
          (await_result "store get"
             (S3.Object.get_string conn ~bucket ~key:(object_key key)
                ~max_bytes:64L ()))
      in
      check_equal command_index command Alcotest.string
        (Printf.sprintf "store body %s" key)
        body result.value)
    (Model.objects_as_strings model);
  assert_bucket_tags command_index command conn model.bucket_tags;
  assert_get_versioning command_index command conn model.versioning

let apply_minio_command command_index conn model command =
  let next_model =
    match command with
    | Command.Put_string (key, body, tags) ->
        let options =
          match tags with
          | [] -> None
          | _ -> Some (Object.Put.options_exn ~tags:(tags_to_set tags) ())
        in
        let result =
          expect_ok command_index command "put_string"
            (await_result "put_string"
               (S3.Object.put_string conn ~bucket ~key:(object_key key) ?options
                  ~contents:body ()))
        in
        check_version_id_presence command_index command "put version id"
          (Model.versioning_keeps_history model)
          result.version_id;
        let next_model = Model.apply command model in
        assert_object_tags command_index command conn key
          (Model.find key next_model);
        next_model
    | Get_string key ->
        assert_get command_index command conn key (Model.find key model);
        model
    | Find_string key ->
        assert_find command_index command conn key (Model.find key model);
        model
    | Head_object key ->
        assert_head command_index command conn key (Model.find key model);
        model
    | Exists_object key ->
        assert_exists command_index command conn key (Model.find key model);
        model
    | Delete_object key ->
        let expected = Model.find key model in
        assert_delete command_index command conn key
          ~keeps_history:(Model.versioning_keeps_history model)
          expected;
        Model.apply command model
    | List_keys ->
        assert_list_keys command_index command conn model;
        model
    | List_prefix prefix ->
        assert_list_prefix command_index command conn prefix model;
        model
    | Copy_object (source_key, destination_key) ->
        let source = Model.find source_key model in
        assert_copy command_index command conn ~source_key ~destination_key
          ~destination_has_version_id:(Model.versioning_keeps_history model)
          source;
        let next_model = Model.apply command model in
        (match source with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn destination_key
              (Model.find destination_key next_model));
        next_model
    | Put_object_tags (key, tags) ->
        let expected = Model.find key model in
        assert_put_object_tags command_index command conn key tags expected;
        let next_model = Model.apply command model in
        (match expected with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn key
              (Model.find key next_model));
        next_model
    | Get_object_tags key ->
        assert_object_tags command_index command conn key (Model.find key model);
        model
    | Delete_object_tags key ->
        let expected = Model.find key model in
        assert_delete_object_tags command_index command conn key expected;
        let next_model = Model.apply command model in
        (match expected with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn key
              (Model.find key next_model));
        next_model
    | Put_bucket_tags tags ->
        assert_put_bucket_tags command_index command conn tags;
        let next_model = Model.apply command model in
        assert_bucket_tags command_index command conn next_model.bucket_tags;
        next_model
    | Get_bucket_tags ->
        assert_bucket_tags command_index command conn model.bucket_tags;
        model
    | Delete_bucket_tags ->
        assert_delete_bucket_tags command_index command conn;
        let next_model = Model.apply command model in
        assert_bucket_tags command_index command conn next_model.bucket_tags;
        next_model
    | Put_versioning status ->
        assert_put_versioning command_index command conn status;
        let next_model = Model.apply command model in
        assert_get_versioning command_index command conn next_model.versioning;
        next_model
    | Get_versioning ->
        assert_get_versioning command_index command conn model.versioning;
        model
  in
  next_model

module Minio_target : S3_workload.TARGET = struct
  let name = "minio"
  let bucket = bucket

  type connection = S3.t

  let with_connection f =
    let conn = connect () in
    cleanup_bucket_or_fail conn ~bucket;
    ignore
      (await_ok "create bucket" (S3.Bucket.create conn ~bucket ())
        : Bucket.Create.result);
    protect_with_bucket_cleanup conn ~bucket (fun () -> f conn)

  let check_store = check_store
  let run_command = apply_minio_command
end

module Workload =
  S3_workload.Make_profile
    (struct
      let profile = S3_workload.Minio
    end)
    (Minio_target)

let () = Alcotest.run "awskit-s3-minio-workload" Workload.suite
