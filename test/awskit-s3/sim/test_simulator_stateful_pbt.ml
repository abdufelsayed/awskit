open Awskit_s3
open S3_model
module Command = S3_command
module Model = S3_model
module Simulator = Awskit_s3_sim

let test_time = Ptime.epoch

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let bucket = Bucket_name.of_string_exn "stateful-pbt-bucket"
let key_to_object_key key = Object_key.of_string_exn key

let tags_to_set tags =
  tags
  |> List.map (fun (key, value) -> Tag.create_exn ~key ~value)
  |> Tag.Set.of_list_exn

let tags_of_set tags =
  Tag.Set.to_list tags |> List.map (fun tag -> (Tag.key tag, Tag.value tag))

let error_shape error =
  if Error.is_no_such_bucket error then "service:NoSuchBucket"
  else if Error.is_no_such_key error then "service:NoSuchKey"
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

let fail command_index command message =
  QCheck.Test.fail_reportf "command #%d %s: %s" command_index
    (Command.to_string command)
    message

let fail_error command_index command label error =
  fail command_index command
    (Printf.sprintf "%s unexpected error shape %s" label (error_shape error))

let expect_no_such_key command_index command label = function
  | Error error when Error.is_no_such_key error -> ()
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
    label expected (tags_of_set actual)

let assert_get command_index command conn key expected =
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get_string"
          (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
             ~max_bytes:64L ())
      in
      check_equal command_index command Alcotest.string "get body" object_.body
        result.value;
      check_version_id_presence command_index command "get version id"
        object_.has_version_id result.version_id
  | None ->
      expect_no_such_key command_index command "get_string"
        (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
           ~max_bytes:64L ())

let assert_find command_index command conn key expected =
  match
    Simulator.Object.find_string conn ~bucket ~key:(key_to_object_key key)
      ~max_bytes:64L ()
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
          (Simulator.Object.head conn ~bucket ~key:(key_to_object_key key) ())
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
        (Simulator.Object.head conn ~bucket ~key:(key_to_object_key key) ())

let assert_exists command_index command conn key expected =
  let result =
    expect_ok command_index command "exists"
      (Simulator.Object.exists conn ~bucket ~key:(key_to_object_key key) ())
  in
  check_equal command_index command Alcotest.bool "exists"
    (Option.is_some expected) result

let assert_object_tags command_index command conn key expected =
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get object tags"
          (Simulator.Object.Tagging.get conn ~bucket
             ~key:(key_to_object_key key) ())
      in
      check_tags command_index command "object tags" object_.tags result.tags
  | None ->
      expect_no_such_key command_index command "get object tags"
        (Simulator.Object.Tagging.get conn ~bucket ~key:(key_to_object_key key)
           ())

let assert_put_object_tags command_index command conn key tags expected =
  match expected with
  | Some _ ->
      ignore
        (expect_ok command_index command "put object tags"
           (Simulator.Object.Tagging.put conn ~bucket
              ~key:(key_to_object_key key) ~tags:(tags_to_set tags) ())
          : Awskit.Response.t)
  | None ->
      expect_no_such_key command_index command "put object tags"
        (Simulator.Object.Tagging.put conn ~bucket ~key:(key_to_object_key key)
           ~tags:(tags_to_set tags) ())

let assert_delete_object_tags command_index command conn key expected =
  match expected with
  | Some _ ->
      ignore
        (expect_ok command_index command "delete object tags"
           (Simulator.Object.Tagging.delete conn ~bucket
              ~key:(key_to_object_key key) ())
          : Awskit.Response.t)
  | None ->
      expect_no_such_key command_index command "delete object tags"
        (Simulator.Object.Tagging.delete conn ~bucket
           ~key:(key_to_object_key key) ())

let assert_bucket_tags command_index command conn expected =
  let result =
    expect_ok command_index command "get bucket tags"
      (Simulator.Bucket.Tagging.get conn ~bucket ())
  in
  check_tags command_index command "bucket tags" expected result.tags

let assert_put_bucket_tags command_index command conn tags =
  ignore
    (expect_ok command_index command "put bucket tags"
       (Simulator.Bucket.Tagging.put conn ~bucket ~tags:(tags_to_set tags) ())
      : Awskit.Response.t)

let assert_delete_bucket_tags command_index command conn =
  ignore
    (expect_ok command_index command "delete bucket tags"
       (Simulator.Bucket.Tagging.delete conn ~bucket ())
      : Awskit.Response.t)

let assert_get_versioning command_index command conn expected =
  let status_to_string = Option.map Bucket.Versioning.Status.to_string in
  let result =
    expect_ok command_index command "get versioning"
      (Simulator.Bucket.Versioning.get conn ~bucket ())
  in
  check_equal command_index command
    Alcotest.(option string)
    "versioning status"
    (status_to_string expected)
    (status_to_string result.status)

let check_store command_index command conn model =
  let store = Simulator.store conn in
  check_equal command_index command
    Alcotest.(list (pair string string))
    "objects_as_strings"
    (Model.objects_as_strings model)
    (Simulator.objects_as_strings store ~bucket);
  check_equal command_index command
    Alcotest.(list string)
    "keys" (Model.keys model)
    (Simulator.keys store ~bucket);
  assert_bucket_tags command_index command conn model.bucket_tags;
  assert_get_versioning command_index command conn model.versioning

let assert_put_versioning command_index command conn status =
  ignore
    (expect_ok command_index command "put versioning"
       (Simulator.Bucket.Versioning.put conn ~bucket ~status ())
      : Awskit.Response.t)

let assert_delete command_index command conn key ~keeps_history =
  let result =
    expect_ok command_index command "delete"
      (Simulator.Object.delete conn ~bucket ~key:(key_to_object_key key) ())
  in
  check_version_id_presence command_index command "delete version id"
    keeps_history result.version_id;
  check_equal command_index command Alcotest.bool "delete marker" keeps_history
    (Option.value ~default:false result.delete_marker)

let assert_list_keys command_index command conn model =
  let keys =
    expect_ok command_index command "list keys"
      (Simulator.Object.List.keys conn ~bucket ~max_pages:8 ())
    |> List.map Object_key.to_string
  in
  check_equal command_index command
    Alcotest.(list string)
    "list keys" (Model.keys model) keys

let assert_list_prefix command_index command conn prefix model =
  let options =
    Object.List.options_exn ~prefix:(Object_key.Prefix.of_string_exn prefix) ()
  in
  let keys =
    expect_ok command_index command "list prefix"
      (Simulator.Object.List.keys conn ~bucket ~options ~max_pages:8 ())
    |> List.map Object_key.to_string
  in
  check_equal command_index command
    Alcotest.(list string)
    "list prefix keys"
    (Model.keys_with_prefix prefix model)
    keys

let assert_copy command_index command conn ~source_key ~destination_key
    ~destination_has_version_id expected =
  match expected with
  | Some source ->
      let result =
        expect_ok command_index command "copy"
          (Simulator.Object.copy conn ~source_bucket:bucket
             ~source_key:(key_to_object_key source_key)
             ~destination_bucket:bucket
             ~destination_key:(key_to_object_key destination_key)
             ())
      in
      check_version_id_presence command_index command "copy source version id"
        source.has_version_id result.copy_source_version_id;
      check_version_id_presence command_index command
        "copy destination version id" destination_has_version_id
        result.version_id
  | None ->
      expect_no_such_key command_index command "copy"
        (Simulator.Object.copy conn ~source_bucket:bucket
           ~source_key:(key_to_object_key source_key)
           ~destination_bucket:bucket
           ~destination_key:(key_to_object_key destination_key)
           ())

let apply_simulator_command command_index conn model command =
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
            (Simulator.Object.put_string conn ~bucket
               ~key:(key_to_object_key key) ?options ~contents:body ())
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
        assert_delete command_index command conn key
          ~keeps_history:(Model.versioning_keeps_history model);
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
  check_store command_index command conn next_model;
  next_model

module Target : S3_workload.TARGET = struct
  let name = "awskit-s3-sim"
  let bucket = bucket

  type connection = Simulator.t

  let with_connection f =
    let clock = Simulator.Clock.create ~now:test_time () in
    let config = Simulator.config_exn ~max_list_keys:1 () in
    let store = Simulator.create_store ~config ~clock () in
    let conn = Simulator.connect store ~credentials in
    match Simulator.Bucket.create conn ~bucket () with
    | Ok (_ : Bucket.Create.result) -> f conn
    | Error error ->
        Alcotest.failf "simulator setup failed: %s"
          (Awskit.Error.to_string_hum error)

  let check_store = check_store
  let run_command = apply_simulator_command
end

module Workload = S3_workload.Make (Target)

let suite = Workload.suite
