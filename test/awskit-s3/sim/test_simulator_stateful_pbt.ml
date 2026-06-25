open Awskit_s3
open Awskit_s3_test
open Support
module String_map = Map.Make (String)

type tag_model = (string * string) list
type object_model = { body : string; tags : tag_model; has_version_id : bool }

type model = {
  objects : object_model String_map.t;
  bucket_tags : tag_model;
  versioning : Bucket.Versioning.Status.t option;
}

type command =
  | Put_string of string * string * tag_model
  | Get_string of string
  | Find_string of string
  | Head_object of string
  | Exists_object of string
  | Delete_object of string
  | List_keys
  | List_prefix of string
  | Copy_object of string * string
  | Put_object_tags of string * tag_model
  | Get_object_tags of string
  | Delete_object_tags of string
  | Put_bucket_tags of tag_model
  | Get_bucket_tags
  | Delete_bucket_tags
  | Put_versioning of Bucket.Versioning.Status.t
  | Get_versioning

let to_alcotest = Awskit_test.Qcheck.to_alcotest
let bucket = bucket_name "stateful-pbt-bucket"

let key_domain =
  [ "a.txt"; "b.txt"; "logs/a.txt"; "logs/b.txt"; "photos/2026.jpg" ]

let prefix_domain = [ "logs/"; "photos/"; "missing/"; "a" ]

let tag_sets_domain =
  [
    [];
    [ ("env", "dev") ];
    [ ("env", "prod") ];
    [ ("owner", "sdk") ];
    [ ("team", "storage") ];
    [ ("env", "dev"); ("owner", "sdk") ];
    [ ("team", "storage"); ("mode", "pbt") ];
    [ ("path/key", "x@y") ];
  ]

let versioning_status_domain =
  [ Bucket.Versioning.Status.Enabled; Bucket.Versioning.Status.Suspended ]

let key_to_object_key key = Object_key.of_string_exn key

let model_empty =
  { objects = String_map.empty; bucket_tags = []; versioning = None }

let model_versioning_keeps_history model =
  match model.versioning with
  | Some Bucket.Versioning.Status.Enabled | Some Suspended -> true
  | Some (Unknown _) | None -> false

let model_find key model = String_map.find_opt key model.objects

let model_put key body tags model =
  {
    model with
    objects =
      String_map.add key
        { body; tags; has_version_id = model_versioning_keeps_history model }
        model.objects;
  }

let model_delete key model =
  { model with objects = String_map.remove key model.objects }

let model_copy ~source_key ~destination_key model =
  match model_find source_key model with
  | None -> model
  | Some source ->
      {
        model with
        objects =
          String_map.add destination_key
            {
              source with
              has_version_id = model_versioning_keeps_history model;
            }
            model.objects;
      }

let model_put_object_tags key tags model =
  match model_find key model with
  | None -> model
  | Some object_ ->
      {
        model with
        objects = String_map.add key { object_ with tags } model.objects;
      }

let model_delete_object_tags key model = model_put_object_tags key [] model
let model_put_bucket_tags tags model = { model with bucket_tags = tags }
let model_delete_bucket_tags model = { model with bucket_tags = [] }
let model_put_versioning status model = { model with versioning = Some status }

let model_to_list model =
  String_map.bindings model.objects
  |> List.map (fun (key, object_) -> (key, object_.body))

let model_keys model = model |> model_to_list |> List.map fst

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal prefix (String.sub value 0 prefix_len)

let model_keys_with_prefix prefix model =
  model_keys model |> List.filter (is_prefix ~prefix)

let tags_to_set tags =
  tags |> List.map (fun (key, value) -> tag key value) |> tag_set

let tags_of_set tags =
  Tag.Set.to_list tags |> List.map (fun tag -> (Tag.key tag, Tag.value tag))

let pp_string_literal value = Printf.sprintf "%S" value

let pp_tags tags =
  tags
  |> List.map (fun (key, value) -> Printf.sprintf "(%S, %S)" key value)
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let pp_versioning_status status = Bucket.Versioning.Status.to_string status

let print_command = function
  | Put_string (key, body, tags) ->
      Printf.sprintf "Put_string(%S, %s, %s)" key (pp_string_literal body)
        (pp_tags tags)
  | Get_string key -> Printf.sprintf "Get_string(%S)" key
  | Find_string key -> Printf.sprintf "Find_string(%S)" key
  | Head_object key -> Printf.sprintf "Head_object(%S)" key
  | Exists_object key -> Printf.sprintf "Exists_object(%S)" key
  | Delete_object key -> Printf.sprintf "Delete_object(%S)" key
  | List_keys -> "List_keys"
  | List_prefix prefix -> Printf.sprintf "List_prefix(%S)" prefix
  | Copy_object (source_key, destination_key) ->
      Printf.sprintf "Copy_object(%S, %S)" source_key destination_key
  | Put_object_tags (key, tags) ->
      Printf.sprintf "Put_object_tags(%S, %s)" key (pp_tags tags)
  | Get_object_tags key -> Printf.sprintf "Get_object_tags(%S)" key
  | Delete_object_tags key -> Printf.sprintf "Delete_object_tags(%S)" key
  | Put_bucket_tags tags -> Printf.sprintf "Put_bucket_tags(%s)" (pp_tags tags)
  | Get_bucket_tags -> "Get_bucket_tags"
  | Delete_bucket_tags -> "Delete_bucket_tags"
  | Put_versioning status ->
      Printf.sprintf "Put_versioning(%s)" (pp_versioning_status status)
  | Get_versioning -> "Get_versioning"

let print_commands commands =
  commands
  |> List.mapi (fun index command ->
      Printf.sprintf "%02d. %s" (index + 1) (print_command command))
  |> String.concat "\n"

let gen_key = QCheck.Gen.oneof_list key_domain
let gen_prefix = QCheck.Gen.oneof_list prefix_domain
let gen_tags = QCheck.Gen.oneof_list tag_sets_domain
let gen_versioning_status = QCheck.Gen.oneof_list versioning_status_domain

let gen_body =
  QCheck.Gen.(
    string_size
      ~gen:
        (oneof_weighted
           [ (8, char_range 'a' 'z'); (1, return ' '); (1, numeral) ])
      (int_range 0 12))

let gen_command =
  let open QCheck.Gen in
  oneof_weighted
    [
      ( 4,
        map3
          (fun key body tags -> Put_string (key, body, tags))
          gen_key gen_body gen_tags );
      (2, map (fun key -> Get_string key) gen_key);
      (2, map (fun key -> Find_string key) gen_key);
      (2, map (fun key -> Head_object key) gen_key);
      (2, map (fun key -> Exists_object key) gen_key);
      (2, map (fun key -> Delete_object key) gen_key);
      (1, return List_keys);
      (1, map (fun prefix -> List_prefix prefix) gen_prefix);
      (2, map2 (fun source dest -> Copy_object (source, dest)) gen_key gen_key);
      (2, map2 (fun key tags -> Put_object_tags (key, tags)) gen_key gen_tags);
      (2, map (fun key -> Get_object_tags key) gen_key);
      (2, map (fun key -> Delete_object_tags key) gen_key);
      (1, map (fun tags -> Put_bucket_tags tags) gen_tags);
      (1, return Get_bucket_tags);
      (1, return Delete_bucket_tags);
      (1, map (fun status -> Put_versioning status) gen_versioning_status);
      (1, return Get_versioning);
    ]

let gen_commands = QCheck.Gen.list_size (QCheck.Gen.int_range 1 40) gen_command

let shrink_key key =
  match key_domain with
  | first :: _ when not (String.equal key first) -> QCheck.Iter.return first
  | _ -> QCheck.Iter.empty

let shrink_body body =
  QCheck.Shrink.string ~shrink:QCheck.Shrink.char_printable body

let shrink_prefix prefix =
  match prefix_domain with
  | first :: _ when not (String.equal prefix first) -> QCheck.Iter.return first
  | _ -> QCheck.Iter.empty

let shrink_tags = function
  | [] -> QCheck.Iter.empty
  | _ :: _ -> QCheck.Iter.return []

let shrink_versioning_status = function
  | Bucket.Versioning.Status.Suspended ->
      QCheck.Iter.return Bucket.Versioning.Status.Enabled
  | Enabled | Unknown _ -> QCheck.Iter.empty

let shrink_command = function
  | Put_string (key, body, tags) ->
      QCheck.Iter.of_list
        [
          QCheck.Iter.map
            (fun key -> Put_string (key, body, tags))
            (shrink_key key);
          QCheck.Iter.map
            (fun body -> Put_string (key, body, tags))
            (shrink_body body);
          QCheck.Iter.map
            (fun tags -> Put_string (key, body, tags))
            (shrink_tags tags);
        ]
      |> QCheck.Iter.flatten
  | Get_string key ->
      QCheck.Iter.map (fun key -> Get_string key) (shrink_key key)
  | Find_string key ->
      QCheck.Iter.map (fun key -> Find_string key) (shrink_key key)
  | Head_object key ->
      QCheck.Iter.map (fun key -> Head_object key) (shrink_key key)
  | Exists_object key ->
      QCheck.Iter.map (fun key -> Exists_object key) (shrink_key key)
  | Delete_object key ->
      QCheck.Iter.map (fun key -> Delete_object key) (shrink_key key)
  | List_keys -> QCheck.Iter.empty
  | List_prefix prefix ->
      QCheck.Iter.map (fun prefix -> List_prefix prefix) (shrink_prefix prefix)
  | Copy_object (source_key, destination_key) ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun source_key -> Copy_object (source_key, destination_key))
           (shrink_key source_key))
        (QCheck.Iter.map
           (fun destination_key -> Copy_object (source_key, destination_key))
           (shrink_key destination_key))
  | Put_object_tags (key, tags) ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun key -> Put_object_tags (key, tags))
           (shrink_key key))
        (QCheck.Iter.map
           (fun tags -> Put_object_tags (key, tags))
           (shrink_tags tags))
  | Get_object_tags key ->
      QCheck.Iter.map (fun key -> Get_object_tags key) (shrink_key key)
  | Delete_object_tags key ->
      QCheck.Iter.map (fun key -> Delete_object_tags key) (shrink_key key)
  | Put_bucket_tags tags ->
      QCheck.Iter.map (fun tags -> Put_bucket_tags tags) (shrink_tags tags)
  | Get_bucket_tags | Delete_bucket_tags -> QCheck.Iter.empty
  | Put_versioning status ->
      QCheck.Iter.map
        (fun status -> Put_versioning status)
        (shrink_versioning_status status)
  | Get_versioning -> QCheck.Iter.empty

let shrink_commands = QCheck.Shrink.list ~shrink:shrink_command

let commands_arbitrary =
  QCheck.make ~print:print_commands ~shrink:shrink_commands gen_commands

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
    (print_command command) message

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
  try Alcotest.check testable label expected actual
  with Alcotest.Test_error ->
    fail command_index command (Printf.sprintf "%s mismatch" label)

let check_version_id_presence command_index command label expected version_id =
  check_equal command_index command Alcotest.bool label expected
    (Option.is_some version_id)

let check_tags command_index command label expected actual =
  check_equal command_index command
    Alcotest.(list (pair string string))
    label expected (tags_of_set actual)

let check_store command_index command store model =
  check_equal command_index command
    Alcotest.(list (pair string string))
    "objects_as_strings" (model_to_list model)
    (Simulator.objects_as_strings store ~bucket);
  check_equal command_index command
    Alcotest.(list string)
    "keys" (model_keys model)
    (Simulator.keys store ~bucket)

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
    "list keys" (model_keys model) keys

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
    (model_keys_with_prefix prefix model)
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

let apply_command command_index conn model command =
  let store = Simulator.store conn in
  let next_model =
    match command with
    | Put_string (key, body, tags) ->
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
          (model_versioning_keeps_history model)
          result.version_id;
        let next_model = model_put key body tags model in
        assert_object_tags command_index command conn key
          (model_find key next_model);
        next_model
    | Get_string key ->
        assert_get command_index command conn key (model_find key model);
        model
    | Find_string key ->
        assert_find command_index command conn key (model_find key model);
        model
    | Head_object key ->
        assert_head command_index command conn key (model_find key model);
        model
    | Exists_object key ->
        assert_exists command_index command conn key (model_find key model);
        model
    | Delete_object key ->
        assert_delete command_index command conn key
          ~keeps_history:(model_versioning_keeps_history model);
        model_delete key model
    | List_keys ->
        assert_list_keys command_index command conn model;
        model
    | List_prefix prefix ->
        assert_list_prefix command_index command conn prefix model;
        model
    | Copy_object (source_key, destination_key) ->
        let source = model_find source_key model in
        assert_copy command_index command conn ~source_key ~destination_key
          ~destination_has_version_id:(model_versioning_keeps_history model)
          source;
        let next_model = model_copy ~source_key ~destination_key model in
        (match source with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn destination_key
              (model_find destination_key next_model));
        next_model
    | Put_object_tags (key, tags) ->
        let expected = model_find key model in
        assert_put_object_tags command_index command conn key tags expected;
        let next_model = model_put_object_tags key tags model in
        (match expected with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn key
              (model_find key next_model));
        next_model
    | Get_object_tags key ->
        assert_object_tags command_index command conn key (model_find key model);
        model
    | Delete_object_tags key ->
        let expected = model_find key model in
        assert_delete_object_tags command_index command conn key expected;
        let next_model = model_delete_object_tags key model in
        (match expected with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn key
              (model_find key next_model));
        next_model
    | Put_bucket_tags tags ->
        assert_put_bucket_tags command_index command conn tags;
        let next_model = model_put_bucket_tags tags model in
        assert_bucket_tags command_index command conn next_model.bucket_tags;
        next_model
    | Get_bucket_tags ->
        assert_bucket_tags command_index command conn model.bucket_tags;
        model
    | Delete_bucket_tags ->
        assert_delete_bucket_tags command_index command conn;
        let next_model = model_delete_bucket_tags model in
        assert_bucket_tags command_index command conn next_model.bucket_tags;
        next_model
    | Put_versioning status ->
        assert_put_versioning command_index command conn status;
        let next_model = model_put_versioning status model in
        assert_get_versioning command_index command conn next_model.versioning;
        next_model
    | Get_versioning ->
        assert_get_versioning command_index command conn model.versioning;
        model
  in
  check_store command_index command store next_model;
  next_model

let fresh_simulator () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let config = Simulator.config_exn ~max_list_keys:1 () in
  let store = Simulator.create_store ~config ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket () |> ok_or_fail "stateful bucket"
      : Bucket.Create.result);
  conn

let prop_object_lifecycle =
  QCheck.Test.make ~count:150 ~name:"object lifecycle matches pure model"
    commands_arbitrary (fun commands ->
      let conn = fresh_simulator () in
      check_store 0 List_keys (Simulator.store conn) model_empty;
      assert_bucket_tags 0 Get_bucket_tags conn model_empty.bucket_tags;
      assert_get_versioning 0 Get_versioning conn model_empty.versioning;
      ignore
        (List.fold_left
           (fun model (index, command) ->
             apply_command index conn model command)
           model_empty
           (List.mapi (fun index command -> (index + 1, command)) commands)
          : model);
      true)

let suite =
  [
    ( "pbt:awskit-s3-sim:simulator-stateful",
      [ to_alcotest prop_object_lifecycle ] );
  ]
