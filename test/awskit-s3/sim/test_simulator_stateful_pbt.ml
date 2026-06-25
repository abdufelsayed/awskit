open Awskit_s3
open Awskit_s3_test
open Support
module String_map = Map.Make (String)

type model = string String_map.t

type command =
  | Put_string of string * string
  | Get_string of string
  | Find_string of string
  | Head_object of string
  | Exists_object of string
  | Delete_object of string
  | List_keys
  | Copy_object of string * string

let to_alcotest = Awskit_test.Qcheck.to_alcotest
let bucket = bucket_name "stateful-pbt-bucket"

let key_domain =
  [ "a.txt"; "b.txt"; "logs/a.txt"; "logs/b.txt"; "photos/2026.jpg" ]

let key_to_object_key key = Object_key.of_string_exn key
let model_empty = String_map.empty
let model_find key model = String_map.find_opt key model
let model_put key body model = String_map.add key body model
let model_delete key model = String_map.remove key model

let model_copy ~source_key ~destination_key model =
  match model_find source_key model with
  | None -> model
  | Some body -> model_put destination_key body model

let model_to_list model = String_map.bindings model
let model_keys model = model |> String_map.bindings |> List.map fst
let pp_string_literal value = Printf.sprintf "%S" value

let print_command = function
  | Put_string (key, body) ->
      Printf.sprintf "Put_string(%S, %s)" key (pp_string_literal body)
  | Get_string key -> Printf.sprintf "Get_string(%S)" key
  | Find_string key -> Printf.sprintf "Find_string(%S)" key
  | Head_object key -> Printf.sprintf "Head_object(%S)" key
  | Exists_object key -> Printf.sprintf "Exists_object(%S)" key
  | Delete_object key -> Printf.sprintf "Delete_object(%S)" key
  | List_keys -> "List_keys"
  | Copy_object (source_key, destination_key) ->
      Printf.sprintf "Copy_object(%S, %S)" source_key destination_key

let print_commands commands =
  commands
  |> List.mapi (fun index command ->
      Printf.sprintf "%02d. %s" (index + 1) (print_command command))
  |> String.concat "\n"

let gen_key = QCheck.Gen.oneof_list key_domain

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
      (3, map2 (fun key body -> Put_string (key, body)) gen_key gen_body);
      (2, map (fun key -> Get_string key) gen_key);
      (2, map (fun key -> Find_string key) gen_key);
      (2, map (fun key -> Head_object key) gen_key);
      (2, map (fun key -> Exists_object key) gen_key);
      (2, map (fun key -> Delete_object key) gen_key);
      (1, return List_keys);
      (2, map2 (fun source dest -> Copy_object (source, dest)) gen_key gen_key);
    ]

let gen_commands = QCheck.Gen.list_size (QCheck.Gen.int_range 1 40) gen_command

let shrink_key key =
  match key_domain with
  | first :: _ when not (String.equal key first) -> QCheck.Iter.return first
  | _ -> QCheck.Iter.empty

let shrink_body body =
  QCheck.Shrink.string ~shrink:QCheck.Shrink.char_printable body

let shrink_command = function
  | Put_string (key, body) ->
      QCheck.Iter.append
        (QCheck.Iter.map (fun key -> Put_string (key, body)) (shrink_key key))
        (QCheck.Iter.map
           (fun body -> Put_string (key, body))
           (shrink_body body))
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
  | Copy_object (source_key, destination_key) ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun source_key -> Copy_object (source_key, destination_key))
           (shrink_key source_key))
        (QCheck.Iter.map
           (fun destination_key -> Copy_object (source_key, destination_key))
           (shrink_key destination_key))

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
  | Some body ->
      let result =
        expect_ok command_index command "get_string"
          (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
             ~max_bytes:64L ())
      in
      check_equal command_index command Alcotest.string "get body" body
        result.value
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
      | Some body ->
          check_equal command_index command Alcotest.string "find body" body
            result.value
      | None -> fail command_index command "find_string expected Ok None")
  | Ok None -> (
      match expected with
      | None -> ()
      | Some _ -> fail command_index command "find_string expected object")
  | Error error -> fail_error command_index command "find_string" error

let assert_head command_index command conn key expected =
  match expected with
  | Some body ->
      let result =
        expect_ok command_index command "head"
          (Simulator.Object.head conn ~bucket ~key:(key_to_object_key key) ())
      in
      check_equal command_index command
        Alcotest.(option int64)
        "head content length"
        (Some (Int64.of_int (String.length body)))
        result.content_length
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

let assert_delete command_index command conn key =
  ignore
    (expect_ok command_index command "delete"
       (Simulator.Object.delete conn ~bucket ~key:(key_to_object_key key) ())
      : Object.Delete.result)

let assert_list_keys command_index command conn model =
  let keys =
    expect_ok command_index command "list keys"
      (Simulator.Object.List.keys conn ~bucket ~max_pages:8 ())
    |> List.map Object_key.to_string
  in
  check_equal command_index command
    Alcotest.(list string)
    "list keys" (model_keys model) keys

let assert_copy command_index command conn ~source_key ~destination_key expected
    =
  match expected with
  | Some _ ->
      ignore
        (expect_ok command_index command "copy"
           (Simulator.Object.copy conn ~source_bucket:bucket
              ~source_key:(key_to_object_key source_key)
              ~destination_bucket:bucket
              ~destination_key:(key_to_object_key destination_key)
              ())
          : Object.Copy.result)
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
    | Put_string (key, body) ->
        ignore
          (expect_ok command_index command "put_string"
             (Simulator.Object.put_string conn ~bucket
                ~key:(key_to_object_key key) ~contents:body ())
            : Object.Put.result);
        model_put key body model
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
        assert_delete command_index command conn key;
        model_delete key model
    | List_keys ->
        assert_list_keys command_index command conn model;
        model
    | Copy_object (source_key, destination_key) ->
        let source = model_find source_key model in
        assert_copy command_index command conn ~source_key ~destination_key
          source;
        model_copy ~source_key ~destination_key model
  in
  check_store command_index command store next_model;
  next_model

let fresh_simulator () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
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
