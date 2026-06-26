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

let metadata_to_store metadata = Metadata.of_list_exn metadata

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

let target_profile = "strict"

let digest_summary body =
  Printf.sprintf "len=%d digest=%s" (String.length body)
    (Digest.to_hex (Digest.string body))

let list_to_string pp values =
  values |> List.map pp |> String.concat "; " |> Printf.sprintf "[%s]"

let option_to_string pp = function None -> "none" | Some value -> pp value
let versioning_to_string = option_to_string Bucket.Versioning.Status.to_string
let tags_to_string = Command.tags_to_string
let metadata_to_string = Command.metadata_to_string

let model_listed_version_to_string (version : Model.listed_version) =
  Printf.sprintf "%s:%s:version_id=%b:latest=%b:size=%s" version.key
    (match version.kind with `Object -> "object" | `Delete_marker -> "marker")
    version.has_version_id version.is_latest
    (Option.fold ~none:"-" ~some:Int64.to_string version.size)

let model_summary (model : Model.t) =
  let object_summary =
    Model.objects_as_strings model
    |> list_to_string (fun (key, body) ->
        Printf.sprintf "%S:%s" key (digest_summary body))
  in
  let version_summary =
    Model.listed_versions model |> list_to_string model_listed_version_to_string
  in
  Printf.sprintf "objects=%s bucket_tags=%s versioning=%s versions=%s"
    object_summary
    (tags_to_string model.bucket_tags)
    (versioning_to_string model.versioning)
    version_summary

let content_range_summary_to_string
    (summary : Model.content_range_summary option) =
  match summary with
  | None -> "none"
  | Some summary ->
      let complete_length =
        match summary.complete_length with
        | None -> "*"
        | Some length -> Int64.to_string length
      in
      Printf.sprintf "bytes %Ld-%Ld/%s" summary.start summary.finish
        complete_length

let object_read_summary_to_string (summary : Model.object_read_summary) =
  Printf.sprintf
    "{body=%s; metadata=%s; content_length=%s; content_range=%s; \
     has_version_id=%b}"
    (digest_summary summary.read_body)
    (metadata_to_string summary.read_metadata)
    (option_to_string Int64.to_string summary.read_content_length)
    (content_range_summary_to_string summary.read_content_range)
    summary.read_has_version_id

let object_metadata_summary_to_string (summary : Model.object_metadata_summary)
    =
  Printf.sprintf "{content_length=%s; metadata=%s; has_version_id=%b}"
    (option_to_string Int64.to_string summary.metadata_content_length)
    (metadata_to_string summary.metadata_entries)
    summary.metadata_has_version_id

let object_write_summary_to_string (summary : Model.object_write_summary) =
  Printf.sprintf "{has_version_id=%b}" summary.write_has_version_id

let object_delete_summary_to_string (summary : Model.object_delete_summary) =
  Printf.sprintf "{delete_marker=%b; has_version_id=%b}" summary.delete_marker
    summary.delete_has_version_id

let object_copy_summary_to_string (summary : Model.object_copy_summary) =
  Printf.sprintf "{source_has_version_id=%b; destination_has_version_id=%b}"
    summary.copy_source_has_version_id summary.copy_destination_has_version_id

let operation_result_to_string = function
  | Model.Response_ok -> "response-ok"
  | Put_ok summary -> "put-ok " ^ object_write_summary_to_string summary
  | Get_ok summary -> "get-ok " ^ object_read_summary_to_string summary
  | Find_ok summary ->
      "find-ok " ^ option_to_string object_read_summary_to_string summary
  | Head_ok summary -> "head-ok " ^ object_metadata_summary_to_string summary
  | Exists_ok exists -> Printf.sprintf "exists-ok %b" exists
  | Delete_ok summary -> "delete-ok " ^ object_delete_summary_to_string summary
  | List_keys_ok keys ->
      "list-keys-ok " ^ list_to_string (Printf.sprintf "%S") keys
  | List_versions_ok versions ->
      "list-versions-ok "
      ^ list_to_string model_listed_version_to_string versions
  | Copy_ok summary -> "copy-ok " ^ object_copy_summary_to_string summary
  | Object_tags_ok tags -> "object-tags-ok " ^ tags_to_string tags
  | Bucket_tags_ok tags -> "bucket-tags-ok " ^ tags_to_string tags
  | Versioning_ok status -> "versioning-ok " ^ versioning_to_string status
  | Not_found -> "not-found"
  | Invalid_range -> "invalid-range"

type command_context = {
  command_index : int;
  command : Command.t;
  transcript : string option;
  expected_result : string;
  model_before : string;
  model_after : string;
}

let current_command_context = ref None
let current_command_transcript = ref None

let with_command_transcript transcript f =
  let previous = !current_command_transcript in
  current_command_transcript := Some transcript;
  Fun.protect ~finally:(fun () -> current_command_transcript := previous) f

let with_command_context context f =
  let previous = !current_command_context in
  current_command_context := Some context;
  Fun.protect ~finally:(fun () -> current_command_context := previous) f

let command_context ~command_index ~model_before command =
  let model_after = Model.apply command model_before in
  {
    command_index;
    command;
    transcript = !current_command_transcript;
    expected_result =
      operation_result_to_string (Model.expected_result command model_before);
    model_before = model_summary model_before;
    model_after = model_summary model_after;
  }

let failure_report ?expected ?observed command_index command message =
  let context_lines =
    match !current_command_context with
    | Some context when Int.equal context.command_index command_index ->
        [
          Printf.sprintf "target_profile: %s" target_profile;
          Printf.sprintf "command_index: %d" context.command_index;
          Printf.sprintf "command: %s" (Command.to_string context.command);
          (match context.transcript with
          | None -> "full_command_transcript: <see QCheck counterexample>"
          | Some transcript -> "full_command_transcript:\n" ^ transcript);
          Printf.sprintf "expected_result: %s"
            (Option.value ~default:context.expected_result expected);
          Printf.sprintf "observed_result: %s"
            (Option.value ~default:message observed);
          Printf.sprintf "model_before: %s" context.model_before;
          Printf.sprintf "model_after: %s" context.model_after;
        ]
    | None | Some _ ->
        [
          Printf.sprintf "target_profile: %s" target_profile;
          Printf.sprintf "command_index: %d" command_index;
          Printf.sprintf "command: %s" (Command.to_string command);
          "full_command_transcript: <see QCheck counterexample>";
          "expected_result: <not available outside command context>";
          Printf.sprintf "observed_result: %s"
            (Option.value ~default:message observed);
          "model_before: <not available outside command context>";
          "model_after: <not available outside command context>";
        ]
  in
  String.concat "\n" (message :: context_lines)

let fail ?expected ?observed command_index command message =
  QCheck.Test.fail_report
    (failure_report ?expected ?observed command_index command message)

let fail_error command_index command label error =
  fail command_index command
    ~observed:(Printf.sprintf "Error %s" (error_shape error))
    (Printf.sprintf "%s unexpected error shape %s" label (error_shape error))

let expect_no_such_key command_index command label = function
  | Error error when Error.is_no_such_key error -> ()
  | Error error -> fail_error command_index command label error
  | Ok _ ->
      fail command_index command
        (label ^ " expected NoSuchKey")
        ~expected:"Error service:NoSuchKey" ~observed:"Ok"

let is_invalid_range_error error =
  match
    (Awskit.Error.service_status error, Awskit.Error.service_code error)
  with
  | Some 416, Some "InvalidRange" | _, Some "InvalidRange" -> true
  | _ -> false

let expect_invalid_range command_index command label = function
  | Error error when is_invalid_range_error error -> ()
  | Error error -> fail_error command_index command label error
  | Ok _ ->
      fail command_index command
        (label ^ " expected InvalidRange")
        ~expected:"Error service:InvalidRange" ~observed:"Ok"

let expect_ok command_index command label = function
  | Ok value -> value
  | Error error -> fail_error command_index command label error

let testable_to_string testable value =
  Format.asprintf "%a" (Alcotest.pp testable) value

let check_equal command_index command testable label expected actual =
  if Alcotest.equal testable expected actual then ()
  else
    fail command_index command label
      ~expected:(testable_to_string testable expected)
      ~observed:(testable_to_string testable actual)

let check_version_id_presence command_index command label expected version_id =
  check_equal command_index command Alcotest.bool label expected
    (Option.is_some version_id)

let check_tags command_index command label expected actual =
  check_equal command_index command
    Alcotest.(list (pair string string))
    label expected (tags_of_set actual)

let check_metadata command_index command label expected actual =
  check_equal command_index command
    Alcotest.(list (pair string string))
    label expected (Metadata.to_list actual)

let max_bytes_for_expected = function
  | Some object_ -> Int64.of_int (String.length object_.body)
  | None -> 64L

let model_content_range_to_string = function
  | None -> None
  | Some (summary : Model.content_range_summary) ->
      let complete_length =
        match summary.complete_length with
        | None -> "*"
        | Some length -> Int64.to_string length
      in
      Some
        (Printf.sprintf "bytes %Ld-%Ld/%s" summary.start summary.finish
           complete_length)

let actual_content_range_to_string = Option.map Range.Content_range.to_header

let assert_get command_index command conn key expected =
  let max_bytes = max_bytes_for_expected expected in
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get_string"
          (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
             ~max_bytes ())
      in
      check_equal command_index command Alcotest.string "get body" object_.body
        result.value;
      check_metadata command_index command "get metadata" object_.metadata
        result.metadata;
      check_version_id_presence command_index command "get version id"
        object_.has_version_id result.version_id
  | None ->
      expect_no_such_key command_index command "get_string"
        (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
           ~max_bytes ())

let assert_get_range command_index command conn key range model =
  let max_bytes = max_bytes_for_expected (Model.find key model) in
  let options = Object.Get.options_exn ~range () in
  match Model.expected_result command model with
  | Get_ok summary ->
      let result =
        expect_ok command_index command "get range"
          (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
             ~options ~max_bytes ())
      in
      check_equal command_index command Alcotest.string "get range body"
        summary.read_body result.value;
      check_equal command_index command
        Alcotest.(option int64)
        "get range content length" summary.read_content_length
        result.content_length;
      check_equal command_index command
        Alcotest.(option string)
        "get range content range"
        (model_content_range_to_string summary.read_content_range)
        (actual_content_range_to_string result.content_range);
      check_metadata command_index command "get range metadata"
        summary.read_metadata result.metadata;
      check_version_id_presence command_index command "get range version id"
        summary.read_has_version_id result.version_id
  | Not_found ->
      expect_no_such_key command_index command "get range"
        (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
           ~options ~max_bytes ())
  | Invalid_range ->
      expect_invalid_range command_index command "get range"
        (Simulator.Object.get_string conn ~bucket ~key:(key_to_object_key key)
           ~options ~max_bytes ())
  | result ->
      fail command_index command
        (Printf.sprintf "unexpected model result %s for range get"
           (Model.operation_result_kind result))

let assert_find command_index command conn key expected =
  let max_bytes = max_bytes_for_expected expected in
  match
    Simulator.Object.find_string conn ~bucket ~key:(key_to_object_key key)
      ~max_bytes ()
  with
  | Ok (Some result) -> (
      match expected with
      | Some object_ ->
          check_equal command_index command Alcotest.string "find body"
            object_.body result.value;
          check_metadata command_index command "find metadata" object_.metadata
            result.metadata;
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
      check_metadata command_index command "head metadata" object_.metadata
        result.metadata;
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

let listed_version_to_string (version : Model.listed_version) =
  Printf.sprintf "%s:%s:version_id=%b:latest=%b:size=%s" version.key
    (match version.kind with `Object -> "object" | `Delete_marker -> "marker")
    version.has_version_id version.is_latest
    (Option.fold ~none:"-" ~some:Int64.to_string version.size)

let actual_object_version_to_model (version : Object.Versions.object_version) :
    Model.listed_version =
  {
    key = Object_key.to_string version.key;
    kind = `Object;
    has_version_id = Option.is_some version.version_id;
    is_latest = Option.value ~default:false version.is_latest;
    size = version.size;
  }

let actual_delete_marker_to_model (marker : Object.Versions.delete_marker) :
    Model.listed_version =
  {
    key = Object_key.to_string marker.key;
    kind = `Delete_marker;
    has_version_id = Option.is_some marker.version_id;
    is_latest = Option.value ~default:false marker.is_latest;
    size = None;
  }

let collection_page_bound item_count = Int.max 1 (item_count + 1)

let assert_version_listing command_index command conn model =
  let max_pages =
    collection_page_bound (List.length (Model.listed_versions model))
  in
  let pages =
    expect_ok command_index command "list object versions"
      (Simulator.Object.Versions.pages conn ~bucket ~max_pages ())
  in
  let actual =
    List.concat_map
      (fun (page : Object.Versions.page) ->
        List.map actual_object_version_to_model page.versions
        @ List.map actual_delete_marker_to_model page.delete_markers)
      pages
    |> List.map listed_version_to_string
    |> List.sort String.compare
  in
  let expected =
    Model.listed_versions model
    |> List.map listed_version_to_string
    |> List.sort String.compare
  in
  check_equal command_index command
    Alcotest.(list string)
    "list object versions" expected actual

let check_store_state command_index command conn model =
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
  assert_get_versioning command_index command conn model.versioning;
  assert_version_listing command_index command conn model

let store_check_context ~command_index ~model command =
  {
    command_index;
    command;
    transcript = !current_command_transcript;
    expected_result =
      (if command_index = 0 then "initial store matches empty model"
       else "store matches expected model after command");
    model_before = model_summary model;
    model_after = model_summary model;
  }

let check_store command_index command conn model =
  with_command_context (store_check_context ~command_index ~model command)
    (fun () -> check_store_state command_index command conn model)

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
  let expected = Model.keys model in
  let keys =
    expect_ok command_index command "list keys"
      (Simulator.Object.List.keys conn ~bucket
         ~max_pages:(collection_page_bound (List.length expected))
         ())
    |> List.map Object_key.to_string
  in
  check_equal command_index command
    Alcotest.(list string)
    "list keys" expected keys

let assert_list_prefix command_index command conn prefix model =
  let options =
    Object.List.options_exn ~prefix:(Object_key.Prefix.of_string_exn prefix) ()
  in
  let expected = Model.keys_with_prefix prefix model in
  let keys =
    expect_ok command_index command "list prefix"
      (Simulator.Object.List.keys conn ~bucket ~options
         ~max_pages:(collection_page_bound (List.length expected))
         ())
    |> List.map Object_key.to_string
  in
  check_equal command_index command
    Alcotest.(list string)
    "list prefix keys" expected keys

let expected_list_keys_page prefix ~max_keys model =
  match prefix with
  | None -> Model.list_keys_page ~max_keys model
  | Some prefix -> Model.list_keys_page ~prefix ~max_keys model

let expected_list_keys_page_is_truncated prefix ~max_keys model =
  match prefix with
  | None -> Model.list_keys_page_is_truncated ~max_keys model
  | Some prefix -> Model.list_keys_page_is_truncated ~prefix ~max_keys model

let assert_list_keys_page command_index command conn prefix max_keys model =
  let options =
    match prefix with
    | None -> Object.List.options_exn ~max_keys ()
    | Some prefix ->
        Object.List.options_exn
          ~prefix:(Object_key.Prefix.of_string_exn prefix)
          ~max_keys ()
  in
  let page =
    expect_ok command_index command "list keys page"
      (Simulator.Object.list conn ~bucket ~options ())
  in
  let actual =
    List.map
      (fun (object_ : Object.List.object_summary) ->
        Object_key.to_string object_.key)
      page.objects
  in
  let expected = expected_list_keys_page prefix ~max_keys model in
  let expected_is_truncated =
    expected_list_keys_page_is_truncated prefix ~max_keys model
  in
  check_equal command_index command
    Alcotest.(list string)
    "list keys page" expected actual;
  check_equal command_index command Alcotest.bool "list keys page truncated"
    expected_is_truncated page.is_truncated;
  check_equal command_index command Alcotest.bool "list keys page next token"
    expected_is_truncated
    (Option.is_some page.next_continuation_token)

let listed_object_version_summaries versions =
  versions
  |> List.filter (fun (version : Model.listed_version) ->
      version.kind = `Object)
  |> List.map listed_version_to_string

let listed_delete_marker_summaries versions =
  versions
  |> List.filter (fun (version : Model.listed_version) ->
      version.kind = `Delete_marker)
  |> List.map listed_version_to_string

let last = function [] -> None | values -> Some (List.hd (List.rev values))

let assert_list_versions_page command_index command conn max_keys model =
  let options = Object.Versions.options_exn ~max_keys () in
  let page =
    expect_ok command_index command "list object versions page"
      (Simulator.Object.list_versions conn ~bucket ~options ())
  in
  let actual_object_versions =
    List.map actual_object_version_to_model page.versions
    |> List.map listed_version_to_string
  in
  let actual_delete_markers =
    List.map actual_delete_marker_to_model page.delete_markers
    |> List.map listed_version_to_string
  in
  let expected_page = Model.list_versions_page ~max_keys model in
  let expected_is_truncated =
    Model.list_versions_page_is_truncated ~max_keys model
  in
  let expected_next_entry =
    if expected_is_truncated then last expected_page else None
  in
  let expected_next_key_marker =
    Option.map
      (fun (version : Model.listed_version) -> version.key)
      expected_next_entry
  in
  let expected_next_version_id_marker =
    match expected_next_entry with
    | None -> false
    | Some version -> version.has_version_id
  in
  check_equal command_index command
    Alcotest.(list string)
    "list object versions page objects"
    (listed_object_version_summaries expected_page)
    actual_object_versions;
  check_equal command_index command
    Alcotest.(list string)
    "list object versions page delete markers"
    (listed_delete_marker_summaries expected_page)
    actual_delete_markers;
  check_equal command_index command Alcotest.bool
    "list object versions page truncated" expected_is_truncated
    page.is_truncated;
  check_equal command_index command
    Alcotest.(option string)
    "list object versions page next key marker" expected_next_key_marker
    (Option.map Object_key.to_string page.next_key_marker);
  check_equal command_index command Alcotest.bool
    "list object versions page next version marker"
    expected_next_version_id_marker
    (Option.is_some page.next_version_id_marker)

let assert_copy command_index command conn ~source_key ~destination_key ?options
    ~destination_has_version_id (expected : Model.object_ option) =
  match expected with
  | Some source ->
      let result =
        expect_ok command_index command "copy"
          (Simulator.Object.copy conn ~source_bucket:bucket
             ~source_key:(key_to_object_key source_key)
             ~destination_bucket:bucket
             ~destination_key:(key_to_object_key destination_key)
             ?options ())
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
  let context = command_context ~command_index ~model_before:model command in
  with_command_context context (fun () ->
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
        | Put_string_metadata (key, body, tags, metadata) ->
            let options =
              Object.Put.options_exn
                ~metadata:(metadata_to_store metadata)
                ~tags:(tags_to_set tags) ()
            in
            let result =
              expect_ok command_index command "put_string metadata"
                (Simulator.Object.put_string conn ~bucket
                   ~key:(key_to_object_key key) ~options ~contents:body ())
            in
            check_version_id_presence command_index command "put version id"
              (Model.versioning_keeps_history model)
              result.version_id;
            let next_model = Model.apply command model in
            assert_get command_index command conn key
              (Model.find key next_model);
            assert_object_tags command_index command conn key
              (Model.find key next_model);
            next_model
        | Get_string key ->
            assert_get command_index command conn key (Model.find key model);
            model
        | Get_range (key, range) ->
            assert_get_range command_index command conn key range model;
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
        | List_keys_page { prefix; max_keys } ->
            assert_list_keys_page command_index command conn prefix max_keys
              model;
            model
        | List_versions_page { max_keys } ->
            assert_list_versions_page command_index command conn max_keys model;
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
        | Copy_object_metadata (source_key, destination_key, metadata) ->
            let source = Model.find source_key model in
            let options =
              match metadata with
              | Command.Copy_source_metadata -> None
              | Replace_metadata metadata ->
                  Some
                    (Object.Copy.options_exn
                       ~metadata_directive:
                         (`Replace (metadata_to_store metadata))
                       ())
            in
            assert_copy command_index command conn ~source_key ~destination_key
              ?options
              ~destination_has_version_id:(Model.versioning_keeps_history model)
              source;
            let next_model = Model.apply command model in
            (match source with
            | None -> ()
            | Some _ ->
                assert_get command_index command conn destination_key
                  (Model.find destination_key next_model);
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
            assert_object_tags command_index command conn key
              (Model.find key model);
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
            assert_get_versioning command_index command conn
              next_model.versioning;
            next_model
        | Get_versioning ->
            assert_get_versioning command_index command conn model.versioning;
            model
      in
      check_store_state command_index command conn next_model;
      next_model)

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

let workload_replay_dir =
  List.fold_left Filename.concat ".." [ "fixtures"; "workload-replay" ]

let replay_path name = Filename.concat workload_replay_dir name

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let parse_replay_file path =
  match S3_replay.decode (read_file path) with
  | Ok commands -> commands
  | Error error -> Alcotest.fail (S3_replay.parse_error_to_string ~path error)

let run_replay_file path =
  let commands = parse_replay_file path in
  with_command_transcript (Command.transcript commands) (fun () ->
      Target.with_connection (fun conn ->
          Target.check_store 0 Command.List_keys conn Model.empty;
          let (_ : Model.t) =
            List.fold_left
              (fun model (index, command) ->
                let next_model = Target.run_command index conn model command in
                Target.check_store index command conn next_model;
                next_model)
              Model.empty
              (List.mapi (fun index command -> (index + 1, command)) commands)
          in
          ()))

let replay_case name =
  Alcotest.test_case name `Quick (fun () -> run_replay_file (replay_path name))

let suite =
  Workload.suite
  @ [
      ( "replay:awskit-s3-sim:s3-state",
        [
          replay_case "versioning-after-put.txt";
          replay_case "range-read-boundaries.txt";
        ] );
    ]
