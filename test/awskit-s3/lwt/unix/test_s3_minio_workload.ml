module S3 = Awskit_s3_lwt_unix
open S3_model
module Bucket = Awskit_s3.Bucket
module Bucket_name = Awskit_s3.Bucket_name
module Command = S3_command
module Content_type = Awskit_s3.Content_type
module Metadata = Awskit_s3.Metadata
module Multipart = Awskit_s3.Multipart
module Model = S3_model
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key
module Tag = Awskit_s3.Tag
module Transfer = Awskit_s3.Transfer

let getenv_default name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

type integration_profile = S3_workload.integration_profile =
  | Bounded
  | Expensive

let integration_profile_allowed =
  String.concat ", " S3_workload.integration_profile_allowed_values

let integration_profile_to_string = S3_workload.integration_profile_to_string

let parse_integration_profile () =
  match Sys.getenv_opt "AWSKIT_INTEGRATION_PROFILE" with
  | None | Some "" -> Ok Bounded
  | Some value -> (
      match S3_workload.integration_profile_of_string value with
      | Ok profile -> Ok profile
      | Error (S3_workload.Unknown_integration_profile value) -> Error value)

let selected_integration_profile_result = parse_integration_profile ()

let selected_integration_profile =
  match selected_integration_profile_result with
  | Ok profile -> profile
  | Error _ -> Bounded

let selected_integration_profile_name =
  integration_profile_to_string selected_integration_profile

let selected_workload_profile =
  S3_workload.minio_workload_profile selected_integration_profile

let endpoint = getenv_default "AWSKIT_S3_MINIO_ENDPOINT" "http://127.0.0.1:9000"
let unsafe_http = getenv_default "AWSKIT_S3_MINIO_UNSAFE_HTTP" ""
let access_key = getenv_default "AWSKIT_S3_MINIO_ACCESS_KEY_ID" "minioadmin"
let secret_key = getenv_default "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY" "minioadmin"
let region = getenv_default "AWSKIT_S3_MINIO_REGION" "us-east-1"

let minio_config_vars =
  [
    "AWSKIT_S3_MINIO_ENDPOINT";
    "AWSKIT_S3_MINIO_ACCESS_KEY_ID";
    "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY";
    "AWSKIT_S3_MINIO_REGION";
    "AWSKIT_S3_MINIO_UNSAFE_HTTP";
  ]

let minio_configured =
  List.exists
    (fun name ->
      match Sys.getenv_opt name with
      | Some value when value <> "" -> true
      | _ -> false)
    minio_config_vars

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
let normalize_metadata metadata = List.sort compare_string_pair metadata

let tags_of_set tags =
  Tag.Set.to_list tags
  |> List.map (fun tag -> (Tag.key tag, Tag.value tag))
  |> normalize_tags

let metadata_to_set metadata = Metadata.to_list metadata |> normalize_metadata
let metadata_of_model metadata = Metadata.of_list_exn metadata

let report_selected_profile profile =
  Format.eprintf
    "@[<v>Local-service integration profile: %s@;\
     Evidence target: local S3-compatible MinIO test double; this is not AWS \
     provider certification.@]@."
    (integration_profile_to_string profile)

let fail_unconfigured_minio profile label error =
  Alcotest.failf
    "Local-service integration profile %s requires a reachable local MinIO \
     test double. No AWSKIT_S3_MINIO_* configuration was supplied and %s \
     failed against the default endpoint. Start local MinIO with default \
     credentials, run scripts/test.sh integration, or set \
     AWSKIT_S3_MINIO_ENDPOINT, AWSKIT_S3_MINIO_ACCESS_KEY_ID, \
     AWSKIT_S3_MINIO_SECRET_ACCESS_KEY, and AWSKIT_S3_MINIO_REGION explicitly. \
     Original error: %a"
    (integration_profile_to_string profile)
    label Awskit_s3.Error.pp error

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

let write_file path body =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel body)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let remove_file path = try Sys.remove path with Sys_error _ -> ()

let transfer_body length =
  String.init length (fun index ->
      match index mod 17 with
      | 0 -> '\000'
      | 1 -> '\255'
      | _ -> Char.chr (32 + (index * 13 mod 91)))

let has_final_progress ~direction ~phase ~total (event : Transfer.progress) =
  event.direction = direction
  && event.phase = phase
  && Int64.equal event.transferred total
  && event.total = Some total

let check_final_progress label ~direction ~phase ~total progress =
  Alcotest.(check bool)
    label true
    (List.exists (has_final_progress ~direction ~phase ~total) progress)

let observe_lwt promise =
  Lwt.catch
    (fun () -> Lwt.map (fun result -> `Returned result) promise)
    (fun exn -> Lwt.return (`Raised exn))

let expect_raised_exception label matches = function
  | `Raised exn when matches exn -> ()
  | `Raised exn ->
      Alcotest.failf "%s raised unexpected exception: %s" label
        (Printexc.to_string exn)
  | `Returned (Error error) ->
      Alcotest.failf "%s returned error instead of raising: %a" label
        Awskit_s3.Error.pp error
  | `Returned (Ok _) -> Alcotest.failf "%s unexpectedly succeeded" label

let check_object_absent label conn ~bucket ~key =
  Alcotest.(check bool)
    label false
    (await_ok (label ^ " exists") (S3.Object.exists conn ~bucket ~key ()))

let delete_object_version key version_id =
  match version_id with
  | Some version_id -> Object.Delete_many.object_ ~key ~version_id ()
  | None -> delete_object_key key

let bucket_to_string bucket = Bucket_name.to_string bucket
let key_to_string key = Object_key.to_string key

let version_id_to_string = function
  | None -> "current"
  | Some version_id -> Object.Version_id.to_string version_id

let upload_to_string upload =
  Printf.sprintf "bucket=%s key=%s upload_id=%s"
    (Multipart.Upload.bucket upload |> bucket_to_string)
    (Multipart.Upload.key upload |> key_to_string)
    (Multipart.Upload.upload_id upload |> Multipart.Upload_id.to_string)

let cleanup_object_to_string (object_ : Object.Delete_many.object_) =
  Printf.sprintf "key=%s version=%s"
    (key_to_string object_.key)
    (version_id_to_string object_.version_id)

let preview_list ~to_string values =
  let max_items = 8 in
  let rec loop remaining count acc = function
    | [] -> (List.rev acc, count, 0)
    | rest when remaining = 0 -> (List.rev acc, count, List.length rest)
    | value :: rest ->
        loop (remaining - 1) (count + 1) (to_string value :: acc) rest
  in
  let preview, shown, omitted = loop max_items 0 [] values in
  let suffix =
    if omitted = 0 then "" else Printf.sprintf "; ... %d more" omitted
  in
  Printf.sprintf "count=%d shown=%d [%s%s]" (List.length values) shown
    (String.concat "; " preview)
    suffix

let cleanup_context bucket error =
  Awskit.Error.Producer.with_context
    (Printf.sprintf "cleaning MinIO bucket %s" (bucket_to_string bucket))
    error

let cleanup_delete_objects_context ~bucket objects =
  Printf.sprintf "deleting MinIO cleanup objects bucket=%s %s"
    (bucket_to_string bucket)
    (preview_list ~to_string:cleanup_object_to_string objects)

let delete_item_cleanup_error ~bucket (error : Object.Delete_many.item_error) =
  Awskit.Error.Producer.service ~status:200 ~code:error.code
    ?message:error.message ~headers:[] ()
  |> Awskit.Error.Producer.with_context
       (Printf.sprintf "delete_objects item failed bucket=%s key=%s"
          (bucket_to_string bucket) (key_to_string error.key))

let cleanup_delete_objects conn ~bucket objects =
  let open Lwt.Syntax in
  match objects with
  | [] -> Lwt.return_ok ()
  | _ -> (
      let context = cleanup_delete_objects_context ~bucket objects in
      let* result = S3.Object.delete_objects conn ~bucket ~objects () in
      match result with
      | Error error ->
          Lwt.return_error (Awskit.Error.Producer.with_context context error)
      | Ok ({ errors = []; _ } : Object.Delete_many.result) -> Lwt.return_ok ()
      | Ok result ->
          Lwt.return_error
            (List.map (delete_item_cleanup_error ~bucket) result.errors
            |> Awskit.Error.Producer.multiple
            |> Awskit.Error.Producer.with_context context))

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
                 (Printf.sprintf "listing MinIO cleanup objects bucket=%s"
                    (bucket_to_string bucket))))

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
      Alcotest.failf "MinIO cleanup failed in profile %s: %a"
        selected_integration_profile_name Awskit_s3.Error.pp error

let report_cleanup_after_primary_failure error =
  Format.eprintf
    "@[<v>MinIO cleanup failed after the primary workload failure in profile \
     %s; preserving the primary failure.@;\
     %a@]@."
    selected_integration_profile_name Awskit_s3.Error.pp error

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
  QCheck.Test.fail_reportf "integration_profile=%s command #%d %s: %s"
    selected_integration_profile_name command_index
    (Command.to_string command)
    message

let fail_error command_index command label error =
  fail command_index command
    (Printf.sprintf "%s unexpected error shape %s" label (error_shape error))

let expect_no_such_key command_index command label = function
  | Error error when is_absent_object_error error -> ()
  | Error error -> fail_error command_index command label error
  | Ok _ -> fail command_index command (label ^ " expected NoSuchKey")

let is_invalid_range_error error =
  match
    (Awskit.Error.service_status error, Awskit.Error.service_code error)
  with
  | Some 416, Some "InvalidRange" | _, Some "InvalidRange" -> true
  | _ -> false

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.equal (String.sub text index substring_length) substring then
      true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let is_content_range_decode_error error =
  match Awskit.Error.kind error with
  | Decode { message } -> contains message "Content-Range"
  | _ -> false

(* MinIO returns 206 with Content-Range: bytes 0--1/0 for suffix ranges
   against empty objects. The MinIO target-profile law accepts only Awskit's
   decode rejection of that malformed header; the shared model still expects an
   invalid range and a successful read is still a failure. *)
let minio_empty_object_suffix_range_law model key range =
  match (Model.find key model, Awskit_s3.Range.view range) with
  | Some (object_ : Model.object_), Suffix _ when String.length object_.body = 0
    ->
      true
  | _ -> false

let minio_empty_object_suffix_range_decode_law model key range error =
  minio_empty_object_suffix_range_law model key range
  && is_content_range_decode_error error

let expect_invalid_range command_index command label ~model ~key ~range =
  function
  | Error error when is_invalid_range_error error -> ()
  | Error error
    when minio_empty_object_suffix_range_decode_law model key range error ->
      ()
  | Error error -> fail_error command_index command label error
  | Ok _ -> fail command_index command (label ^ " expected InvalidRange")

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

let check_metadata command_index command label expected actual =
  check_equal command_index command
    Alcotest.(list (pair string string))
    label
    (normalize_metadata expected)
    (metadata_to_set actual)

let max_bytes_for_expected = function
  | Some object_ -> Int64.of_int (String.length object_.body)
  | None -> 64L

let content_length_of_model object_ =
  Some (Int64.of_int (String.length object_.body))

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

let actual_content_range_to_string =
  Option.map Awskit_s3.Range.Content_range.to_header

let assert_get command_index command conn key expected =
  let max_bytes = max_bytes_for_expected expected in
  match expected with
  | Some object_ ->
      let result =
        expect_ok command_index command "get_string"
          (await_result "get_string"
             (S3.Object.get_string conn ~bucket ~key:(object_key key) ~max_bytes
                ()))
      in
      check_equal command_index command Alcotest.string "get body" object_.body
        result.value;
      check_equal command_index command
        Alcotest.(option int64)
        "get content length"
        (content_length_of_model object_)
        result.content_length;
      check_metadata command_index command "get metadata" object_.metadata
        result.metadata;
      check_version_id_presence command_index command "get version id"
        object_.has_version_id result.version_id
  | None ->
      expect_no_such_key command_index command "get_string"
        (await_result "get_string"
           (S3.Object.get_string conn ~bucket ~key:(object_key key) ~max_bytes
              ()))

let assert_get_range command_index command conn key range model =
  let max_bytes = max_bytes_for_expected (Model.find key model) in
  let options = Object.Get.options ~range () in
  match Model.expected_result command model with
  | Get_ok summary ->
      let result =
        expect_ok command_index command "get range"
          (await_result "get range"
             (S3.Object.get_string conn ~bucket ~key:(object_key key) ~options
                ~max_bytes ()))
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
        (await_result "get range"
           (S3.Object.get_string conn ~bucket ~key:(object_key key) ~options
              ~max_bytes ()))
  | Invalid_range ->
      expect_invalid_range command_index command "get range" ~model ~key ~range
        (await_result "get range"
           (S3.Object.get_string conn ~bucket ~key:(object_key key) ~options
              ~max_bytes ()))
  | result ->
      fail command_index command
        (Printf.sprintf "unexpected model result %s for range get"
           (Model.operation_result_kind result))

let assert_find command_index command conn key expected =
  let max_bytes = max_bytes_for_expected expected in
  match
    await_result "find_string"
      (S3.Object.find_string conn ~bucket ~key:(object_key key) ~max_bytes ())
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
          (await_result "head"
             (S3.Object.head conn ~bucket ~key:(object_key key) ()))
      in
      check_equal command_index command
        Alcotest.(option int64)
        "head content length"
        (content_length_of_model object_)
        result.content_length;
      check_metadata command_index command "head metadata" object_.metadata
        result.metadata;
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
  let expected_to_string = Option.map Bucket.Versioning.Status.to_string in
  let observed_to_string =
    Option.map Bucket.Versioning.Status.observed_to_string
  in
  let result =
    expect_ok command_index command "get versioning"
      (await_result "get versioning" (S3.Bucket.Versioning.get conn ~bucket ()))
  in
  check_equal command_index command
    Alcotest.(option string)
    "versioning status"
    (expected_to_string expected)
    (observed_to_string result.status)

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
      (await_result "list keys page" (S3.Object.list conn ~bucket ~options ()))
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

let assert_copy command_index command conn ~source_key ~destination_key ?options
    ~destination_has_version_id expected =
  match expected with
  | Some _source ->
      let result =
        expect_ok command_index command "copy"
          (await_result "copy"
             (S3.Object.copy conn ~source_bucket:bucket
                ~source_key:(object_key source_key) ~destination_bucket:bucket
                ~destination_key:(object_key destination_key)
                ?options ()))
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
              ?options ()))

let put_options ~tags ~metadata =
  let tags = match tags with [] -> None | tags -> Some (tags_to_set tags) in
  let metadata =
    match metadata with
    | [] -> None
    | metadata -> Some (metadata_of_model metadata)
  in
  match (tags, metadata) with
  | None, None -> None
  | _ -> Some (Object.Put.options ?tags ?metadata ())

let assert_visible_store_object command_index command conn key object_ =
  let expected = Some object_ in
  assert_get command_index command conn key expected;
  assert_head command_index command conn key expected;
  assert_object_tags command_index command conn key expected

let assert_absent_profile_key command_index command conn model key =
  match Model.find key model with
  | Some _ -> ()
  | None ->
      let result =
        expect_ok command_index command
          (Printf.sprintf "store absent exists %s" key)
          (await_result "store absent exists"
             (S3.Object.exists conn ~bucket ~key:(object_key key) ()))
      in
      check_equal command_index command Alcotest.bool
        (Printf.sprintf "store absent exists %s" key)
        false result

let assert_absent_profile_keys command_index command conn model =
  Command.keys_for_profile Command.Small
  |> List.iter (assert_absent_profile_key command_index command conn model)

let assert_prefix_domain command_index command conn model =
  Command.prefix_domain
  |> List.iter (fun prefix ->
      assert_list_prefix command_index command conn prefix model)

let check_store command_index command conn model =
  assert_list_keys command_index command conn model;
  Model.String_map.bindings model.objects
  |> List.iter (fun (key, object_) ->
      assert_visible_store_object command_index command conn key object_);
  assert_absent_profile_keys command_index command conn model;
  assert_prefix_domain command_index command conn model;
  assert_bucket_tags command_index command conn model.bucket_tags;
  assert_get_versioning command_index command conn model.versioning

let apply_minio_command command_index conn model command =
  let next_model =
    match command with
    | Command.Put_string (key, body, tags) ->
        let options = put_options ~tags ~metadata:[] in
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
        assert_head command_index command conn key (Model.find key next_model);
        next_model
    | Put_string_metadata (key, body, tags, metadata) ->
        let options = put_options ~tags ~metadata in
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
        assert_head command_index command conn key (Model.find key next_model);
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
    | List_keys_page { prefix; max_keys } ->
        assert_list_keys_page command_index command conn prefix max_keys model;
        model
    | List_versions_page _ ->
        fail command_index command
          "list versions page is unsupported by the MinIO workload profile"
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
              (Model.find destination_key next_model);
            assert_head command_index command conn destination_key
              (Model.find destination_key next_model));
        next_model
    | Copy_object_metadata (source_key, destination_key, metadata) ->
        let metadata_directive =
          match metadata with
          | Command.Copy_source_metadata -> `Copy
          | Replace_metadata metadata -> `Replace (metadata_of_model metadata)
        in
        let options = Object.Copy.options ~metadata_directive () in
        let source = Model.find source_key model in
        assert_copy command_index command conn ~source_key ~destination_key
          ~options
          ~destination_has_version_id:(Model.versioning_keeps_history model)
          source;
        let next_model = Model.apply command model in
        (match source with
        | None -> ()
        | Some _ ->
            assert_object_tags command_index command conn destination_key
              (Model.find destination_key next_model);
            assert_head command_index command conn destination_key
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
  S3_workload.Make_with_profile
    (struct
      let workload_profile = selected_workload_profile
    end)
    (Minio_target)

let with_minio_bucket f =
  let conn = connect () in
  cleanup_bucket_or_fail conn ~bucket;
  ignore
    (await_ok "create bucket" (S3.Bucket.create conn ~bucket ())
      : Bucket.Create.result);
  protect_with_bucket_cleanup conn ~bucket (fun () -> f conn ~bucket)

let run_minio_transcript conn commands =
  ignore
    (List.fold_left
       (fun model (index, command) ->
         let next_model = apply_minio_command index conn model command in
         check_store index command conn next_model;
         next_model)
       Model.empty
       (List.mapi (fun index command -> (index + 1, command)) commands)
      : Model.t)

let test_minio_target_profile_boundaries () =
  let rejected_key_command =
    Command.Put_string ("prefix//double-slash", "body", [ ("env", "dev") ])
  in
  let trailing_key_command = Command.Put_string ("prefix/trailing/", "", []) in
  let versioned_trailing_key_commands =
    [
      Command.Put_versioning Bucket.Versioning.Status.Enabled;
      trailing_key_command;
    ]
  in
  let unversioned_delete_commands =
    [
      Command.Put_string ("a.txt", "delete-me", []);
      Command.Delete_object "a.txt";
    ]
  in
  let versioned_delete_marker_commands =
    [
      Command.Put_versioning Bucket.Versioning.Status.Enabled;
      Command.Put_string ("a.txt", "delete-me", []);
      Command.Delete_object "a.txt";
    ]
  in
  Alcotest.(check bool)
    "strict profile keeps double slash key coverage" true
    (S3_history.history_supported S3_history.strict_default
       [ rejected_key_command ]);
  Alcotest.(check bool)
    "MinIO broad profile excludes rejected double slash key" false
    (S3_history.history_supported S3_history.minio_expensive
       [ rejected_key_command ]);
  Alcotest.(check bool)
    "strict profile keeps versioned trailing slash key coverage" true
    (S3_history.history_supported S3_history.strict_default
       versioned_trailing_key_commands);
  Alcotest.(check bool)
    "MinIO broad profile excludes versioned trailing slash key" false
    (S3_history.history_supported S3_history.minio_expensive
       versioned_trailing_key_commands);
  Alcotest.(check bool)
    "MinIO broad profile keeps unversioned trailing slash key" true
    (S3_history.history_supported S3_history.minio_expensive
       [ trailing_key_command ]);
  Alcotest.(check bool)
    "strict profile keeps versioned delete-marker coverage" true
    (S3_history.history_supported S3_history.strict_default
       versioned_delete_marker_commands);
  Alcotest.(check bool)
    "MinIO broad profile excludes versioned delete markers" false
    (S3_history.history_supported S3_history.minio_expensive
       versioned_delete_marker_commands);
  Alcotest.(check bool)
    "MinIO broad profile keeps unversioned deletes" true
    (S3_history.history_supported S3_history.minio_expensive
       unversioned_delete_commands)

let test_empty_suffix_range_shared_model_law () =
  let range = Awskit_s3.Range.suffix_exn 1L in
  let command = Command.Get_range ("empty.txt", range) in
  let model =
    Model.apply (Command.Put_string ("empty.txt", "", [])) Model.empty
  in
  Alcotest.(check string)
    "empty object suffix range remains invalid in shared model" "invalid-range"
    (Model.expected_result command model |> Model.operation_result_kind)

let test_empty_suffix_range_target_profile_law () =
  with_minio_bucket (fun conn ~bucket:_ ->
      run_minio_transcript conn
        [
          Command.Put_string ("empty.txt", "", []);
          Command.Get_range ("empty.txt", Awskit_s3.Range.suffix_exn 1L);
        ])

let test_state_transcript_covers_minio_profile () =
  with_minio_bucket (fun conn ~bucket:_ ->
      run_minio_transcript conn
        [
          Put_versioning Bucket.Versioning.Status.Enabled;
          Command.Put_string ("a.txt", "alpha", [ ("env", "dev") ]);
          Get_range ("a.txt", Awskit_s3.Range.bytes_exn ~start:1L ~finish:3L);
          Get_range ("a.txt", Awskit_s3.Range.from_exn 2L);
          Get_range ("a.txt", Awskit_s3.Range.suffix_exn 2L);
          Get_range ("a.txt", Awskit_s3.Range.bytes_exn ~start:5L ~finish:9L);
          Put_string ("empty.txt", "", []);
          Get_range ("empty.txt", Awskit_s3.Range.suffix_exn 1L);
          Put_string ("logs/a.txt", "log-a", [ ("team", "storage") ]);
          Put_string ("photos/2026.jpg", "image-before-overwrite", []);
          Get_string "a.txt";
          Head_object "logs/a.txt";
          Exists_object "missing.txt";
          List_keys;
          List_prefix "logs/";
          List_keys_page { prefix = Some "logs/"; max_keys = 1 };
          Copy_object ("a.txt", "b.txt");
          Copy_object ("logs/a.txt", "photos/2026.jpg");
          Put_object_tags ("b.txt", [ ("env", "prod"); ("owner", "sdk") ]);
          Get_object_tags "b.txt";
          Delete_object_tags "b.txt";
          Put_bucket_tags [ ("team", "storage"); ("mode", "pbt") ];
          Get_bucket_tags;
          Delete_bucket_tags;
          Put_string ("photos/2026.jpg", "image", []);
          Delete_object "photos/2026.jpg";
          Get_versioning;
        ])

let test_object_metadata_tags_and_delete () =
  with_minio_bucket (fun conn ~bucket ->
      let key = object_key "metadata/object.txt" in
      let metadata =
        Metadata.of_list_exn [ ("tenant", "minio"); ("trace-id", "abc123") ]
      in
      let tags = [ ("env", "dev"); ("owner", "sdk") ] in
      let options =
        Object.Put.options
          ~content_type:(Content_type.of_string_exn "text/plain")
          ~metadata ~tags:(tags_to_set tags) ()
      in
      ignore
        (await_ok "put metadata object"
           (S3.Object.put_string conn ~bucket ~key ~options
              ~contents:"metadata-body" ())
          : Object.Put.result);
      let head =
        await_ok "head metadata object" (S3.Object.head conn ~bucket ~key ())
      in
      Alcotest.(check (option int64))
        "metadata content length" (Some 13L) head.content_length;
      Alcotest.(check (option string))
        "metadata content type" (Some "text/plain")
        (Option.map Content_type.to_string head.content_type);
      Alcotest.(check (list (pair string string)))
        "head metadata" (metadata_to_set metadata)
        (metadata_to_set head.metadata);
      let read =
        await_ok "get metadata object"
          (S3.Object.get_string conn ~bucket ~key ~max_bytes:32L ())
      in
      Alcotest.(check string) "metadata body" "metadata-body" read.value;
      Alcotest.(check (list (pair string string)))
        "get metadata" (metadata_to_set metadata)
        (metadata_to_set read.metadata);
      let tag_result =
        await_ok "get object tags" (S3.Object.Tagging.get conn ~bucket ~key ())
      in
      Alcotest.(check (list (pair string string)))
        "object tags" (normalize_tags tags)
        (tags_of_set tag_result.tags);
      ignore
        (await_ok "delete metadata object"
           (S3.Object.delete conn ~bucket ~key ())
          : Object.Delete.result);
      Alcotest.(check bool)
        "deleted object absent" false
        (await_ok "deleted object exists"
           (S3.Object.exists conn ~bucket ~key ())))

let test_list_pagination_is_service_backed () =
  with_minio_bucket (fun conn ~bucket ->
      let keys =
        [ "page/a.txt"; "page/b.txt"; "page/c.txt"; "page/d.txt"; "page/e.txt" ]
      in
      List.iter
        (fun key ->
          ignore
            (await_ok ("put " ^ key)
               (S3.Object.put_string conn ~bucket ~key:(object_key key)
                  ~contents:key ())
              : Object.Put.result))
        keys;
      let options =
        Object.List.options_exn
          ~prefix:(Object_key.Prefix.of_string_exn "page/")
          ~max_keys:2 ()
      in
      let pages =
        await_ok "list paginated keys"
          (S3.Object.List.pages conn ~bucket ~options ~max_pages:4 ())
      in
      Alcotest.(check bool)
        "listing uses more than one page" true
        (List.length pages > 1);
      let actual =
        pages
        |> List.concat_map (fun (page : Object.List.page) -> page.objects)
        |> List.map (fun (object_ : Object.List.object_summary) ->
            Object_key.to_string object_.key)
      in
      Alcotest.(check (list string)) "paginated keys" keys actual)

let test_manual_multipart_lifecycle () =
  with_minio_bucket (fun conn ~bucket ->
      let key = object_key "manual-multipart.bin" in
      let key_context =
        Printf.sprintf "bucket=%s key=%s" (bucket_to_string bucket)
          (key_to_string key)
      in
      let metadata = Metadata.of_list_exn [ ("transfer", "manual") ] in
      let create_options =
        Multipart.Create.options ~metadata
          ~tags:(tags_to_set [ ("mode", "manual") ])
          ()
      in
      let create =
        await_ok
          (Printf.sprintf "create manual multipart upload %s" key_context)
          (S3.Multipart.create_upload conn ~bucket ~key ~options:create_options
             ())
      in
      let upload_context = upload_to_string create.upload in
      let part_size = Transfer.min_part_size in
      let first_body = transfer_body part_size in
      let second_body = transfer_body 113 in
      let upload_part part_number contents =
        await_ok
          (Printf.sprintf "upload manual multipart part %d %s" part_number
             upload_context)
          (S3.Multipart.upload_part conn ~upload:create.upload
             ~part_number:(Multipart.Part_number.of_int_exn part_number)
             ~body:(S3.Body.of_string contents)
             ())
      in
      let first = upload_part 1 first_body in
      let second = upload_part 2 second_body in
      let listed_parts =
        await_ok
          (Printf.sprintf "list manual multipart parts %s" upload_context)
          (S3.Multipart.List_parts.parts conn ~upload:create.upload ~max_pages:2
             ())
      in
      Alcotest.(check (list int))
        "listed part numbers" [ 1; 2 ]
        (List.map
           (fun (part : Multipart.List_parts.part_info) ->
             Multipart.Part_number.to_int part.part_number)
           listed_parts);
      Alcotest.(check (list (option int64)))
        "listed part sizes"
        [ Some (Int64.of_int part_size); Some (Int64.of_int 113) ]
        (List.map
           (fun (part : Multipart.List_parts.part_info) -> part.size)
           listed_parts);
      ignore
        (await_ok
           (Printf.sprintf "complete manual multipart upload %s" upload_context)
           (S3.Multipart.complete_upload conn ~upload:create.upload
              ~parts:[ first.part; second.part ]
              ())
          : Multipart.Complete.result);
      let body = first_body ^ second_body in
      let object_ =
        await_ok "get manual multipart object"
          (S3.Object.get_string conn ~bucket ~key
             ~max_bytes:(Int64.of_int (String.length body))
             ())
      in
      Alcotest.(check string) "manual multipart body" body object_.value;
      Alcotest.(check (list (pair string string)))
        "manual multipart metadata" (metadata_to_set metadata)
        (metadata_to_set object_.metadata);
      let tags =
        await_ok "manual multipart tags"
          (S3.Object.Tagging.get conn ~bucket ~key ())
      in
      Alcotest.(check (list (pair string string)))
        "manual multipart tags"
        [ ("mode", "manual") ]
        (tags_of_set tags.tags))

let test_transfer_small_roundtrip () =
  with_minio_bucket (fun conn ~bucket ->
      let upload_path = Filename.temp_file "awskit-minio-upload-small" ".bin" in
      let download_path =
        Filename.temp_file "awskit-minio-download-small" ".bin"
      in
      let body = transfer_body ((128 * 1024) + 17) in
      let total = Int64.of_int (String.length body) in
      let upload_progress = ref [] in
      let download_progress = ref [] in
      write_file upload_path body;
      remove_file download_path;
      Fun.protect
        ~finally:(fun () ->
          remove_file upload_path;
          remove_file download_path)
        (fun () ->
          let upload_options =
            Transfer.upload_options_exn ~multipart_threshold:(Int64.succ total)
              ()
          in
          let download_options =
            Transfer.download_options_exn
              ~multipart_threshold:(Int64.succ total) ()
          in
          let upload_result =
            await_ok "small transfer upload"
              (S3.Object.Transfer.upload_file conn ~bucket
                 ~key:(object_key "small-transfer.bin")
                 ~options:upload_options ~path:upload_path
                 ~on_progress:(fun event ->
                   upload_progress := event :: !upload_progress)
                 ())
          in
          let download_result =
            await_ok "small transfer download"
              (S3.Object.Transfer.download_file conn ~bucket
                 ~key:(object_key "small-transfer.bin")
                 ~options:download_options ~path:download_path
                 ~on_progress:(fun event ->
                   download_progress := event :: !download_progress)
                 ())
          in
          Alcotest.(check bool)
            "upload strategy" true
            (Transfer.upload_strategy upload_result = `Put);
          Alcotest.(check bool)
            "download strategy" true
            (Transfer.download_strategy download_result = `Get);
          Alcotest.(check int64)
            "upload bytes" total
            (Transfer.upload_bytes_transferred upload_result);
          Alcotest.(check int64)
            "download bytes" total
            (Transfer.download_bytes_transferred download_result);
          Alcotest.(check string)
            "downloaded body" body (read_file download_path);
          check_final_progress "upload final progress"
            ~direction:Transfer.Upload ~phase:Transfer.Single_request ~total
            !upload_progress;
          check_final_progress "download final progress"
            ~direction:Transfer.Download ~phase:Transfer.Single_request ~total
            !download_progress))

let test_transfer_multipart_upload_bytes () =
  with_minio_bucket (fun conn ~bucket ->
      let path = Filename.temp_file "awskit-minio-upload-multipart" ".bin" in
      let part_size = Transfer.min_part_size in
      let body = transfer_body (part_size + 4099) in
      let total = Int64.of_int (String.length body) in
      let progress = ref [] in
      write_file path body;
      Fun.protect
        ~finally:(fun () -> remove_file path)
        (fun () ->
          let options =
            Transfer.upload_options_exn
              ~multipart_threshold:(Int64.of_int part_size) ~part_size
              ~concurrency:2 ()
          in
          let upload_result =
            await_ok "multipart transfer upload"
              (S3.Object.Transfer.upload_file conn ~bucket
                 ~key:(object_key "multipart-transfer.bin")
                 ~options ~path
                 ~on_progress:(fun event -> progress := event :: !progress)
                 ())
          in
          Alcotest.(check bool)
            "upload strategy" true
            (Transfer.upload_strategy upload_result = `Multipart);
          Alcotest.(check int64)
            "upload bytes" total
            (Transfer.upload_bytes_transferred upload_result);
          (match upload_result with
          | Transfer.Multipart { parts; _ } ->
              Alcotest.(check int) "multipart parts" 2 (List.length parts)
          | Transfer.Put _ -> Alcotest.fail "expected multipart upload result");
          let remote =
            await_ok "get multipart transfer"
              (S3.Object.get_string conn ~bucket
                 ~key:(object_key "multipart-transfer.bin")
                 ~max_bytes:total ())
          in
          Alcotest.(check string) "remote body" body remote.value;
          check_final_progress "multipart final progress"
            ~direction:Transfer.Upload ~phase:Transfer.Part ~total !progress))

let test_transfer_ranged_download_bytes () =
  with_minio_bucket (fun conn ~bucket ->
      let download_path =
        Filename.temp_file "awskit-minio-download-ranged" ".bin"
      in
      let part_size = Transfer.min_part_size in
      let body = transfer_body (part_size + 2048) in
      let total = Int64.of_int (String.length body) in
      let progress = ref [] in
      remove_file download_path;
      Fun.protect
        ~finally:(fun () -> remove_file download_path)
        (fun () ->
          ignore
            (await_ok "put ranged transfer object"
               (S3.Object.put_string conn ~bucket
                  ~key:(object_key "ranged-transfer.bin")
                  ~contents:body ())
              : Object.Put.result);
          let options =
            Transfer.download_options_exn
              ~multipart_threshold:(Int64.of_int part_size) ~part_size
              ~concurrency:2 ()
          in
          let download_result =
            await_ok "ranged transfer download"
              (S3.Object.Transfer.download_file conn ~bucket
                 ~key:(object_key "ranged-transfer.bin")
                 ~options ~path:download_path
                 ~on_progress:(fun event -> progress := event :: !progress)
                 ())
          in
          Alcotest.(check bool)
            "download strategy" true
            (Transfer.download_strategy download_result = `Ranged);
          Alcotest.(check int64)
            "download bytes" total
            (Transfer.download_bytes_transferred download_result);
          (match download_result with
          | Transfer.Ranged { parts; _ } ->
              Alcotest.(check int) "ranged parts" 2 parts
          | Transfer.Get _ -> Alcotest.fail "expected ranged download result");
          Alcotest.(check string)
            "ranged downloaded body" body (read_file download_path);
          check_final_progress "ranged final progress"
            ~direction:Transfer.Download ~phase:Transfer.Ranged_get ~total
            !progress))

let test_transfer_progress_callback_exception_aborts_owned_multipart_upload () =
  with_minio_bucket (fun conn ~bucket ->
      let exception Progress_failed in
      let path = Filename.temp_file "awskit-minio-upload-failing" ".bin" in
      let part_size = Transfer.min_part_size in
      let body = transfer_body (part_size + 1024) in
      let progress_seen = ref false in
      write_file path body;
      Fun.protect
        ~finally:(fun () -> remove_file path)
        (fun () ->
          let options =
            Transfer.upload_options_exn
              ~multipart_threshold:(Int64.of_int part_size) ~part_size
              ~concurrency:1 ()
          in
          let observed =
            Lwt_main.run
              (observe_lwt
                 (S3.Object.Transfer.multipart_upload_file conn ~bucket
                    ~key:(object_key "progress-failed-multipart-transfer.bin")
                    ~options ~path
                    ~on_progress:(fun _event ->
                      progress_seen := true;
                      raise Progress_failed)
                    ()))
          in
          expect_raised_exception "multipart progress callback"
            (function Progress_failed -> true | _ -> false)
            observed;
          Alcotest.(check bool) "progress observed" true !progress_seen;
          check_object_absent "failed transfer object absent" conn ~bucket
            ~key:(object_key "progress-failed-multipart-transfer.bin")))

let test_transfer_ranged_download_callback_exception_leaves_no_success () =
  with_minio_bucket (fun conn ~bucket ->
      let exception Download_progress_failed in
      let download_path =
        Filename.temp_file "awskit-minio-download-ranged-failing" ".bin"
      in
      let part_size = Transfer.min_part_size in
      let body = transfer_body (part_size + 2048) in
      let progress_seen = ref false in
      remove_file download_path;
      Fun.protect
        ~finally:(fun () -> remove_file download_path)
        (fun () ->
          ignore
            (await_ok "put failing ranged transfer object"
               (S3.Object.put_string conn ~bucket
                  ~key:(object_key "ranged-callback-failed-transfer.bin")
                  ~contents:body ())
              : Object.Put.result);
          let options =
            Transfer.download_options_exn
              ~multipart_threshold:(Int64.of_int part_size) ~part_size
              ~concurrency:1 ()
          in
          let observed =
            Lwt_main.run
              (observe_lwt
                 (S3.Object.Transfer.download_file conn ~bucket
                    ~key:(object_key "ranged-callback-failed-transfer.bin")
                    ~options ~path:download_path
                    ~on_progress:(fun _event ->
                      progress_seen := true;
                      raise Download_progress_failed)
                    ()))
          in
          expect_raised_exception "ranged download progress callback"
            (function Download_progress_failed -> true | _ -> false)
            observed;
          Alcotest.(check bool) "download progress observed" true !progress_seen;
          Alcotest.(check bool)
            "download target absent after callback failure" false
            (Sys.file_exists download_path)))

let test_transfer_multipart_upload_cancellation_leaves_no_completed_object () =
  with_minio_bucket (fun conn ~bucket ->
      let path = Filename.temp_file "awskit-minio-upload-cancelled" ".bin" in
      let part_size = Transfer.min_part_size in
      let body = transfer_body (part_size + 1024) in
      let progress_seen = ref false in
      let key = object_key "cancelled-multipart-transfer.bin" in
      write_file path body;
      Fun.protect
        ~finally:(fun () -> remove_file path)
        (fun () ->
          let options =
            Transfer.upload_options_exn
              ~multipart_threshold:(Int64.of_int part_size) ~part_size
              ~concurrency:1 ()
          in
          let observed =
            Lwt_main.run
              (observe_lwt
                 (S3.Object.Transfer.multipart_upload_file conn ~bucket ~key
                    ~options ~path
                    ~on_progress:(fun _event ->
                      progress_seen := true;
                      raise Lwt.Canceled)
                    ()))
          in
          expect_raised_exception "multipart upload cancellation"
            (function Lwt.Canceled -> true | _ -> false)
            observed;
          Alcotest.(check bool)
            "cancellation progress observed" true !progress_seen;
          check_object_absent "cancelled transfer object absent" conn ~bucket
            ~key))

let transfer_suite =
  [
    ( "integration:minio:s3-state",
      [
        Alcotest.test_case "empty suffix range target-profile law" `Quick
          test_empty_suffix_range_target_profile_law;
        Alcotest.test_case "deterministic shared state transcript" `Quick
          test_state_transcript_covers_minio_profile;
      ] );
    ( "integration:minio:object",
      [
        Alcotest.test_case "metadata tags and delete" `Quick
          test_object_metadata_tags_and_delete;
        Alcotest.test_case "paginated list keys" `Quick
          test_list_pagination_is_service_backed;
      ] );
    ( "integration:minio:multipart",
      [
        Alcotest.test_case "manual multipart lifecycle" `Quick
          test_manual_multipart_lifecycle;
      ] );
    ( "integration:minio:transfer",
      [
        Alcotest.test_case "small upload/download byte roundtrip" `Quick
          test_transfer_small_roundtrip;
        Alcotest.test_case "multipart upload byte comparison" `Quick
          test_transfer_multipart_upload_bytes;
        Alcotest.test_case "ranged download byte comparison" `Quick
          test_transfer_ranged_download_bytes;
        Alcotest.test_case
          "progress callback exception aborts owned multipart upload" `Quick
          test_transfer_progress_callback_exception_aborts_owned_multipart_upload;
        Alcotest.test_case
          "ranged download callback interruption has no success" `Quick
          test_transfer_ranged_download_callback_exception_leaves_no_success;
        Alcotest.test_case
          "multipart upload cancellation leaves no completed object" `Quick
          test_transfer_multipart_upload_cancellation_leaves_no_completed_object;
      ] );
  ]

let profile_suite profile =
  [
    ( "integration:minio:profile",
      [
        Alcotest.test_case
          (Printf.sprintf "selected integration profile: %s"
             (integration_profile_to_string profile))
          `Quick
          (fun () ->
            report_selected_profile profile;
            Alcotest.(check string)
              "AWSKIT_INTEGRATION_PROFILE"
              (integration_profile_to_string profile)
              selected_integration_profile_name);
        Alcotest.test_case "MinIO target-profile laws" `Quick
          test_minio_target_profile_boundaries;
        Alcotest.test_case "empty suffix range shared model law" `Quick
          test_empty_suffix_range_shared_model_law;
      ] );
  ]

type minio_gate =
  | Available
  | Missing_config of string * Awskit_s3.Error.t
  | Setup_failed of string * Awskit_s3.Error.t

let classify_setup_error label error =
  if minio_configured then Setup_failed (label, error)
  else Missing_config (label, error)

let preflight_minio () =
  let conn = connect () in
  match Lwt_main.run (cleanup_bucket_result conn ~bucket) with
  | Error error ->
      classify_setup_error "clean MinIO bucket before workload" error
  | Ok () -> (
      match Lwt_main.run (S3.Bucket.create conn ~bucket ()) with
      | Error error -> classify_setup_error "create bucket" error
      | Ok (_ : Bucket.Create.result) -> (
          match Lwt_main.run (cleanup_bucket_result conn ~bucket) with
          | Ok () -> Available
          | Error error ->
              Setup_failed ("clean MinIO bucket after preflight", error)))

let invalid_profile_suite value =
  [
    ( "integration:minio:profile",
      [
        Alcotest.test_case "invalid integration profile" `Quick (fun () ->
            Alcotest.failf
              "invalid AWSKIT_INTEGRATION_PROFILE=%S; allowed values: %s" value
              integration_profile_allowed);
      ] );
  ]

let missing_config_suite profile label error =
  [
    ( "integration:minio:configuration",
      [
        Alcotest.test_case "missing local MinIO configuration" `Quick (fun () ->
            fail_unconfigured_minio profile label error);
      ] );
  ]

let setup_failed_suite profile label error =
  [
    ( "integration:minio:configuration",
      [
        Alcotest.test_case "configured MinIO setup fails" `Quick (fun () ->
            Alcotest.failf "%s (integration profile %s): %a" label
              (integration_profile_to_string profile)
              Awskit_s3.Error.pp error);
      ] );
  ]

let suite =
  match selected_integration_profile_result with
  | Error value -> invalid_profile_suite value
  | Ok profile -> (
      match preflight_minio () with
      | Available -> profile_suite profile @ Workload.suite @ transfer_suite
      | Missing_config (label, error) ->
          profile_suite profile @ missing_config_suite profile label error
      | Setup_failed (label, error) ->
          profile_suite profile @ setup_failed_suite profile label error)

let () = Alcotest.run "awskit-s3-minio-workload" suite
