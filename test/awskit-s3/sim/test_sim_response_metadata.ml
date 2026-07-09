open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let enable_versioning conn =
  ignore
    (Simulator.Bucket.Versioning.put conn
       ~bucket:(bucket_name "test-bucket")
       ~status:Bucket.Versioning.Status.Enabled ()
    |> ok_or_fail "enable versioning")

let service_error label = function
  | Error error -> (
      match Awskit.Error.kind error with
      | Service service -> service
      | _ ->
          Alcotest.failf "%s: expected service error: %a" label Awskit.Error.pp
            error)
  | Ok _ -> Alcotest.failf "%s: expected service error" label

let service_header service name =
  List.find_map
    (fun (key, value) ->
      if String.equal (String.lowercase_ascii key) (String.lowercase_ascii name)
      then Some value
      else None)
    service.Awskit.Error.headers

let check_delete_marker_405 label marker_version service =
  Alcotest.(check int) (label ^ " status") 405 service.Awskit.Error.status;
  Alcotest.(check (option string))
    (label ^ " code") (Some "MethodNotAllowed") service.code;
  Alcotest.(check (option string))
    (label ^ " delete marker header")
    (Some "true")
    (service_header service "x-amz-delete-marker");
  Alcotest.(check (option string))
    (label ^ " version header")
    (Some (Object.Version_id.to_string marker_version))
    (service_header service "x-amz-version-id");
  Alcotest.(check (option string))
    (label ^ " last modified")
    (Some (Ptime.to_rfc3339 test_time))
    (service_header service "last-modified")

let test_delete_marker_specific_version_has_last_modified () =
  let conn = make_simulator () in
  enable_versioning conn;
  ignore (put_string conn "deleted.txt" "body");
  let deleted =
    Simulator.Object.delete conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "deleted.txt") ()
    |> ok_or_fail "delete current"
  in
  let marker_version =
    match deleted.version_id with
    | Some version_id -> version_id
    | None -> Alcotest.fail "delete marker did not return a version id"
  in
  let get_options = Object.Get.options ~version_id:marker_version () in
  let head_options = Object.Head.options ~version_id:marker_version () in
  let get_service =
    service_error "get delete marker"
      (Simulator.Object.get_string conn
         ~bucket:(bucket_name "test-bucket")
         ~key:(object_key "deleted.txt") ~options:get_options ~max_bytes:1024L
         ())
  in
  let head_service =
    service_error "head delete marker"
      (Simulator.Object.head conn
         ~bucket:(bucket_name "test-bucket")
         ~key:(object_key "deleted.txt") ~options:head_options ())
  in
  check_delete_marker_405 "get" marker_version get_service;
  check_delete_marker_405 "head" marker_version head_service

let test_ranged_get_accept_ranges_header () =
  let conn = make_simulator () in
  ignore (put_string conn "range.txt" "abcdef");
  let options =
    Object.Get.options ~range:(Range.bytes_exn ~start:1L ~finish:3L) ()
  in
  let result =
    Simulator.Object.get_string conn
      ~bucket:(bucket_name "test-bucket")
      ~key:(object_key "range.txt") ~options ~max_bytes:1024L ()
    |> ok_or_fail "ranged get"
  in
  Alcotest.(check string) "body" "bcd" result.value;
  Alcotest.(check int) "status" 206 (Awskit.Response.status result.response);
  Alcotest.(check (option string))
    "content range" (Some "bytes 1-3/6")
    (Awskit.Response.header result.response "content-range");
  Alcotest.(check (option string))
    "accept ranges" (Some "bytes")
    (Awskit.Response.header result.response "accept-ranges")

let suite =
  [
    ( "contract:awskit-s3-sim:response-metadata",
      [
        Alcotest.test_case "delete marker 405 last modified" `Quick
          test_delete_marker_specific_version_has_last_modified;
        Alcotest.test_case "ranged get accept ranges" `Quick
          test_ranged_get_accept_ranges_header;
      ] );
  ]
