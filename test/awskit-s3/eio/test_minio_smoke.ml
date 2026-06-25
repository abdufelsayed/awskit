module S3 = Awskit_s3_eio
module Metadata = Awskit_s3.Metadata
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key
module Range = Awskit_s3.Range
module Tag = Awskit_s3.Tag

let bucket_of_string value = Awskit_s3.Bucket_name.of_string_exn value
let object_key value = Awskit_s3.Object_key.of_string_exn value

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

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let endpoint_config () =
  let endpoint =
    Awskit.Endpoint.of_string endpoint |> ok_or_fail "minio endpoint"
  in
  let signing_region = Awskit.Region.of_string_exn region in
  match Awskit.Endpoint.scheme endpoint with
  | `Https ->
      Alcotest.fail
        "Eio MinIO smoke uses Awskit_eio.http_only; use an http:// MinIO \
         endpoint or add a TLS connector before enabling https:// coverage"
  | `Http when unsafe_http = "1" ->
      Awskit_s3.Endpoint_config.unsafe_plaintext ~endpoint ~signing_region
        ~addressing_style:`Path ()
  | `Http ->
      Awskit_s3.Endpoint_config.local_plaintext ~endpoint ~signing_region
        ~addressing_style:`Path ()
      |> ok_or_fail "minio endpoint policy"

let connect env ~sw =
  S3.create ~sw ~env ~https:Awskit_eio.http_only ~region ~credentials
    ~endpoint_config:(endpoint_config ()) ()
  |> ok_or_fail "connect"

let bucket_name_string () =
  Printf.sprintf "awskit-minio-eio-%d-smoke" (Unix.getpid ())

let ignore_absent label result =
  match result with
  | Ok _ -> ()
  | Error error
    when Awskit_s3.Error.is_no_such_key error
         || Awskit_s3.Error.is_no_such_bucket error ->
      ()
  | Error error ->
      Format.eprintf "MinIO smoke cleanup %s failed: %a@." label
        Awskit_s3.Error.pp error

let cleanup_step label f =
  match f () with
  | result -> ignore_absent label result
  | exception exn ->
      Format.eprintf "MinIO smoke cleanup %s raised: %s@." label
        (Printexc.to_string exn)

let cleanup conn ~bucket ~keys =
  List.iter
    (fun key ->
      cleanup_step
        (Printf.sprintf "object delete %s" (Object_key.to_string key))
        (fun () -> S3.Object.delete conn ~bucket ~key ()))
    keys;
  cleanup_step "bucket delete" (fun () -> S3.Bucket.delete conn ~bucket ())

let with_bucket conn f =
  let bucket = bucket_of_string (bucket_name_string ()) in
  let key = object_key "smoke.txt" in
  let copy_key = object_key "smoke-copy.txt" in
  let missing_key = object_key "missing.txt" in
  let keys = [ key; copy_key ] in
  cleanup conn ~bucket ~keys;
  ignore (S3.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  match f ~bucket ~key ~copy_key ~missing_key with
  | value ->
      cleanup conn ~bucket ~keys;
      value
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      cleanup conn ~bucket ~keys;
      Printexc.raise_with_backtrace exn backtrace

let compare_pair (left_key, left_value) (right_key, right_value) =
  match String.compare left_key right_key with
  | 0 -> String.compare left_value right_value
  | order -> order

let sorted_pairs pairs = List.sort compare_pair pairs

let check_metadata label expected actual =
  Alcotest.(check (list (pair string string)))
    label
    (Metadata.to_list expected |> sorted_pairs)
    (Metadata.to_list actual |> sorted_pairs)

let tag_pair tag = (Tag.key tag, Tag.value tag)
let tag_pairs tags = List.map tag_pair (Tag.Set.to_list tags)

let check_tags label expected actual =
  Alcotest.(check (list (pair string string)))
    label
    (tag_pairs expected |> sorted_pairs)
    (tag_pairs actual |> sorted_pairs)

let check_response_success label response =
  Alcotest.(check bool) label true (Awskit.Response.is_success response)

let expect_not_found_status label error =
  Alcotest.(check bool)
    (label ^ " not found") true
    (Awskit_s3.Error.is_not_found error);
  Alcotest.(check (option int))
    (label ^ " status") (Some 404)
    (Awskit.Error.service_status error)

let expect_optional_service_code label ~allowed error =
  match Awskit_s3.Error.service_code error with
  | None -> ()
  | Some code
    when List.exists (fun expected -> String.equal expected code) allowed ->
      ()
  | Some code ->
      Alcotest.failf "%s: unexpected service code %S in %a" label code
        Awskit_s3.Error.pp error

let expect_missing_head conn ~bucket ~key =
  match S3.Object.head conn ~bucket ~key () with
  | Error error ->
      expect_not_found_status "missing head" error;
      expect_optional_service_code "missing head"
        ~allowed:[ "NoSuchKey"; "NotFound" ]
        error
  | Ok _ -> Alcotest.fail "missing head: expected not-found service error"

let expect_missing_get conn ~bucket ~key =
  match S3.Object.get_string conn ~bucket ~key ~max_bytes:32L () with
  | Error error ->
      expect_not_found_status "missing get" error;
      Alcotest.(check bool)
        "missing get code" true
        (Awskit_s3.Error.is_no_such_key error)
  | Ok _ -> Alcotest.fail "missing get: expected no-such-key service error"

let test_object_smoke env () =
  Eio.Switch.run @@ fun sw ->
  let conn = connect env ~sw in
  with_bucket conn @@ fun ~bucket ~key ~copy_key ~missing_key ->
  let body = "abcdefghij" in
  let metadata =
    Metadata.of_list_exn
      [ ("suite", "minio-eio-smoke"); ("shape", "metadata-copy") ]
  in
  let put_options = Object.Put.options_exn ~metadata () in
  ignore
    (S3.Object.put_string conn ~bucket ~key ~options:put_options ~contents:body
       ()
    |> ok_or_fail "put object");
  let head = S3.Object.head conn ~bucket ~key () |> ok_or_fail "head object" in
  Alcotest.(check (option int64))
    "head content length"
    (Some (Int64.of_int (String.length body)))
    head.content_length;
  check_metadata "head metadata" metadata head.metadata;
  let got =
    S3.Object.get_string conn ~bucket ~key ~max_bytes:32L ()
    |> ok_or_fail "get object"
  in
  Alcotest.(check string) "get body" body got.Object.Get.value;
  check_metadata "get metadata" metadata got.Object.Get.metadata;
  let range_options =
    {
      Object.Get.default_options with
      range = Some (Range.bytes_exn ~start:2L ~finish:5L);
    }
  in
  let range =
    S3.Object.get_string conn ~bucket ~key ~options:range_options ~max_bytes:16L
      ()
    |> ok_or_fail "range get"
  in
  Alcotest.(check string) "range body" "cdef" range.Object.Get.value;
  let tags =
    Tag.Set.of_list_exn
      [
        Tag.create_exn ~key:"suite" ~value:"minio-eio-smoke";
        Tag.create_exn ~key:"adapter" ~value:"eio";
      ]
  in
  S3.Object.Tagging.put conn ~bucket ~key ~tags ()
  |> ok_or_fail "put object tags"
  |> check_response_success "put object tags response";
  let got_tags =
    S3.Object.Tagging.get conn ~bucket ~key () |> ok_or_fail "get object tags"
  in
  check_tags "get object tags" tags got_tags.Object.Tagging.tags;
  S3.Object.Tagging.delete conn ~bucket ~key ()
  |> ok_or_fail "delete object tags"
  |> check_response_success "delete object tags response";
  let cleared_tags =
    S3.Object.Tagging.get conn ~bucket ~key ()
    |> ok_or_fail "get cleared object tags"
  in
  check_tags "cleared object tags" Tag.Set.empty
    cleared_tags.Object.Tagging.tags;
  let copy_options = Object.Copy.options_exn ~metadata_directive:`Copy () in
  ignore
    (S3.Object.copy conn ~source_bucket:bucket ~source_key:key
       ~destination_bucket:bucket ~destination_key:copy_key
       ~options:copy_options ()
    |> ok_or_fail "copy object");
  let copied =
    S3.Object.get_string conn ~bucket ~key:copy_key ~max_bytes:32L ()
    |> ok_or_fail "get copied object"
  in
  Alcotest.(check string) "copied body" body copied.Object.Get.value;
  check_metadata "copied metadata" metadata copied.Object.Get.metadata;
  expect_missing_head conn ~bucket ~key:missing_key;
  expect_missing_get conn ~bucket ~key:missing_key;
  ignore (S3.Object.delete conn ~bucket ~key () |> ok_or_fail "delete object");
  ignore
    (S3.Object.delete conn ~bucket ~key () |> ok_or_fail "delete object again");
  expect_missing_head conn ~bucket ~key;
  ignore
    (S3.Object.delete conn ~bucket ~key:copy_key ()
    |> ok_or_fail "delete copied object");
  ignore (S3.Bucket.delete conn ~bucket () |> ok_or_fail "delete bucket")

let suite env =
  [
    ( "integration:awskit-s3-eio:minio-smoke",
      [
        Alcotest.test_case "object create read range delete" `Quick
          (test_object_smoke env);
      ] );
  ]
