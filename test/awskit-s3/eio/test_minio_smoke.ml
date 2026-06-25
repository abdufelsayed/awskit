module S3 = Awskit_s3_eio
module Object = Awskit_s3.Object
module Range = Awskit_s3.Range

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

let cleanup conn ~bucket ~key =
  ignore_absent "object delete" (S3.Object.delete conn ~bucket ~key ());
  ignore_absent "bucket delete" (S3.Bucket.delete conn ~bucket ())

let with_bucket conn f =
  let bucket = bucket_of_string (bucket_name_string ()) in
  let key = object_key "smoke.txt" in
  ignore_absent "preflight object delete"
    (S3.Object.delete conn ~bucket ~key ());
  ignore_absent "preflight bucket delete" (S3.Bucket.delete conn ~bucket ());
  ignore (S3.Bucket.create conn ~bucket () |> ok_or_fail "create bucket");
  match f ~bucket ~key with
  | value ->
      cleanup conn ~bucket ~key;
      value
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      cleanup conn ~bucket ~key;
      Printexc.raise_with_backtrace exn backtrace

let test_object_smoke env () =
  Eio.Switch.run @@ fun sw ->
  let conn = connect env ~sw in
  with_bucket conn @@ fun ~bucket ~key ->
  let body = "abcdefghij" in
  ignore
    (S3.Object.put_string conn ~bucket ~key ~contents:body ()
    |> ok_or_fail "put object");
  let head = S3.Object.head conn ~bucket ~key () |> ok_or_fail "head object" in
  Alcotest.(check (option int64))
    "head content length"
    (Some (Int64.of_int (String.length body)))
    head.content_length;
  let got =
    S3.Object.get_string conn ~bucket ~key ~max_bytes:32L ()
    |> ok_or_fail "get object"
  in
  Alcotest.(check string) "get body" body got.Object.Get.value;
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
  ignore (S3.Object.delete conn ~bucket ~key () |> ok_or_fail "delete object");
  ignore (S3.Bucket.delete conn ~bucket () |> ok_or_fail "delete bucket")

let suite env =
  [
    ( "integration:awskit-s3-eio:minio-smoke",
      [
        Alcotest.test_case "object create read range delete" `Quick
          (test_object_smoke env);
      ] );
  ]
