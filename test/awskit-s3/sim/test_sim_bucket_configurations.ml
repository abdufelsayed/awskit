open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let test_encryption_put_get_projection () =
  let conn = make_simulator () in
  let default =
    Bucket.Encryption.Default_encryption.sse_kms_exn ~key_id:"key-id"
      ~bucket_key_enabled:true ()
  in
  let config =
    Bucket.Encryption.Config.singleton
      (Bucket.Encryption.Rule.Default_and_sse_c
         {
           default_encryption = default;
           sse_c_policy = Bucket.Encryption.Sse_c_policy.Block;
         })
  in
  ignore
    (Simulator.Bucket.Encryption.put conn
       ~bucket:(bucket_name "test-bucket")
       ~config ()
    |> ok_or_fail "put encryption");
  let result =
    Simulator.Bucket.Encryption.get conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "get encryption"
  in
  match result.config.rules with
  | [ rule ] ->
      let default =
        match rule.default_encryption with
        | Some default -> default
        | None -> Alcotest.fail "expected default encryption"
      in
      Alcotest.(check (option string))
        "algorithm" (Some "aws:kms")
        (Option.map Bucket.Encryption.Observed.Algorithm.to_string
           default.algorithm);
      Alcotest.(check (option string))
        "key id" (Some "key-id") default.kms_key_id;
      Alcotest.(check (option bool))
        "bucket key" (Some true) rule.bucket_key_enabled;
      Alcotest.(check (list string))
        "SSE-C policy" [ "SSE-C" ]
        (List.map Bucket.Encryption.Observed.Sse_c_policy.to_string
           rule.sse_c_policies)
  | _ -> Alcotest.fail "expected one encryption rule"

let test_cors_put_get_projection () =
  let conn = make_simulator () in
  let rule =
    Bucket.Cors.Rule.create_exn ~id:"browser"
      ~allowed_origins:[ "https://example.com" ]
      ~allowed_methods:[ Bucket.Cors.Method.Get; Put ]
      ~max_age_seconds:60 ()
  in
  let config = Bucket.Cors.Config.singleton rule in
  ignore
    (Simulator.Bucket.Cors.put conn
       ~bucket:(bucket_name "test-bucket")
       ~config ()
    |> ok_or_fail "put CORS");
  let result =
    Simulator.Bucket.Cors.get conn ~bucket:(bucket_name "test-bucket") ()
    |> ok_or_fail "get CORS"
  in
  match result.config.rules with
  | [ observed ] ->
      Alcotest.(check (option string)) "id" (Some "browser") observed.id;
      Alcotest.(check (list string))
        "origins" [ "https://example.com" ] observed.allowed_origins;
      Alcotest.(check (list string))
        "methods" [ "GET"; "PUT" ]
        (List.map Bucket.Cors.Method.observed_to_string observed.allowed_methods);
      Alcotest.(check (option int)) "max age" (Some 60) observed.max_age_seconds
  | _ -> Alcotest.fail "expected one CORS rule"

let suite =
  [
    ( "contract:awskit-s3-sim:bucket-configurations",
      [
        Alcotest.test_case "encryption put/get projection" `Quick
          test_encryption_put_get_projection;
        Alcotest.test_case "CORS put/get projection" `Quick
          test_cors_put_get_projection;
      ] );
  ]
