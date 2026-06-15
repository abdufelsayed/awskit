(** Integration tests for Lwt-Unix runtime: connection creation. *)

open Base

let write_temp contents =
  let path = Stdlib.Filename.temp_file "awskit-test" ".aws" in
  let channel = Stdlib.open_out_bin path in
  Stdlib.output_string channel contents;
  Stdlib.close_out channel;
  path

let getenv_of_assoc values name =
  List.Assoc.find values name ~equal:String.equal

let check_access_key label expected credentials =
  Alcotest.(check string)
    label expected
    (Awskit.Credentials.access_key_id credentials)

let resolve_provider provider =
  Lwt_main.run (Awskit_lwt_unix.Credentials.Provider.resolve provider)

let credentials_or_fail label = function
  | Ok credentials -> credentials
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 () in
  let conn =
    match
      Awskit_lwt_unix.create ~region:"eu-west-1" ~credentials:c ~clock ~endpoint
        ()
    with
    | Ok conn -> conn
    | Error e -> Fmt.failwith "%a" Awskit.Error.pp e
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit_lwt_unix.Runtime.region conn |> Awskit.Region.to_string);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       Awskit_lwt_unix.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    match
      Awskit_lwt_unix.create ~region:"us-east-1" ~credentials:c ~clock ()
    with
    | Ok conn -> conn
    | Error e -> Fmt.failwith "%a" Awskit.Error.pp e
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map
       Awskit_lwt_unix.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_credentials_from_env () =
  let getenv =
    getenv_of_assoc
      [
        ("AWS_ACCESS_KEY_ID", "ENV_AK");
        ("AWS_SECRET_ACCESS_KEY", "ENV_SK");
        ("AWS_SESSION_TOKEN", "ENV_TOKEN");
      ]
  in
  let credentials =
    Awskit_unix.Credentials.from_env ~getenv ()
    |> credentials_or_fail "from env"
  in
  check_access_key "env access key" "ENV_AK" credentials;
  Alcotest.(check (option string))
    "env session token" (Some "ENV_TOKEN")
    (Awskit.Credentials.session_token credentials)

let test_credentials_from_shared_credentials_profile () =
  let credentials_file =
    write_temp
      {|
[default]
aws_access_key_id = DEFAULT_AK
aws_secret_access_key = DEFAULT_SK

[dev]
aws_access_key_id = DEV_AK
aws_secret_access_key = DEV_SK
aws_session_token = DEV_TOKEN
|}
  in
  let config_file = write_temp "" in
  let credentials =
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"dev" ()
    |> credentials_or_fail "from profile"
  in
  check_access_key "profile access key" "DEV_AK" credentials;
  Alcotest.(check (option string))
    "profile session token" (Some "DEV_TOKEN")
    (Awskit.Credentials.session_token credentials)

let test_credentials_from_config_profile () =
  let credentials_file = write_temp "" in
  let config_file =
    write_temp
      {|
[profile prod]
aws_access_key_id = PROD_AK
aws_secret_access_key = PROD_SK
|}
  in
  let credentials =
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"prod" ()
    |> credentials_or_fail "from config profile"
  in
  check_access_key "config profile access key" "PROD_AK" credentials

let test_default_chain_prefers_env () =
  let credentials_file =
    write_temp
      {|
[default]
aws_access_key_id = FILE_AK
aws_secret_access_key = FILE_SK
|}
  in
  let config_file = write_temp "" in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_ACCESS_KEY_ID", "ENV_AK");
        ("AWS_SECRET_ACCESS_KEY", "ENV_SK");
        ("AWS_SHARED_CREDENTIALS_FILE", credentials_file);
        ("AWS_CONFIG_FILE", config_file);
      ]
  in
  let credentials =
    Awskit_unix.Credentials.default_chain ~getenv ()
    |> credentials_or_fail "default chain"
  in
  check_access_key "preferred env access key" "ENV_AK" credentials

let test_default_chain_rejects_partial_env () =
  let credentials_file =
    write_temp
      {|
[default]
aws_access_key_id = FILE_AK
aws_secret_access_key = FILE_SK
|}
  in
  let config_file = write_temp "" in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_ACCESS_KEY_ID", "ONLY_AK");
        ("AWS_SHARED_CREDENTIALS_FILE", credentials_file);
        ("AWS_CONFIG_FILE", config_file);
      ]
  in
  match Awskit_unix.Credentials.default_chain ~getenv () with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected partial env to fail"

let metadata_json ?(access_key_id = "META_AK")
    ?(expiration = "2030-01-01T00:00:00Z") () =
  Printf.sprintf
    {|{"Code":"Success","AccessKeyId":"%s","SecretAccessKey":"META_SK","Token":"META_TOKEN","Expiration":"%s"}|}
    access_key_id expiration

let test_container_credentials_provider () =
  let calls = ref [] in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth ~headers uri =
    calls := (meth, headers, Uri.to_string uri) :: !calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"TASK_AK" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call ()
  in
  let credentials =
    resolve_provider provider |> credentials_or_fail "container"
  in
  check_access_key "container access key" "TASK_AK" credentials;
  Alcotest.(check int) "one metadata call" 1 (List.length !calls);
  match !calls with
  | [ (`GET, [], uri) ] ->
      Alcotest.(check string)
        "container uri" "http://169.254.170.2/v2/credentials/task-role" uri
  | _ -> Alcotest.fail "unexpected container metadata request"

let test_container_full_uri_rejects_untrusted_http () =
  let getenv =
    getenv_of_assoc
      [ ("AWS_CONTAINER_CREDENTIALS_FULL_URI", "http://example.com/creds") ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Alcotest.fail "metadata HTTP call should not be attempted"
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected untrusted HTTP endpoint to fail"

let test_instance_metadata_provider_uses_imdsv2 () =
  let calls = ref [] in
  let http_call ~meth ~headers uri =
    let uri_string = Uri.to_string uri in
    calls := (meth, headers, uri_string) :: !calls;
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "TOKEN";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/" ->
        Alcotest.(check (option string))
          "role request token" (Some "TOKEN")
          (List.Assoc.find headers "X-aws-ec2-metadata-token"
             ~equal:String.equal);
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "instance-role\n";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/instance-role" ->
        Alcotest.(check (option string))
          "credential request token" (Some "TOKEN")
          (List.Assoc.find headers "X-aws-ec2-metadata-token"
             ~equal:String.equal);
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = metadata_json ~access_key_id:"IMDS_AK" ();
          }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             ("unexpected request " ^ uri_string))
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  let credentials = resolve_provider provider |> credentials_or_fail "imds" in
  check_access_key "imds access key" "IMDS_AK" credentials;
  Alcotest.(check int) "imds call count" 3 (List.length !calls)

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let expect_transport label = function
  | Error error -> (
      let open Awskit.Error in
      match kind error with
      | Transport _ -> ()
      | _ ->
          Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error)
  | Ok _ -> Alcotest.failf "%s: expected transport error" label

let imds_role_headers headers =
  List.Assoc.find headers "X-aws-ec2-metadata-token" ~equal:String.equal

let test_instance_metadata_provider_falls_back_on_token_404 () =
  let calls = ref [] in
  let http_call ~meth ~headers uri =
    calls := (meth, headers, Uri.to_string uri) :: !calls;
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 404; headers = []; body = "" }
    | `GET, "/latest/meta-data/iam/security-credentials/" ->
        Alcotest.(check (option string))
          "role request tokenless" None
          (imds_role_headers headers);
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "instance-role\n";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/instance-role" ->
        Alcotest.(check (option string))
          "credential request tokenless" None
          (imds_role_headers headers);
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = metadata_json ~access_key_id:"IMDSV1_AK" ();
          }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  let credentials =
    resolve_provider provider |> credentials_or_fail "imds fallback"
  in
  check_access_key "imds fallback access key" "IMDSV1_AK" credentials;
  Alcotest.(check int) "imds fallback call count" 3 (List.length !calls)

let test_instance_metadata_provider_rejects_disabled_imdsv1_fallback () =
  let get_attempted = ref false in
  let calls = ref [] in
  let http_call ~meth ~headers:_ uri =
    calls := (meth, Uri.to_string uri) :: !calls;
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 404; headers = []; body = "" }
    | `GET, _ ->
        get_attempted := true;
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected IMDSv1 GET")
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call
      ~imdsv1_fallback:`Disabled ()
  in
  expect_validation "disabled IMDSv1 fallback" (resolve_provider provider);
  Alcotest.(check bool) "no IMDSv1 GET" false !get_attempted;
  Alcotest.(check int) "token call only" 1 (List.length !calls)

let test_instance_metadata_provider_rejects_env_disabled_imdsv1_fallback () =
  let get_attempted = ref false in
  let getenv = getenv_of_assoc [ ("AWS_EC2_METADATA_V1_DISABLED", "true") ] in
  let http_call ~meth ~headers:_ uri =
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 404; headers = []; body = "" }
    | `GET, _ ->
        get_attempted := true;
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected IMDSv1 GET")
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~getenv ~http_call ()
  in
  expect_validation "env disabled IMDSv1 fallback" (resolve_provider provider);
  Alcotest.(check bool) "no env-disabled IMDSv1 GET" false !get_attempted

let test_instance_metadata_provider_token_500_does_not_fallback () =
  let get_attempted = ref false in
  let calls = ref 0 in
  let http_call ~meth ~headers:_ uri =
    Int.incr calls;
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 500; headers = []; body = "" }
    | `GET, _ ->
        get_attempted := true;
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected IMDSv1 GET")
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_validation "token 500" (resolve_provider provider);
  Alcotest.(check bool) "no token-500 IMDSv1 GET" false !get_attempted;
  Alcotest.(check int) "token 500 call count" 1 !calls

let test_instance_metadata_provider_token_error_does_not_fallback () =
  let get_attempted = ref false in
  let calls = ref 0 in
  let http_call ~meth ~headers:_ uri =
    Int.incr calls;
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_error
          (Awskit.Error.Internal.transport ~retryable:true
             "metadata token failed")
    | `GET, _ ->
        get_attempted := true;
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected IMDSv1 GET")
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_transport "token transport error" (resolve_provider provider);
  Alcotest.(check bool) "no token-error IMDSv1 GET" false !get_attempted;
  Alcotest.(check int) "token error call count" 1 !calls

let ptime_of_rfc3339 value =
  match Ptime.of_rfc3339 ~strict:false value with
  | Ok (time, _, _) -> time
  | Error _ -> Alcotest.failf "invalid test timestamp %S" value

let test_metadata_provider_refreshes_before_expiration () =
  let now = ref Ptime.epoch in
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [ ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/credentials") ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body =
          metadata_json
            ~access_key_id:(Printf.sprintf "AK_%d" !calls)
            ~expiration:"1970-01-01T01:00:00Z" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call
      ~clock:(fun () -> !now)
      ()
  in
  let first = resolve_provider provider |> credentials_or_fail "first" in
  let second = resolve_provider provider |> credentials_or_fail "second" in
  check_access_key "cached access key" "AK_1" first;
  check_access_key "still cached access key" "AK_1" second;
  Alcotest.(check int) "cached call count" 1 !calls;
  now := ptime_of_rfc3339 "1970-01-01T00:56:00Z";
  let refreshed =
    resolve_provider provider |> credentials_or_fail "refreshed"
  in
  check_access_key "refreshed access key" "AK_2" refreshed;
  Alcotest.(check int) "refreshed call count" 2 !calls

let suite () =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "credentials from env" `Quick
          test_credentials_from_env;
        Alcotest.test_case "credentials from shared profile" `Quick
          test_credentials_from_shared_credentials_profile;
        Alcotest.test_case "credentials from config profile" `Quick
          test_credentials_from_config_profile;
        Alcotest.test_case "default chain prefers env" `Quick
          test_default_chain_prefers_env;
        Alcotest.test_case "default chain rejects partial env" `Quick
          test_default_chain_rejects_partial_env;
        Alcotest.test_case "container credentials provider" `Quick
          test_container_credentials_provider;
        Alcotest.test_case "container full uri rejects untrusted http" `Quick
          test_container_full_uri_rejects_untrusted_http;
        Alcotest.test_case "instance metadata provider uses imdsv2" `Quick
          test_instance_metadata_provider_uses_imdsv2;
        Alcotest.test_case "instance metadata falls back on token 404" `Quick
          test_instance_metadata_provider_falls_back_on_token_404;
        Alcotest.test_case "instance metadata rejects disabled imdsv1 fallback"
          `Quick
          test_instance_metadata_provider_rejects_disabled_imdsv1_fallback;
        Alcotest.test_case "instance metadata rejects env-disabled imdsv1"
          `Quick
          test_instance_metadata_provider_rejects_env_disabled_imdsv1_fallback;
        Alcotest.test_case "instance metadata token 500 does not fallback"
          `Quick test_instance_metadata_provider_token_500_does_not_fallback;
        Alcotest.test_case "instance metadata token error does not fallback"
          `Quick test_instance_metadata_provider_token_error_does_not_fallback;
        Alcotest.test_case "metadata provider refreshes before expiration"
          `Quick test_metadata_provider_refreshes_before_expiration;
      ] );
  ]
