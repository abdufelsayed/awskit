(** Integration tests for Lwt-Unix runtime: connection creation. *)

open Base

let write_temp contents =
  let path = Stdlib.Filename.temp_file "awskit-test" ".aws" in
  let channel = Stdlib.open_out_bin path in
  Stdlib.output_string channel contents;
  Stdlib.close_out channel;
  path

let make_temp_dir () =
  let path = Stdlib.Filename.temp_file "awskit-test" ".dir" in
  Stdlib.Sys.remove path;
  Unix.mkdir path 0o700;
  path

let getenv_of_assoc values name =
  List.Assoc.find values name ~equal:String.equal

let check_access_key label expected credentials =
  Alcotest.(check string)
    label expected
    (Awskit.Credentials.access_key_id credentials)

let check_source_label label expected credentials =
  Alcotest.(check (option string))
    label (Some expected)
    (Awskit.Credentials.source_label credentials)

let check_source_variant label expected credentials =
  Alcotest.(check (option bool))
    label (Some true)
    (Option.map (Awskit.Credentials.source credentials) ~f:(function
      | source when Poly.equal source expected -> true
      | _ -> false))

let check_expiration label expected credentials =
  Alcotest.(check (option bool))
    label (Some true)
    (Option.map
       (Awskit.Credentials.expires_at credentials)
       ~f:(Ptime.equal expected))

let resolve_provider provider =
  Lwt_main.run (Awskit_lwt_unix.Credentials.Provider.resolve provider)

let provider_source_to_string = function
  | `Static -> "static"
  | `Env -> "env"
  | `Shared_file path -> path
  | `Config_file path -> path
  | `Container -> "container"
  | `Imds -> "imds"
  | `Custom source -> source

let provider_resolution_to_result resolution =
  let open Awskit_lwt_unix.Credentials.Provider in
  match resolution with
  | Resolved credentials -> Ok credentials
  | Unavailable { source; reason } ->
      Error
        (Awskit.Error.Internal.credentials
           ~source:(provider_source_to_string source)
           reason)
  | Invalid error | Failed error -> Error error

let resolve_provider_result provider =
  resolve_provider provider |> provider_resolution_to_result

let credentials_or_fail label = function
  | Ok credentials -> credentials
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let ptime_of_rfc3339 value =
  match Ptime.of_rfc3339 ~strict:false value with
  | Ok (time, _, _) -> time
  | Error _ -> Alcotest.failf "invalid test timestamp %S" value

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = "http://localhost:9000" in
  let region = "eu-west-1" in
  let conn =
    match Awskit_lwt_unix.create ~region ~credentials:c ~clock ~endpoint () with
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
  let region = "us-east-1" in
  let conn =
    match Awskit_lwt_unix.create ~region ~credentials:c ~clock () with
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
  check_source_label "env source" "environment" credentials;
  check_source_variant "env source variant" `Env credentials;
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
  check_source_label "profile source" credentials_file credentials;
  check_source_variant "profile source variant" (`Shared_file credentials_file)
    credentials;
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
  check_access_key "config profile access key" "PROD_AK" credentials;
  check_source_label "config profile source" config_file credentials;
  check_source_variant "config profile source variant"
    (`Config_file config_file) credentials

let test_credentials_from_profile_prefers_shared_file_static_credentials () =
  let credentials_file =
    write_temp
      {|
[both]
aws_access_key_id = SHARED_AK
aws_secret_access_key = SHARED_SK
|}
  in
  let config_file =
    write_temp
      {|
[profile both]
aws_access_key_id = CONFIG_AK
aws_secret_access_key = CONFIG_SK
|}
  in
  let credentials =
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"both" ()
    |> credentials_or_fail "from merged profile"
  in
  check_access_key "merged profile access key" "SHARED_AK" credentials;
  check_source_label "merged profile source" credentials_file credentials;
  check_source_variant "merged profile source variant"
    (`Shared_file credentials_file) credentials

let test_credentials_from_profile_prefers_shared_static_over_config_role () =
  let credentials_file =
    write_temp
      {|
[dev]
aws_access_key_id = SHARED_STATIC_AK
aws_secret_access_key = SHARED_STATIC_SK
|}
  in
  let config_file =
    write_temp
      {|
[profile dev]
role_arn = arn:aws:iam::123456789012:role/dev
source_profile = base
|}
  in
  let credentials =
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"dev" ()
    |> credentials_or_fail "from shared static with config role"
  in
  check_access_key "shared static access key" "SHARED_STATIC_AK" credentials;
  check_source_label "shared static source" credentials_file credentials;
  check_source_variant "shared static source variant"
    (`Shared_file credentials_file) credentials

let test_credentials_from_profile_rejects_split_static_credentials () =
  let credentials_file =
    write_temp {|
[split]
aws_access_key_id = SPLIT_AK
|}
  in
  let config_file =
    write_temp {|
[profile split]
aws_secret_access_key = SPLIT_SK
|}
  in
  match
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"split" ()
  with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok credentials ->
      Alcotest.failf "unexpected split profile credentials: %s"
        (Awskit.Credentials.access_key_id credentials)

let test_credentials_from_profile_rejects_partial_shared_static_before_config ()
    =
  let credentials_file =
    write_temp {|
[partial]
aws_access_key_id = PARTIAL_SHARED_AK
|}
  in
  let config_file =
    write_temp
      {|
[profile partial]
aws_access_key_id = CONFIG_AK
aws_secret_access_key = CONFIG_SK
|}
  in
  match
    Awskit_unix.Credentials.from_profile ~credentials_file ~config_file
      ~profile:"partial" ()
  with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok credentials ->
      Alcotest.failf "unexpected config fallback credentials: %s"
        (Awskit.Credentials.access_key_id credentials)

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

let metadata_json_without_expiration ?(access_key_id = "META_AK") () =
  Printf.sprintf
    {|{"Code":"Success","AccessKeyId":"%s","SecretAccessKey":"META_SK","Token":"META_TOKEN"}|}
    access_key_id

let test_default_lwt_unix_provider_continues_from_unavailable_local_to_container
    () =
  let home = make_temp_dir () in
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [
        ("HOME", home);
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"CHAIN_CONTAINER_AK" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.default_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Resolved credentials ->
      check_access_key "container fallback access key" "CHAIN_CONTAINER_AK"
        credentials;
      Alcotest.(check int) "container call count" 1 !calls
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Invalid error | Failed error ->
      Alcotest.failf "unexpected credential error: %a" Awskit.Error.pp error

let test_default_lwt_unix_provider_home_without_files_continues_to_container ()
    =
  let home = make_temp_dir () in
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"HOME_CONTAINER_AK" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.default_provider ~getenv ~home ~http_call ()
  in
  match resolve_provider provider with
  | Resolved credentials ->
      check_access_key "container fallback access key" "HOME_CONTAINER_AK"
        credentials;
      Alcotest.(check int) "container call count" 1 !calls
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Invalid error | Failed error ->
      Alcotest.failf "unexpected credential error: %a" Awskit.Error.pp error

let test_default_lwt_unix_provider_stops_on_partial_env () =
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_ACCESS_KEY_ID", "ONLY_AK");
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"SHOULD_NOT_USE" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.default_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Invalid error ->
      Alcotest.(check bool)
        "partial env validation" true
        (Awskit.Error.is_validation error);
      Alcotest.(check int) "metadata not attempted" 0 !calls
  | Resolved credentials ->
      Alcotest.failf "unexpected credentials: %s"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Failed error ->
      Alcotest.failf "unexpected credential failure: %a" Awskit.Error.pp error

let test_default_lwt_unix_provider_stops_on_session_token_only_env () =
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_SESSION_TOKEN", "TOKEN_ONLY");
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"SHOULD_NOT_USE" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.default_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Invalid error ->
      Alcotest.(check bool)
        "session token only validation" true
        (Awskit.Error.is_validation error);
      Alcotest.(check int) "metadata not attempted" 0 !calls
  | Resolved credentials ->
      Alcotest.failf "unexpected credentials: %s"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Failed error ->
      Alcotest.failf "unexpected credential failure: %a" Awskit.Error.pp error

let test_default_lwt_unix_provider_stops_on_explicit_empty_profile_files () =
  let credentials_file = write_temp "" in
  let config_file = write_temp "" in
  let calls = ref 0 in
  let getenv =
    getenv_of_assoc
      [
        ("AWS_SHARED_CREDENTIALS_FILE", credentials_file);
        ("AWS_CONFIG_FILE", config_file);
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Int.incr calls;
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json ~access_key_id:"SHOULD_NOT_USE" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.default_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Invalid error ->
      Alcotest.(check bool)
        "explicit profile file validation" true
        (Awskit.Error.is_validation error);
      Alcotest.(check int) "metadata not attempted" 0 !calls
  | Resolved credentials ->
      Alcotest.failf "unexpected credentials: %s"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Failed error ->
      Alcotest.failf "unexpected credential failure: %a" Awskit.Error.pp error

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
    resolve_provider_result provider |> credentials_or_fail "container"
  in
  check_access_key "container access key" "TASK_AK" credentials;
  check_source_label "container source" "container" credentials;
  check_source_variant "container source variant" `Container credentials;
  check_expiration "container expiration"
    (ptime_of_rfc3339 "2030-01-01T00:00:00Z")
    credentials;
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
  match resolve_provider_result provider with
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected untrusted HTTP endpoint to fail"

let expect_failed_resolution label = function
  | Awskit_lwt_unix.Credentials.Provider.Failed error ->
      Alcotest.(check bool)
        (label ^ " validation error")
        true
        (Awskit.Error.is_validation error)
  | Resolved credentials ->
      Alcotest.failf "%s: unexpected credentials: %s" label
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "%s: unexpected unavailable credentials from %s: %s" label
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Invalid error ->
      Alcotest.failf "%s: expected failed, got invalid: %a" label
        Awskit.Error.pp error

let test_container_metadata_http_500_fails_provider () =
  let getenv =
    getenv_of_assoc
      [
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Lwt.return_ok
      { Awskit_lwt_unix.Credentials.status = 500; headers = []; body = "" }
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call ()
  in
  expect_failed_resolution "container HTTP 500" (resolve_provider provider)

let test_container_metadata_missing_expiration_is_invalid () =
  let getenv =
    getenv_of_assoc
      [
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body = metadata_json_without_expiration ~access_key_id:"NO_EXP_AK" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call ()
  in
  match resolve_provider provider with
  | Invalid error when Awskit.Error.is_validation error -> ()
  | Invalid error ->
      Alcotest.failf "unexpected invalid error: %a" Awskit.Error.pp error
  | Resolved credentials ->
      Alcotest.failf "unexpected credentials without expiration: %s"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "unexpected unavailable credentials from %s: %s"
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Failed error ->
      Alcotest.failf "expected invalid, got failed: %a" Awskit.Error.pp error

let expect_invalid_resolution label = function
  | Awskit_lwt_unix.Credentials.Provider.Invalid error ->
      Alcotest.(check bool)
        (label ^ " validation error")
        true
        (Awskit.Error.is_validation error)
  | Resolved credentials ->
      Alcotest.failf "%s: unexpected credentials: %s" label
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable unavailable ->
      Alcotest.failf "%s: unexpected unavailable credentials from %s: %s" label
        (provider_source_to_string unavailable.source)
        unavailable.reason
  | Failed error ->
      Alcotest.failf "%s: expected invalid, got failed: %a" label
        Awskit.Error.pp error

let test_container_metadata_malformed_expiration_is_invalid () =
  let getenv =
    getenv_of_assoc
      [
        ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/task-role");
      ]
  in
  let http_call ~meth:_ ~headers:_ _uri =
    Lwt.return_ok
      {
        Awskit_lwt_unix.Credentials.status = 200;
        headers = [];
        body =
          metadata_json ~access_key_id:"BAD_EXP_AK" ~expiration:"not-a-time" ();
      }
  in
  let provider =
    Awskit_lwt_unix.Credentials.container_provider ~getenv ~http_call ()
  in
  expect_invalid_resolution "container malformed expiration"
    (resolve_provider provider)

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
  let credentials =
    resolve_provider_result provider |> credentials_or_fail "imds"
  in
  check_access_key "imds access key" "IMDS_AK" credentials;
  check_source_label "imds source" "instance metadata" credentials;
  check_source_variant "imds source variant" `Imds credentials;
  check_expiration "imds expiration"
    (ptime_of_rfc3339 "2030-01-01T00:00:00Z")
    credentials;
  Alcotest.(check int) "imds call count" 3 (List.length !calls)

let test_instance_metadata_missing_expiration_is_invalid () =
  let http_call ~meth ~headers:_ uri =
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "TOKEN";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "instance-role\n";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/instance-role" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body =
              metadata_json_without_expiration ~access_key_id:"IMDS_NO_EXP_AK"
                ();
          }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_invalid_resolution "IMDS missing expiration"
    (resolve_provider provider)

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let test_create_rejects_invalid_region_string () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  expect_validation "invalid region"
    (Awskit_lwt_unix.create ~region:"" ~credentials
       ~clock:(fun () -> Ptime.epoch)
       ())

let test_create_rejects_invalid_endpoint_string () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  expect_validation "invalid endpoint"
    (Awskit_lwt_unix.create ~region:"us-east-1"
       ~endpoint:"http://localhost:9000/path" ~credentials
       ~clock:(fun () -> Ptime.epoch)
       ())

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

let test_instance_metadata_token_http_500_fails_provider () =
  let http_call ~meth ~headers:_ uri =
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 500; headers = []; body = "" }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_failed_resolution "IMDS token HTTP 500" (resolve_provider provider)

let test_instance_metadata_role_http_500_fails_provider () =
  let http_call ~meth ~headers:_ uri =
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "TOKEN";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 500; headers = []; body = "" }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_failed_resolution "IMDS role HTTP 500" (resolve_provider provider)

let test_instance_metadata_credentials_http_500_fails_provider () =
  let http_call ~meth ~headers:_ uri =
    match (meth, Uri.path uri) with
    | `PUT, "/latest/api/token" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "TOKEN";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/" ->
        Lwt.return_ok
          {
            Awskit_lwt_unix.Credentials.status = 200;
            headers = [];
            body = "instance-role\n";
          }
    | `GET, "/latest/meta-data/iam/security-credentials/instance-role" ->
        Lwt.return_ok
          { Awskit_lwt_unix.Credentials.status = 500; headers = []; body = "" }
    | _ ->
        Lwt.return_error
          (Awskit.Error.Internal.validation ~field:"metadata"
             "unexpected request")
  in
  let provider =
    Awskit_lwt_unix.Credentials.instance_metadata_provider ~http_call ()
  in
  expect_failed_resolution "IMDS credentials HTTP 500"
    (resolve_provider provider)

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
    resolve_provider_result provider |> credentials_or_fail "imds fallback"
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
  expect_validation "disabled IMDSv1 fallback"
    (resolve_provider_result provider);
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
  expect_validation "env disabled IMDSv1 fallback"
    (resolve_provider_result provider);
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
  expect_validation "token 500" (resolve_provider_result provider);
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
  expect_transport "token transport error" (resolve_provider_result provider);
  Alcotest.(check bool) "no token-error IMDSv1 GET" false !get_attempted;
  Alcotest.(check int) "token error call count" 1 !calls

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
  let first = resolve_provider_result provider |> credentials_or_fail "first" in
  let second =
    resolve_provider_result provider |> credentials_or_fail "second"
  in
  check_access_key "cached access key" "AK_1" first;
  check_access_key "still cached access key" "AK_1" second;
  Alcotest.(check int) "cached call count" 1 !calls;
  now := ptime_of_rfc3339 "1970-01-01T00:56:00Z";
  let refreshed =
    resolve_provider_result provider |> credentials_or_fail "refreshed"
  in
  check_access_key "refreshed access key" "AK_2" refreshed;
  Alcotest.(check int) "refreshed call count" 2 !calls

let suite () =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "rejects invalid region string" `Quick
          test_create_rejects_invalid_region_string;
        Alcotest.test_case "rejects invalid endpoint string" `Quick
          test_create_rejects_invalid_endpoint_string;
        Alcotest.test_case "credentials from env" `Quick
          test_credentials_from_env;
        Alcotest.test_case "credentials from shared profile" `Quick
          test_credentials_from_shared_credentials_profile;
        Alcotest.test_case "credentials from config profile" `Quick
          test_credentials_from_config_profile;
        Alcotest.test_case "credentials from merged profile prefer shared file"
          `Quick
          test_credentials_from_profile_prefers_shared_file_static_credentials;
        Alcotest.test_case "credentials prefer shared static over config role"
          `Quick
          test_credentials_from_profile_prefers_shared_static_over_config_role;
        Alcotest.test_case "credentials reject split profile static material"
          `Quick test_credentials_from_profile_rejects_split_static_credentials;
        Alcotest.test_case
          "credentials reject partial shared static before config" `Quick
          test_credentials_from_profile_rejects_partial_shared_static_before_config;
        Alcotest.test_case "default chain prefers env" `Quick
          test_default_chain_prefers_env;
        Alcotest.test_case "default chain rejects partial env" `Quick
          test_default_chain_rejects_partial_env;
        Alcotest.test_case "default provider continues past unavailable local"
          `Quick
          test_default_lwt_unix_provider_continues_from_unavailable_local_to_container;
        Alcotest.test_case "default provider injected home falls through" `Quick
          test_default_lwt_unix_provider_home_without_files_continues_to_container;
        Alcotest.test_case "default provider stops on partial env" `Quick
          test_default_lwt_unix_provider_stops_on_partial_env;
        Alcotest.test_case "default provider stops on session token only env"
          `Quick test_default_lwt_unix_provider_stops_on_session_token_only_env;
        Alcotest.test_case "default provider stops on explicit empty profiles"
          `Quick
          test_default_lwt_unix_provider_stops_on_explicit_empty_profile_files;
        Alcotest.test_case "container credentials provider" `Quick
          test_container_credentials_provider;
        Alcotest.test_case "container full uri rejects untrusted http" `Quick
          test_container_full_uri_rejects_untrusted_http;
        Alcotest.test_case "container metadata HTTP 500 fails provider" `Quick
          test_container_metadata_http_500_fails_provider;
        Alcotest.test_case "container metadata missing expiration is invalid"
          `Quick test_container_metadata_missing_expiration_is_invalid;
        Alcotest.test_case "container metadata malformed expiration is invalid"
          `Quick test_container_metadata_malformed_expiration_is_invalid;
        Alcotest.test_case "instance metadata provider uses imdsv2" `Quick
          test_instance_metadata_provider_uses_imdsv2;
        Alcotest.test_case "instance metadata missing expiration is invalid"
          `Quick test_instance_metadata_missing_expiration_is_invalid;
        Alcotest.test_case "instance metadata token HTTP 500 fails provider"
          `Quick test_instance_metadata_token_http_500_fails_provider;
        Alcotest.test_case "instance metadata role HTTP 500 fails provider"
          `Quick test_instance_metadata_role_http_500_fails_provider;
        Alcotest.test_case
          "instance metadata credentials HTTP 500 fails provider" `Quick
          test_instance_metadata_credentials_http_500_fails_provider;
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
