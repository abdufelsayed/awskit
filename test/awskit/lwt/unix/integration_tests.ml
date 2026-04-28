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
  | Error (Awskit.Error.Validation _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected partial env to fail"

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
      ] );
  ]
