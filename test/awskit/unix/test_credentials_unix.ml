module Credentials = Awskit_unix.Credentials
module Provider = Awskit.Credentials.Provider

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let getenv values name = List.assoc_opt name values

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let with_profile_files ~credentials ~config f =
  let credentials_file = Filename.temp_file "awskit-credentials" ".ini" in
  let config_file = Filename.temp_file "awskit-config" ".ini" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove credentials_file;
      Sys.remove config_file)
    (fun () ->
      write_file credentials_file credentials;
      write_file config_file config;
      f ~credentials_file ~config_file)

let test_environment_credentials () =
  let getenv =
    getenv
      [
        ("AWS_ACCESS_KEY_ID", "ENV-AKID");
        ("AWS_SECRET_ACCESS_KEY", "ENV-SECRET");
        ("AWS_SESSION_TOKEN", "ENV-TOKEN");
      ]
  in
  let credentials =
    Credentials.from_env ~getenv () |> ok_or_fail "environment credentials"
  in
  Alcotest.(check string)
    "access key" "ENV-AKID"
    (Awskit.Credentials.access_key_id credentials);
  Alcotest.(check (option string))
    "session token" (Some "ENV-TOKEN")
    (Awskit.Credentials.reveal_session_token credentials);
  Alcotest.(check (option string))
    "source" (Some "environment")
    (Awskit.Credentials.source_label credentials)

let test_profile_precedence_and_config_sections () =
  with_profile_files
    ~credentials:
      "[dev]\n\
       aws_access_key_id = FILE-AKID\n\
       aws_secret_access_key = FILE-SECRET\n\
       aws_session_token = FILE-TOKEN\n"
    ~config:
      "[profile dev]\n\
       aws_access_key_id = CONFIG-AKID\n\
       aws_secret_access_key = CONFIG-SECRET\n"
    (fun ~credentials_file ~config_file ->
      let credentials =
        Credentials.from_profile ~credentials_file ~config_file ~profile:"dev"
          ()
        |> ok_or_fail "shared profile credentials"
      in
      Alcotest.(check string)
        "credentials file wins" "FILE-AKID"
        (Awskit.Credentials.access_key_id credentials);
      Alcotest.(check (option string))
        "credentials source" (Some credentials_file)
        (Awskit.Credentials.source_label credentials));
  with_profile_files ~credentials:""
    ~config:
      "[profile dev]\n\
       aws_access_key_id = CONFIG-AKID\n\
       aws_secret_access_key = CONFIG-SECRET\n"
    (fun ~credentials_file ~config_file ->
      let credentials =
        Credentials.from_profile ~credentials_file ~config_file ~profile:"dev"
          ()
        |> ok_or_fail "config profile credentials"
      in
      Alcotest.(check string)
        "config profile section" "CONFIG-AKID"
        (Awskit.Credentials.access_key_id credentials);
      Alcotest.(check (option string))
        "config source" (Some config_file)
        (Awskit.Credentials.source_label credentials))

let test_partial_environment_stops_profile_fallback () =
  with_profile_files
    ~credentials:
      "[default]\n\
       aws_access_key_id = FILE-AKID\n\
       aws_secret_access_key = FILE-SECRET\n" ~config:""
    (fun ~credentials_file ~config_file ->
      let getenv =
        getenv
          [
            ("AWS_ACCESS_KEY_ID", "PARTIAL");
            ("AWS_SHARED_CREDENTIALS_FILE", credentials_file);
            ("AWS_CONFIG_FILE", config_file);
          ]
      in
      match Credentials.default_chain ~getenv () with
      | Ok _ -> Alcotest.fail "partial environment must stop provider chain"
      | Error error ->
          Alcotest.(check (option string))
            "sensitive environment field redacted" (Some "<redacted>")
            (Awskit.Error.validation_field error))

let test_invalid_profile_shapes () =
  with_profile_files ~credentials:"[dev]\naws_access_key_id = PARTIAL\n"
    ~config:"" (fun ~credentials_file ~config_file ->
      match
        Credentials.from_profile ~credentials_file ~config_file ~profile:"dev"
          ()
      with
      | Ok _ -> Alcotest.fail "partial profile must be rejected"
      | Error error ->
          Alcotest.(check (option string))
            "partial profile field" (Some "AWS_PROFILE")
            (Awskit.Error.validation_field error));
  with_profile_files ~credentials:""
    ~config:
      "[profile dev]\n\
       role_arn = arn:aws:iam::123456789012:role/demo\n\
       source_profile = source\n" (fun ~credentials_file ~config_file ->
      match
        Credentials.from_profile ~credentials_file ~config_file ~profile:"dev"
          ()
      with
      | Ok _ -> Alcotest.fail "assume-role profile must be unsupported"
      | Error error ->
          Alcotest.(check bool)
            "assume-role error is validation" true
            (Awskit.Error.is_validation error))

let test_unconfigured_provider_is_unavailable () =
  let home = Filename.temp_file "awskit-empty-home" "" in
  Sys.remove home;
  let provider =
    Credentials.default_provider ~getenv:(fun _ -> None) ~home ()
  in
  match Provider.resolve provider with
  | Provider.Unavailable unavailable ->
      Alcotest.(check string)
        "unavailable source" "default"
        (Provider.source_label unavailable.source)
  | Resolved _ | Invalid _ | Failed _ ->
      Alcotest.fail "unconfigured local provider must be unavailable"

let () =
  Alcotest.run "awskit-unix-credentials"
    [
      ( "unit:awskit-unix:credentials",
        [
          Alcotest.test_case "environment credentials" `Quick
            test_environment_credentials;
          Alcotest.test_case "profile precedence and config sections" `Quick
            test_profile_precedence_and_config_sections;
          Alcotest.test_case "partial environment stops profile fallback" `Quick
            test_partial_environment_stops_profile_fallback;
          Alcotest.test_case "invalid profile shapes" `Quick
            test_invalid_profile_shapes;
          Alcotest.test_case "unconfigured provider is unavailable" `Quick
            test_unconfigured_provider_is_unavailable;
        ] );
    ]
