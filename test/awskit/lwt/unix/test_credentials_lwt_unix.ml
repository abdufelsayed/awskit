module Credentials = Awskit_lwt_unix.Credentials
module Provider = Credentials.Provider

type call = {
  meth : Cohttp.Code.meth;
  headers : (string * string) list;
  uri : Uri.t;
}

let getenv values name = List.assoc_opt name values

let time value =
  match Ptime.of_rfc3339 ~strict:false value with
  | Ok (time, _, _) -> time
  | Error _ -> Alcotest.failf "invalid test time %S" value

let metadata_body ~access_key_id ~expiration =
  Printf.sprintf
    {|{"Code":"Success","AccessKeyId":%S,"SecretAccessKey":"SECRET","Token":"TOKEN","Expiration":%S}|}
    access_key_id expiration

let response ?(status = 200) body : Credentials.http_response =
  { status; headers = []; body }

let scripted_http responses =
  let pending = ref responses in
  let calls = ref [] in
  let http_call ~meth ~headers uri =
    calls := { meth; headers; uri } :: !calls;
    match !pending with
    | [] -> Alcotest.fail "metadata HTTP script exhausted"
    | result :: rest ->
        pending := rest;
        Lwt.return result
  in
  (http_call, fun () -> List.rev !calls)

let resolve provider = Provider.resolve provider |> Lwt_main.run

let resolved_credentials = function
  | Provider.Resolved credentials -> credentials
  | Unavailable unavailable ->
      Alcotest.failf "provider unavailable: %s" unavailable.reason
  | Invalid error | Failed error ->
      Alcotest.failf "provider failed: %a" Awskit.Error.pp error

let test_container_policy_and_cache_refresh () =
  let invalid_provider =
    Credentials.container_provider
      ~getenv:
        (getenv
           [
             ( "AWS_CONTAINER_CREDENTIALS_FULL_URI",
               "http://credentials.example.test/creds" );
           ])
      ()
  in
  (match resolve invalid_provider with
  | Provider.Invalid error ->
      Alcotest.(check (option string))
        "unsafe HTTP field is redacted" (Some "<redacted>")
        (Awskit.Error.validation_field error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "unsafe container HTTP endpoint must be invalid");
  let now = ref (time "2026-07-11T00:00:00Z") in
  let http_call, calls =
    scripted_http
      [
        Ok
          (response
             (metadata_body ~access_key_id:"AKID-1"
                ~expiration:"2026-07-11T00:10:00Z"));
        Ok
          (response
             (metadata_body ~access_key_id:"AKID-2"
                ~expiration:"2026-07-11T00:20:00Z"));
      ]
  in
  let provider =
    Credentials.container_provider
      ~getenv:
        (getenv
           [
             ("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials");
             ("AWS_CONTAINER_AUTHORIZATION_TOKEN", "Bearer test-token");
           ])
      ~http_call
      ~clock:(fun () -> !now)
      ()
  in
  let first = resolve provider |> resolved_credentials in
  let cached = resolve provider |> resolved_credentials in
  Alcotest.(check string)
    "first access key" "AKID-1"
    (Awskit.Credentials.access_key_id first);
  Alcotest.(check string)
    "cached access key" "AKID-1"
    (Awskit.Credentials.access_key_id cached);
  Alcotest.(check int) "one cached request" 1 (List.length (calls ()));
  now := time "2026-07-11T00:06:00Z";
  let refreshed = resolve provider |> resolved_credentials in
  Alcotest.(check string)
    "refreshed access key" "AKID-2"
    (Awskit.Credentials.access_key_id refreshed);
  match calls () with
  | [ first_call; _second_call ] ->
      Alcotest.(check string)
        "relative endpoint" "http://169.254.170.2/v2/credentials"
        (Uri.to_string first_call.uri);
      Alcotest.(check (option string))
        "authorization header" (Some "Bearer test-token")
        (List.assoc_opt "Authorization" first_call.headers)
  | calls ->
      Alcotest.failf "expected two metadata requests, got %d"
        (List.length calls)

let test_invalid_container_metadata_is_invalid () =
  let http_call, _calls = scripted_http [ Ok (response "not-json") ] in
  let provider =
    Credentials.container_provider
      ~getenv:
        (getenv
           [
             ( "AWS_CONTAINER_CREDENTIALS_FULL_URI",
               "http://127.0.0.1/credentials" );
           ])
      ~http_call ()
  in
  match resolve provider with
  | Provider.Invalid error ->
      Alcotest.(check bool)
        "metadata parse error is validation" true
        (Awskit.Error.is_validation error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "invalid metadata JSON must be invalid"

let test_imdsv2_and_fallback_policy () =
  let calls = ref [] in
  let http_call ~meth ~headers uri =
    calls := { meth; headers; uri } :: !calls;
    let path = Uri.path uri in
    let result =
      match (meth, path) with
      | `PUT, "/latest/api/token" -> Ok (response "imds-token")
      | `GET, "/latest/meta-data/iam/security-credentials/" ->
          Ok (response "demo-role\n")
      | `GET, "/latest/meta-data/iam/security-credentials/demo-role" ->
          Ok
            (response
               (metadata_body ~access_key_id:"IMDS-AKID"
                  ~expiration:"2026-07-11T01:00:00Z"))
      | _ ->
          Alcotest.failf "unexpected IMDS call %s %s"
            (Cohttp.Code.string_of_method meth)
            path
    in
    Lwt.return result
  in
  let provider =
    Credentials.instance_metadata_provider ~http_call
      ~clock:(fun () -> time "2026-07-11T00:00:00Z")
      ()
  in
  let credentials = resolve provider |> resolved_credentials in
  Alcotest.(check string)
    "IMDS access key" "IMDS-AKID"
    (Awskit.Credentials.access_key_id credentials);
  let calls = List.rev !calls in
  Alcotest.(check int) "token, role, credentials" 3 (List.length calls);
  List.iter
    (fun call ->
      if call.meth = `GET then
        Alcotest.(check (option string))
          "IMDSv2 token header" (Some "imds-token")
          (List.assoc_opt "X-aws-ec2-metadata-token" call.headers))
    calls;
  let token_missing_calls = ref 0 in
  let token_missing ~meth:_ ~headers:_ _uri =
    incr token_missing_calls;
    Lwt.return_ok (response ~status:404 "")
  in
  let disabled =
    Credentials.instance_metadata_provider ~http_call:token_missing
      ~imdsv1_fallback:`Disabled ()
  in
  (match resolve disabled with
  | Provider.Invalid error ->
      Alcotest.(check (option string))
        "fallback policy field" (Some "AWS_EC2_METADATA_V1_DISABLED")
        (Awskit.Error.validation_field error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "disabled IMDSv1 fallback must be invalid");
  Alcotest.(check int) "token request only" 1 !token_missing_calls;
  let env_disabled_calls = ref 0 in
  let env_disabled_token ~meth:_ ~headers:_ _uri =
    incr env_disabled_calls;
    Lwt.return_ok (response ~status:404 "")
  in
  let env_disabled =
    Credentials.instance_metadata_provider
      ~getenv:(getenv [ ("AWS_EC2_METADATA_V1_DISABLED", "true") ])
      ~http_call:env_disabled_token ()
  in
  (match resolve env_disabled with
  | Provider.Invalid _ -> ()
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "environment must disable IMDSv1 fallback");
  Alcotest.(check int) "environment token request only" 1 !env_disabled_calls;
  let metadata_disabled_calls = ref 0 in
  let metadata_disabled_http ~meth:_ ~headers:_ _uri =
    incr metadata_disabled_calls;
    Lwt.return_ok (response "unexpected")
  in
  let metadata_disabled =
    Credentials.instance_metadata_provider
      ~getenv:(getenv [ ("AWS_EC2_METADATA_DISABLED", "true") ])
      ~http_call:metadata_disabled_http ()
  in
  (match resolve metadata_disabled with
  | Provider.Unavailable _ -> ()
  | Resolved _ | Invalid _ | Failed _ ->
      Alcotest.fail "disabled IMDS must be unavailable");
  Alcotest.(check int)
    "disabled IMDS makes no request" 0 !metadata_disabled_calls

let install_never_transport started =
  Cohttp_lwt_unix.Client.set_cache
    (fun ?headers:_ ?body:_ ?absolute_form:_ _meth _uri ->
      started := true;
      fst (Lwt.wait ()))

let loopback_container_provider () =
  Credentials.container_provider
    ~getenv:
      (getenv
         [
           ("AWS_CONTAINER_CREDENTIALS_FULL_URI", "http://127.0.0.1/credentials");
         ])
    ()

let test_default_metadata_timeout () =
  let started = ref false in
  install_never_transport started;
  match resolve (loopback_container_provider ()) with
  | Provider.Failed error ->
      Alcotest.(check bool) "transport started" true !started;
      Alcotest.(check bool)
        "metadata timeout" true
        (Awskit.Error.is_timeout error)
  | Resolved _ | Unavailable _ | Invalid _ ->
      Alcotest.fail "hanging metadata request must time out"

let test_default_metadata_cancellation () =
  let started = ref false in
  install_never_transport started;
  let canceled =
    Lwt_main.run
      (let promise = Provider.resolve (loopback_container_provider ()) in
       Lwt.bind (Lwt.pause ()) (fun () ->
           Lwt.cancel promise;
           Lwt.catch
             (fun () -> Lwt.map (fun _ -> false) promise)
             (function Lwt.Canceled -> Lwt.return true | exn -> Lwt.fail exn)))
  in
  Alcotest.(check bool) "transport started" true !started;
  Alcotest.(check bool) "native Lwt cancellation" true canceled

let () =
  Alcotest.run "awskit-lwt-unix-credentials"
    [
      ( "unit:awskit-lwt-unix:credentials",
        [
          Alcotest.test_case "container policy and cache refresh" `Quick
            test_container_policy_and_cache_refresh;
          Alcotest.test_case "invalid container metadata" `Quick
            test_invalid_container_metadata_is_invalid;
          Alcotest.test_case "IMDSv2 and fallback policy" `Quick
            test_imdsv2_and_fallback_policy;
          Alcotest.test_case "default metadata timeout" `Slow
            test_default_metadata_timeout;
          Alcotest.test_case "default metadata cancellation" `Quick
            test_default_metadata_cancellation;
        ] );
    ]
