let redacted = "<redacted>"

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

let check_contains label substring text =
  Alcotest.(check bool) label true (contains text substring)

let check_absent label substrings text =
  List.iter
    (fun substring ->
      Alcotest.(check bool)
        (Printf.sprintf "%s redacts %s" label substring)
        false (contains text substring))
    substrings

let check_validation_error label = function
  | Ok _ -> Alcotest.failf "%s should fail validation" label
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s returned unexpected error: %a" label Awskit.Error.pp
        error

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let test_time =
  match Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) with
  | Some time -> time
  | None -> invalid_arg "invalid test time"

let span seconds =
  match Ptime.Span.of_float_s seconds with
  | Some span -> span
  | None -> invalid_arg "invalid test span"

let has_operation_context ~name error =
  List.exists
    (function
      | Awskit.Error.Operation operation -> String.equal operation.name name
      | Awskit.Error.Message _ | Awskit.Error.Retry _ | Awskit.Error.Sexp _ ->
          false)
    (Awskit.Error.context error)

let count_from_env ~default =
  match Sys.getenv_opt "AWSKIT_QCHECK_COUNT" with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let retry_error = Awskit.Error.Producer.transport ~retryable:true "temporary"

let prop_retry_jitter_stays_within_policy_bounds =
  let base_delay = Ptime.Span.of_int_s 1 in
  let max_delay = Ptime.Span.of_int_s 4 in
  let policy =
    Awskit.Retry.create_exn ~max_attempts:10 ~base_delay ~max_delay ~jitter:1.0
      ()
  in
  QCheck.Test.make
    ~count:(count_from_env ~default:200)
    ~name:"retry jitter stays within configured delay bounds"
    (QCheck.make
       ~print:(fun (attempt, sample) ->
         Printf.sprintf "attempt=%d sample=%d" attempt sample)
       QCheck.Gen.(pair (int_range 1 8) (int_range 0 1000)))
    (fun (attempt, sample) ->
      let random_float ~upper_bound = upper_bound *. (float sample /. 1000.0) in
      match
        Awskit.Retry.delay policy ~attempt ~error:retry_error ~random_float
      with
      | None -> false
      | Some delay ->
          let seconds = Ptime.Span.to_float_s delay in
          seconds >= 0.0 && seconds <= 4.0)

type retry_schedule_error =
  | Retryable_transport
  | Throttled_status
  | Throttled_code
  | Fatal_transport
  | Timeout_error
  | Cancelled_error

let retry_schedule_error_to_string = function
  | Retryable_transport -> "retryable-transport"
  | Throttled_status -> "throttled-status"
  | Throttled_code -> "throttled-code"
  | Fatal_transport -> "fatal-transport"
  | Timeout_error -> "timeout"
  | Cancelled_error -> "cancelled"

let retry_schedule_error = function
  | Retryable_transport ->
      Awskit.Error.Producer.transport ~retryable:true "temporary"
  | Throttled_status -> Awskit.Error.Producer.service ~status:429 ~headers:[] ()
  | Throttled_code ->
      Awskit.Error.Producer.service ~status:400 ~code:"SlowDown" ~headers:[] ()
  | Fatal_transport -> Awskit.Error.Producer.transport ~retryable:false "fatal"
  | Timeout_error -> Awskit.Error.Producer.timeout "attempt timed out"
  | Cancelled_error -> Awskit.Error.Producer.cancelled ~reason:"caller" ()

let retry_schedule_retries = function
  | Retryable_transport | Throttled_status | Throttled_code | Timeout_error ->
      true
  | Fatal_transport | Cancelled_error -> false

type retry_schedule_case = {
  max_attempts : int;
  attempt : int;
  jitter_per_mille : int;
  sample_per_mille : int;
  error_case : retry_schedule_error;
}

let retry_schedule_case_to_string case =
  Printf.sprintf "max_attempts=%d attempt=%d jitter=%d sample=%d error=%s"
    case.max_attempts case.attempt case.jitter_per_mille case.sample_per_mille
    (retry_schedule_error_to_string case.error_case)

let shrink_retry_schedule_error = function
  | Retryable_transport -> QCheck.Iter.empty
  | Throttled_status | Throttled_code | Timeout_error ->
      QCheck.Iter.return Retryable_transport
  | Fatal_transport -> QCheck.Iter.empty
  | Cancelled_error -> QCheck.Iter.return Fatal_transport

let shrink_retry_schedule_case case =
  let candidates =
    [
      { case with max_attempts = 1 };
      { case with attempt = 0 };
      { case with attempt = 1 };
      { case with jitter_per_mille = 0 };
      { case with jitter_per_mille = 1000 };
      { case with sample_per_mille = 0 };
      { case with sample_per_mille = 1000 };
    ]
  in
  let scalar_shrinks =
    candidates
    |> List.filter (fun candidate -> not (candidate = case))
    |> QCheck.Iter.of_list
  in
  QCheck.Iter.append scalar_shrinks
    (QCheck.Iter.map
       (fun error_case -> { case with error_case })
       (shrink_retry_schedule_error case.error_case))

let clamp_float ~lower ~upper value = Float.max lower (Float.min upper value)

let retry_schedule_expected_seconds ~max_attempts ~attempt ~jitter
    ~sample_per_mille error_case =
  if
    attempt < 1
    || attempt >= max_attempts
    || not (retry_schedule_retries error_case)
  then None
  else
    let base_seconds = 0.25 in
    let max_seconds = 4.0 in
    let exponent = max 0 (attempt - 1) in
    let capped =
      Float.min max_seconds (base_seconds *. (2. ** float_of_int exponent))
    in
    let sampled =
      capped *. (float_of_int sample_per_mille /. 1000.0)
      |> clamp_float ~lower:0.0 ~upper:capped
    in
    Some ((capped *. (1.0 -. jitter)) +. (jitter *. sampled))

let prop_retry_schedule_obeys_attempt_error_and_jitter =
  let base_delay = span 0.25 in
  let max_delay = span 4.0 in
  let error_case_gen =
    QCheck.Gen.oneof_list
      [
        Retryable_transport;
        Throttled_status;
        Throttled_code;
        Fatal_transport;
        Timeout_error;
        Cancelled_error;
      ]
  in
  let gen =
    let open QCheck.Gen in
    map
      (fun ( (max_attempts, attempt, jitter_per_mille, sample_per_mille),
             error_case ) ->
        {
          max_attempts;
          attempt;
          jitter_per_mille;
          sample_per_mille;
          error_case;
        })
      (pair
         (quad (int_range 1 8) (int_range 0 10) (int_range 0 1000)
            (int_range (-500) 1500))
         error_case_gen)
  in
  QCheck.Test.make ~count:(count_from_env ~default:300)
    ~name:"retry schedules obey attempt limits, error class, and jitter"
    (QCheck.make ~print:retry_schedule_case_to_string
       ~shrink:shrink_retry_schedule_case gen) (fun case ->
      let jitter = float_of_int case.jitter_per_mille /. 1000.0 in
      let policy =
        Awskit.Retry.create_exn ~max_attempts:case.max_attempts ~base_delay
          ~max_delay ~jitter ()
      in
      let random_float ~upper_bound =
        upper_bound *. (float_of_int case.sample_per_mille /. 1000.0)
      in
      let expected =
        retry_schedule_expected_seconds ~max_attempts:case.max_attempts
          ~attempt:case.attempt ~jitter ~sample_per_mille:case.sample_per_mille
          case.error_case
      in
      let actual =
        Awskit.Retry.delay policy ~attempt:case.attempt
          ~error:(retry_schedule_error case.error_case)
          ~random_float
      in
      match (expected, actual) with
      | None, None -> true
      | Some expected, Some actual ->
          abs_float (Ptime.Span.to_float_s actual -. expected) <= 0.000_001
      | None, Some _ | Some _, None -> false)

let raw_headers =
  [
    ( "Authorization",
      "AWS4-HMAC-SHA256 Credential=AKIA/20260408/us-east-1/s3/aws4_request, \
       Signature=SECRET" );
    ("Cookie", "session=session-cookie-value");
    ("X-Amz-Security-Token", "SESSION");
    ("X-Amz-Credential", "AKIA/20260408/us-east-1/s3/aws4_request");
    ("X-Amz-Signature", "abc");
    ("x-safe-header", "safe-value");
  ]

let raw_body =
  String.concat " "
    [
      "raw-service-body";
      "SECRET";
      "SESSION";
      "X-Amz-Signature=abc";
      "Authorization: AWS4-HMAC-SHA256";
      "session-cookie-value";
      "X-Amz-Credential";
      "X-Amz-Security-Token";
    ]

let redaction_sentinels =
  [
    "SECRET";
    "SESSION";
    "X-Amz-Signature=abc";
    "Authorization: AWS4-HMAC-SHA256";
    "session-cookie-value";
    "X-Amz-Credential";
    "X-Amz-Security-Token";
    "raw-service-body";
  ]

let make_service_error () =
  Awskit.Error.Producer.service ~status:403 ~code:"AccessDenied"
    ~message:"Authorization: AWS4-HMAC-SHA256 failed" ~request_id:"request-1"
    ~host_id:"host-1" ~headers:raw_headers ~body:raw_body ()

let make_nested_error () =
  Awskit.Error.Producer.multiple
    [
      make_service_error ();
      Awskit.Error.Producer.validation ~field:"AWS_SECRET_ACCESS_KEY"
        "missing configured secret"
      |> Awskit.Error.Producer.with_context "context contains SESSION"
      |> Awskit.Error.Producer.with_sexp_context
           (Base.Sexp.List
              [
                Base.Sexp.List
                  [ Base.Sexp.Atom "X-Amz-Credential"; Base.Sexp.Atom "SECRET" ];
                Base.Sexp.List
                  [ Base.Sexp.Atom "safe"; Base.Sexp.Atom "SESSION" ];
              ]);
    ]

let format pp value = Format.asprintf "%a" pp value

let service_text service =
  let body = Option.to_list service.Awskit.Error.body in
  let headers =
    List.map (fun (name, value) -> name ^ ": " ^ value) service.headers
  in
  String.concat " " (body @ headers)

let context_text context =
  Base.Sexp.to_string_hum (Awskit.Error.sexp_of_context context)

let test_public_diagnostics_redact_secret_material () =
  let direct = make_service_error () in
  let nested = make_nested_error () in
  let public_outputs =
    [
      ("pp", format Awskit.Error.pp nested);
      ("to_string_hum", Awskit.Error.to_string_hum nested);
      ("pp_sexp", format Awskit.Error.pp_sexp nested);
      ("to_sexp_string_hum", Awskit.Error.to_sexp_string_hum nested);
      ("sexp_of_t", Awskit.Error.sexp_of_t nested |> Base.Sexp.to_string_hum);
      ( "sexp_of_kind",
        Awskit.Error.kind nested
        |> Awskit.Error.sexp_of_kind
        |> Base.Sexp.to_string_hum );
    ]
  in
  List.iter
    (fun (label, text) -> check_absent label redaction_sentinels text)
    public_outputs;
  (match Awskit.Error.kind direct with
  | Service service ->
      check_absent "public service view" redaction_sentinels
        (service_text service);
      check_contains "service body marker" redacted
        (Option.value ~default:"" service.body);
      Alcotest.(check bool)
        "safe header survives" true
        (List.exists
           (fun (name, value) ->
             String.equal name "x-safe-header"
             && String.equal value "safe-value")
           service.headers)
  | _ -> Alcotest.fail "expected service error");
  List.iter
    (fun context ->
      check_absent "public context view" redaction_sentinels
        (context_text context))
    (Awskit.Error.context nested);
  let exception_message =
    try Awskit.Error.Producer.raise nested with exn -> Printexc.to_string exn
  in
  check_absent "exception message" redaction_sentinels exception_message

let test_unsafe_diagnostics_preserve_raw_material () =
  let direct = make_service_error () in
  let nested = make_nested_error () in
  Alcotest.(check (option (list (pair string string))))
    "raw service headers" (Some raw_headers)
    (Awskit.Error.Unsafe_diagnostics.service_headers direct);
  Alcotest.(check (option string))
    "raw service body" (Some raw_body)
    (Awskit.Error.Unsafe_diagnostics.service_body direct);
  let unsafe_sexp =
    Awskit.Error.Unsafe_diagnostics.to_sexp_unredacted nested
    |> Base.Sexp.to_string_hum
  in
  List.iter
    (fun sentinel ->
      check_contains ("unsafe preserves " ^ sentinel) sentinel unsafe_sexp)
    redaction_sentinels

let test_prefixed_raw_body_is_always_replaced () =
  let prefixed_raw_body =
    redacted ^ " SECRET X-Amz-Signature=abc X-Amz-Security-Token"
  in
  let error =
    Awskit.Error.Producer.service ~status:500 ~code:"InternalError"
      ~headers:[ ("x-safe-header", "safe-value") ]
      ~body:prefixed_raw_body ()
  in
  (match Awskit.Error.kind error with
  | Service service ->
      let expected_body =
        Printf.sprintf "%s:%d bytes" redacted (String.length prefixed_raw_body)
      in
      Alcotest.(check (option string))
        "public service body" (Some expected_body) service.body;
      check_absent "prefixed body" redaction_sentinels (service_text service)
  | _ -> Alcotest.fail "expected service error");
  Alcotest.(check (option string))
    "unsafe body" (Some prefixed_raw_body)
    (Awskit.Error.Unsafe_diagnostics.service_body error)

let test_validation_field_redacts_secret_field_names () =
  let error =
    Awskit.Error.Producer.multiple
      [
        Awskit.Error.Producer.body "transport consumed response";
        Awskit.Error.Producer.validation ~field:"AWS_SESSION_TOKEN"
          "missing configured session token";
      ]
  in
  Alcotest.(check (option string))
    "validation field" (Some redacted)
    (Awskit.Error.validation_field error)

let test_credentials_metadata_and_provider () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~source:(`Custom "unit-test")
      ~expires_at:test_time ()
  in
  Alcotest.(check string)
    "access key" "AKID"
    (Awskit.Credentials.access_key_id credentials);
  Alcotest.(check (option string))
    "source label" (Some "unit-test")
    (Awskit.Credentials.source_label credentials);
  Alcotest.(check (option bool))
    "expiration" (Some true)
    (Option.map (Ptime.equal test_time)
       (Awskit.Credentials.expires_at credentials));
  let provider = Awskit.Credentials.Provider.static credentials in
  match Awskit.Credentials.Provider.resolve provider with
  | Resolved resolved ->
      Alcotest.(check (option string))
        "resolved source label" (Some "unit-test")
        (Awskit.Credentials.source_label resolved);
      Alcotest.(check (option bool))
        "resolved expiration" (Some true)
        (Option.map (Ptime.equal test_time)
           (Awskit.Credentials.expires_at resolved))
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "static provider should resolve credentials"

let test_signing_sensitive_handoff () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~session_token:"SESSION" ()
  in
  Alcotest.(check (option string))
    "explicit session token reveal" (Some "SESSION")
    (Awskit.Credentials.reveal_session_token credentials);
  let signed =
    Awskit.Signing.sign_request_params ~credentials
      ~region:(Awskit.Region.of_string_exn "us-east-1")
      ~service:"s3" ~method_:`GET ~path:"/object" ~query_params:[]
      ~headers:[ ("host", "bucket.s3.us-east-1.amazonaws.com") ]
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "")
      ~now:test_time
  in
  match signed with
  | Error error ->
      Alcotest.failf "signing should succeed: %a" Awskit.Error.pp error
  | Ok signed ->
      Alcotest.(check bool)
        "signed names include security token" true
        (List.mem "x-amz-security-token"
           (Awskit.Signing.signed_header_names signed));
      let headers = Awskit.Signing.reveal_headers signed in
      Alcotest.(check (option string))
        "revealed token header" (Some "SESSION")
        (List.assoc_opt "x-amz-security-token" headers);
      Alcotest.(check bool)
        "revealed authorization header" true
        (List.mem_assoc "authorization" headers)

let test_static_provider_annotates_absent_source () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~expires_at:test_time ()
  in
  let provider = Awskit.Credentials.Provider.static credentials in
  match Awskit.Credentials.Provider.resolve provider with
  | Resolved resolved ->
      Alcotest.(check (option string))
        "source label" (Some "static")
        (Awskit.Credentials.source_label resolved);
      Alcotest.(check (option bool))
        "source variant" (Some true)
        (Option.map
           (function `Static -> true | _ -> false)
           (Awskit.Credentials.source resolved))
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "static provider should resolve credentials"

let test_credentials_provider_chain_and_expiration () =
  let unavailable_calls = ref 0 in
  let unavailable =
    Awskit.Credentials.Provider.create (fun () ->
        incr unavailable_calls;
        Unavailable { source = `Env; reason = "not configured" })
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~source:(`Shared_file "credentials") ()
  in
  let chain =
    Awskit.Credentials.Provider.chain
      [ unavailable; Awskit.Credentials.Provider.static credentials ]
  in
  (match Awskit.Credentials.Provider.resolve chain with
  | Resolved resolved ->
      Alcotest.(check string)
        "chain resolved access key" "AKID"
        (Awskit.Credentials.access_key_id resolved);
      Alcotest.(check (option string))
        "chain preserves source label" (Some "credentials")
        (Awskit.Credentials.source_label resolved)
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "chain should resolve after unavailable provider");
  Alcotest.(check int) "unavailable provider was consulted" 1 !unavailable_calls;
  let later_provider_called = ref false in
  let invalid_error =
    Awskit.Error.Producer.credentials "bad credentials file"
  in
  let invalid_provider =
    Awskit.Credentials.Provider.create (fun () -> Invalid invalid_error)
  in
  let later_provider =
    Awskit.Credentials.Provider.create (fun () ->
        later_provider_called := true;
        Resolved credentials)
  in
  (match
     Awskit.Credentials.Provider.resolve
       (Awskit.Credentials.Provider.chain
          [ unavailable; invalid_provider; later_provider ])
   with
  | Invalid error ->
      Alcotest.(check bool)
        "invalid error is credentials error" true
        (Awskit.Error.is_credentials error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "invalid provider should stop chain resolution");
  Alcotest.(check bool)
    "chain stops on invalid credentials" false !later_provider_called;
  let expired =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~source:`Env ~expires_at:test_time ()
  in
  match
    Awskit.Credentials.validate_usable ~now:test_time ~operation:"ListBuckets"
      expired
  with
  | Error error ->
      Alcotest.(check bool)
        "expired credentials classify as credentials" true
        (Awskit.Error.is_credentials error);
      Alcotest.(check bool)
        "expiration carries operation context" true
        (has_operation_context ~name:"ListBuckets" error)
  | Ok () -> Alcotest.fail "expired credentials should be unusable"

let test_endpoint_and_request_target_invariants () =
  let endpoint = Awskit.Endpoint.of_string_exn "http://[::1]:9000" in
  Alcotest.(check string)
    "endpoint scheme" "http"
    (Awskit.Endpoint.Scheme.to_string (Awskit.Endpoint.scheme endpoint));
  Alcotest.(check string) "endpoint host" "::1" (Awskit.Endpoint.host endpoint);
  Alcotest.(check (option int))
    "endpoint port" (Some 9000)
    (Awskit.Endpoint.port endpoint);
  Alcotest.(check string)
    "endpoint authority" "[::1]:9000"
    (Awskit.Endpoint.authority endpoint);
  Alcotest.(check string)
    "endpoint prefix" "http://[::1]:9000"
    (Awskit.Endpoint.to_url_prefix endpoint);
  check_validation_error "endpoint zero port"
    (Awskit.Endpoint.http ~host:"localhost" ~port:0 ());
  check_validation_error "endpoint high port"
    (Awskit.Endpoint.http ~host:"localhost" ~port:65_536 ());
  check_validation_error "endpoint path is rejected"
    (Awskit.Endpoint.of_string "https://example.com/path");
  check_validation_error "endpoint userinfo is rejected"
    (Awskit.Endpoint.of_string "https://user@example.com");
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Https ~host:"example.com"
      ~path:"/objects/a%20b"
      ~query:[ ("x", [ "1"; "2" ]); ("empty", [ "" ]); ("flag", []) ]
      ()
  in
  Alcotest.(check string)
    "target authority" "example.com"
    (Awskit.Request.Target.authority target);
  Alcotest.(check string)
    "target path and query" "/objects/a%20b?x=1&x=2&empty=&flag="
    (Awskit.Request.Target.path_and_query target);
  check_validation_error "request target relative path"
    (Awskit.Request.Target.create ~scheme:`Https ~host:"example.com"
       ~path:"relative" ())

let test_request_header_append_contract () =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Https ~host:"example.com"
      ~path:"/" ()
  in
  let request =
    Awskit.Request.create_exn ~method_:`GET ~target
      ~headers:[ ("x-first", "1") ]
      ()
  in
  match Awskit.Request.add_header request ~name:"x-second" ~value:"2" with
  | Error error ->
      Alcotest.failf "unexpected add_header error: %a" Awskit.Error.pp error
  | Ok updated ->
      Alcotest.(check (list (pair string string)))
        "add_header appends in documented order"
        [ ("x-first", "1"); ("x-second", "2") ]
        updated.headers

let test_response_metadata_invariants () =
  let response =
    Awskit.Response.create_exn ~status:206
      ~headers:
        [
          ("X-Amz-Request-Id", "request-1");
          ("X-Amz-Id-2", "host-1");
          ("Content-Length", "42");
        ]
      ()
  in
  Alcotest.(check int) "status" 206 (Awskit.Response.status response);
  Alcotest.(check (option string))
    "request id" (Some "request-1")
    (Awskit.Response.request_id response);
  Alcotest.(check (option string))
    "host id" (Some "host-1")
    (Awskit.Response.host_id response);
  Alcotest.(check (option string))
    "case-insensitive header" (Some "42")
    (Awskit.Response.header response "content-length");
  Alcotest.(check (option int64))
    "int64 header" (Some 42L)
    (Awskit.Response.header_int64 response "CONTENT-LENGTH"
    |> Awskit.Error.Producer.get_ok_exn);
  Alcotest.(check bool)
    "206 is success" true
    (Awskit.Response.is_success response);
  let response_status status =
    Awskit.Response.create_exn ~status () |> Awskit.Response.is_success
  in
  Alcotest.(check bool) "199 is not success" false (response_status 199);
  Alcotest.(check bool) "200 is success" true (response_status 200);
  Alcotest.(check bool) "299 is success" true (response_status 299);
  Alcotest.(check bool) "300 is not success" false (response_status 300);
  check_validation_error "response status low"
    (Awskit.Response.create ~status:99 ());
  check_validation_error "response status high"
    (Awskit.Response.create ~status:600 ())

let test_payload_hash_and_descriptor_invariants () =
  let uppercase_hash = String.make 64 'A' in
  let hash = Awskit.Body.Payload_hash.of_sha256_hex_exn uppercase_hash in
  Alcotest.(check string)
    "payload hash normalizes uppercase hex"
    (String.lowercase_ascii uppercase_hash)
    (Awskit.Body.Payload_hash.to_header_value hash);
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~payload_hash:hash ~replayable:false ()
  in
  Alcotest.(check (option int64))
    "unknown content length" None descriptor.content_length;
  Alcotest.(check bool) "one-shot descriptor" false descriptor.replayable;
  (match Awskit.Body.Request.validate_descriptor descriptor with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "descriptor should validate: %a" Awskit.Error.pp error);
  let original = Failure "request body callback escaped" in
  let escaped =
    try
      ignore (Awskit.Body.Request.raise_escaped_exn original : unit);
      Alcotest.fail "expected escaped exception"
    with exn -> exn
  in
  Alcotest.(check bool)
    "escaped exception preserves original" true
    (match Awskit.Body.Request.escaped_exn escaped with
    | Some exn -> exn == original
    | None -> false)

let test_low_level_validation_boundaries () =
  check_validation_error "blank access key"
    (Awskit.Credentials.create ~access_key_id:"" ~secret_access_key:"SK" ());
  check_validation_error "blank secret key"
    (Awskit.Credentials.create ~access_key_id:"AKID" ~secret_access_key:"" ());
  check_validation_error "session token whitespace"
    (Awskit.Credentials.create ~access_key_id:"AKID" ~secret_access_key:"SECRET"
       ~session_token:" token" ());
  check_validation_error "region control"
    (Awskit.Region.of_string "us\001-east-1");
  check_validation_error "region whitespace"
    (Awskit.Region.of_string " us-east-1");
  check_validation_error "endpoint bracketed host"
    (Awskit.Endpoint.https ~host:"[::1]" ());
  check_validation_error "request target path control"
    (Awskit.Request.Target.create ~scheme:`Https ~host:"s3.amazonaws.com"
       ~path:"/bad\001" ());
  check_validation_error "request header newline"
    (Awskit.Request.validate_headers [ ("x-test", "bad\nvalue") ]);
  check_validation_error "response header newline"
    (Awskit.Response.create ~status:200
       ~headers:[ ("x-test", "bad\nvalue") ]
       ());
  check_validation_error "bad payload hash"
    (Awskit.Body.Payload_hash.of_sha256_hex "not-hex");
  check_validation_error "negative body length"
    (Awskit.Body.Request.descriptor ~content_length:(-1L)
       ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:true
       ())

let test_retry_budget_and_timeout_invariants () =
  check_validation_error "retry max attempts"
    (Awskit.Retry.create ~max_attempts:0 ());
  check_validation_error "retry delay ordering"
    (Awskit.Retry.create ~base_delay:(span 2.0) ~max_delay:(span 1.0) ());
  check_validation_error "retry jitter upper bound"
    (Awskit.Retry.create ~jitter:1.1 ());
  check_validation_error "retry budget capacity"
    (Awskit.Retry.budget ~capacity:0 ());
  check_validation_error "timeout zero span"
    (Awskit.Timeout.create ~connect:Ptime.Span.zero ());
  let budget =
    Awskit.Retry.budget_exn ~capacity:7 ~retry_cost:3 ~timeout_cost:5
      ~success_credit:2 ()
  in
  let policy = Awskit.Retry.create_exn ~budget () in
  Alcotest.(check int)
    "budget capacity" 7
    (Awskit.Retry.budget_capacity (Awskit.Retry.retry_budget policy));
  Alcotest.(check int)
    "transport retry cost" 3
    (Awskit.Retry.retry_cost policy retry_error);
  let timeout_error = Awskit.Error.Producer.timeout "attempt timed out" in
  Alcotest.(check int)
    "timeout retry cost" 5
    (Awskit.Retry.retry_cost policy timeout_error);
  let initial = Awskit.Retry.initial_budget_state policy in
  Alcotest.(check int)
    "initial capacity" 7
    (Awskit.Retry.available_capacity initial);
  let charged =
    match Awskit.Retry.charge_retry policy initial retry_error with
    | Some state -> state
    | None -> Alcotest.fail "retry charge should fit budget"
  in
  Alcotest.(check int)
    "charged capacity" 4
    (Awskit.Retry.available_capacity charged);
  Alcotest.(check (option int))
    "timeout charge exhausts remaining budget" None
    (Option.map Awskit.Retry.available_capacity
       (Awskit.Retry.charge_retry policy charged timeout_error));
  let credited = Awskit.Retry.credit_success policy charged in
  Alcotest.(check int)
    "success credits capacity" 6
    (Awskit.Retry.available_capacity credited);
  let capped = Awskit.Retry.credit_success policy initial in
  Alcotest.(check int)
    "success credit is capped" 7
    (Awskit.Retry.available_capacity capped);
  let timeout_policy =
    Awskit.Timeout.create_exn ~connect:(span 1.0) ~operation:(span 2.0)
      ~drain:(span 3.0) ()
  in
  Alcotest.(check (option (float 0.0001)))
    "operation timeout visible" (Some 2.0)
    (Option.map Ptime.Span.to_float_s
       (Awskit.Timeout.span timeout_policy `Operation));
  Alcotest.(check (option (float 0.0001)))
    "disabled timeout phase remains absent" None
    (Option.map Ptime.Span.to_float_s
       (Awskit.Timeout.span timeout_policy `Request_body))

let test_response_header_decode_errors () =
  let missing = Awskit.Response.create_exn ~status:200 () in
  (match Awskit.Response.required_header missing "etag" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "missing header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected missing header decode error");
  let empty =
    Awskit.Response.create_exn ~status:200 ~headers:[ ("etag", "") ] ()
  in
  (match Awskit.Response.required_header empty "etag" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "empty header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected empty header decode error");
  let malformed =
    Awskit.Response.create_exn ~status:200
      ~headers:[ ("content-length", "not-an-int") ]
      ()
  in
  (match Awskit.Response.header_int malformed "content-length" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "bad int header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected malformed header decode error");
  let negative =
    Awskit.Response.create_exn ~status:200
      ~headers:[ ("content-length", "-1") ]
      ()
  in
  match Awskit.Response.header_int64 negative "content-length" with
  | Error error when is_decode_error error -> ()
  | Error error ->
      Alcotest.failf "negative int64 header returned unexpected error: %a"
        Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected negative int64 header decode error"

let test_error_context_and_multiple_classification () =
  let error =
    Awskit.Error.Producer.validation ~field:"bucket"
      "bucket must be 3-63 characters"
    |> Awskit.Error.Producer.with_operation ~service:"s3" ~name:"CreateBucket"
         ~resource:"s3://ab" ()
    |> Awskit.Error.Producer.with_context "validating caller input"
  in
  Alcotest.(check bool)
    "validation classifier" true
    (Awskit.Error.is_validation error);
  Alcotest.(check (option string))
    "validation field" (Some "bucket")
    (Awskit.Error.validation_field error);
  let sexp = Awskit.Error.sexp_of_t error |> Base.Sexp.to_string_hum in
  check_contains "sexp names operation" "CreateBucket" sexp;
  check_contains "sexp names resource" "s3://ab" sexp;
  let combined =
    Awskit.Error.Producer.multiple
      [
        Awskit.Error.Producer.body "download failed";
        Awskit.Error.Producer.body "cleanup failed";
      ]
  in
  let human = Awskit.Error.to_string_hum combined in
  check_contains "multiple keeps primary" "download failed" human;
  check_contains "multiple keeps cleanup" "cleanup failed" human

let test_retry_deterministic_delay_trace () =
  let policy =
    Awskit.Retry.create_exn ~max_attempts:4
      ~base_delay:(Ptime.Span.of_float_s 0.5 |> Option.get)
      ~max_delay:(Ptime.Span.of_float_s 2.0 |> Option.get)
      ~jitter:0.0 ()
  in
  let random_float ~upper_bound:_ = Alcotest.fail "jitter disabled" in
  let line attempt =
    match
      Awskit.Retry.delay policy ~attempt ~error:retry_error ~random_float
    with
    | None -> Printf.sprintf "attempt=%d stop" attempt
    | Some delay ->
        Printf.sprintf "attempt=%d delay=%.1f" attempt
          (Ptime.Span.to_float_s delay)
  in
  Alcotest.(check (list string))
    "retry trace"
    [
      "attempt=1 delay=0.5";
      "attempt=2 delay=1.0";
      "attempt=3 delay=2.0";
      "attempt=4 stop";
    ]
    [ line 1; line 2; line 3; line 4 ]

let test_exn_apis_raise_sdk_exception () =
  match Awskit.Region.of_string_exn "" with
  | _ -> Alcotest.fail "expected Awskit_error"
  | exception Awskit.Error.Awskit_error error ->
      Alcotest.(check bool)
        "validation exception" true
        (Awskit.Error.is_validation error)

let suite =
  [
    ( "unit:awskit:error-redaction",
      [
        Alcotest.test_case "public diagnostics redact secret material" `Quick
          test_public_diagnostics_redact_secret_material;
        Alcotest.test_case "unsafe diagnostics preserve raw material" `Quick
          test_unsafe_diagnostics_preserve_raw_material;
        Alcotest.test_case "prefixed raw body is always replaced" `Quick
          test_prefixed_raw_body_is_always_replaced;
        Alcotest.test_case "validation field redacts secret field names" `Quick
          test_validation_field_redacts_secret_field_names;
      ] );
    ( "unit:awskit:core-contracts",
      [
        Alcotest.test_case "credentials metadata and provider" `Quick
          test_credentials_metadata_and_provider;
        Alcotest.test_case "signing sensitive handoff" `Quick
          test_signing_sensitive_handoff;
        Alcotest.test_case "static provider annotates absent source" `Quick
          test_static_provider_annotates_absent_source;
        Alcotest.test_case "credentials provider chain and expiration" `Quick
          test_credentials_provider_chain_and_expiration;
        Alcotest.test_case "endpoint and request target invariants" `Quick
          test_endpoint_and_request_target_invariants;
        Alcotest.test_case "request header append contract" `Quick
          test_request_header_append_contract;
        Alcotest.test_case "response metadata invariants" `Quick
          test_response_metadata_invariants;
        Alcotest.test_case "payload hash and descriptor invariants" `Quick
          test_payload_hash_and_descriptor_invariants;
        Alcotest.test_case "low-level validation boundaries" `Quick
          test_low_level_validation_boundaries;
        Alcotest.test_case "retry budget and timeout invariants" `Quick
          test_retry_budget_and_timeout_invariants;
        Alcotest.test_case "response header decode errors" `Quick
          test_response_header_decode_errors;
        Alcotest.test_case "error context and multiple classification" `Quick
          test_error_context_and_multiple_classification;
        Alcotest.test_case "exn APIs raise SDK exception" `Quick
          test_exn_apis_raise_sdk_exception;
      ] );
    ( "pbt:awskit:retry",
      [
        QCheck_alcotest.to_alcotest ~speed_level:`Quick
          prop_retry_jitter_stays_within_policy_bounds;
        QCheck_alcotest.to_alcotest ~speed_level:`Quick
          prop_retry_schedule_obeys_attempt_error_and_jitter;
      ] );
    ( "unit:awskit:retry",
      [
        Alcotest.test_case "deterministic delay trace" `Quick
          test_retry_deterministic_delay_trace;
      ] );
  ]

let () = Alcotest.run "awskit-core-contracts" suite
