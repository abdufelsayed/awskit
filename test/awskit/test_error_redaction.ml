open Base

let redacted = "<redacted>"

let raw_headers =
  [
    ( "Authorization",
      "AWS4-HMAC-SHA256 Credential=AKIA/20260621/us-east-1/s3/aws4_request, \
       Signature=SECRET" );
    ("Cookie", "session=session-cookie-value");
    ("X-Amz-Security-Token", "SESSION");
    ("X-Amz-Credential", "AKIA/20260621/us-east-1/s3/aws4_request");
    ("X-Amz-Signature", "abc");
    ("x-safe-header", "safe-value");
  ]

let raw_body =
  String.concat ~sep:" "
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

let sentinels =
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

let contains text substring = String.is_substring text ~substring

let check_absent label text =
  List.iter sentinels ~f:(fun sentinel ->
      Alcotest.(check bool)
        (label ^ " redacts " ^ sentinel)
        false (contains text sentinel))

let check_present label text =
  List.iter sentinels ~f:(fun sentinel ->
      Alcotest.(check bool)
        (label ^ " preserves " ^ sentinel)
        true (contains text sentinel))

let make_service_error () =
  Awskit.Error.Producer.service ~status:403 ~code:"AccessDenied"
    ~message:"Authorization: AWS4-HMAC-SHA256 failed" ~request_id:"request-1"
    ~host_id:"host-1" ~headers:raw_headers ~body:raw_body ()

let prefixed_raw_body =
  redacted ^ " SECRET X-Amz-Signature=abc X-Amz-Security-Token"

let make_prefixed_body_error () =
  Awskit.Error.Producer.service ~status:500 ~code:"InternalError"
    ~headers:[ ("x-safe-header", "safe-value") ]
    ~body:prefixed_raw_body ()

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

let format pp value = Stdlib.Format.asprintf "%a" pp value

let service_text service =
  String.concat ~sep:" "
    (Option.to_list service.Awskit.Error.body
    @ List.map service.headers ~f:(fun (name, value) -> name ^ ": " ^ value))

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
  List.iter public_outputs ~f:(fun (label, text) -> check_absent label text);
  (match Awskit.Error.kind direct with
  | Awskit.Error.Service service ->
      check_absent "public service view" (service_text service);
      Alcotest.(check bool)
        "service body marker" true
        (Option.value_map service.body ~default:false ~f:(fun body ->
             contains body redacted));
      Alcotest.(check bool)
        "safe header value survives" true
        (List.exists service.headers ~f:(fun (name, value) ->
             String.equal name "x-safe-header"
             && String.equal value "safe-value"))
  | _ -> Alcotest.fail "expected service error");
  List.iter (Awskit.Error.context nested) ~f:(fun context ->
      check_absent "public context view" (context_text context));
  let exception_message =
    try Awskit.Error.Producer.raise nested with exn -> Exn.to_string exn
  in
  check_absent "exception message" exception_message

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
  check_present "unsafe sexp" unsafe_sexp

let test_prefixed_raw_body_is_always_replaced () =
  let error = make_prefixed_body_error () in
  let public_outputs =
    [
      ("pp", format Awskit.Error.pp error);
      ("to_string_hum", Awskit.Error.to_string_hum error);
      ("pp_sexp", format Awskit.Error.pp_sexp error);
      ("to_sexp_string_hum", Awskit.Error.to_sexp_string_hum error);
      ("sexp_of_t", Awskit.Error.sexp_of_t error |> Base.Sexp.to_string_hum);
    ]
  in
  List.iter public_outputs ~f:(fun (label, text) -> check_absent label text);
  (match Awskit.Error.kind error with
  | Awskit.Error.Service service ->
      let expected_body =
        Printf.sprintf "%s:%d bytes" redacted (String.length prefixed_raw_body)
      in
      Alcotest.(check (option string))
        "public service body" (Some expected_body) service.body;
      check_absent "prefixed public service view" (service_text service)
  | _ -> Alcotest.fail "expected service error");
  Alcotest.(check (option string))
    "unsafe body" (Some prefixed_raw_body)
    (Awskit.Error.Unsafe_diagnostics.service_body error);
  let unsafe_sexp =
    Awskit.Error.Unsafe_diagnostics.to_sexp_unredacted error
    |> Base.Sexp.to_string_hum
  in
  Alcotest.(check bool)
    "unsafe sexp preserves raw body" true
    (contains unsafe_sexp prefixed_raw_body)

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

let suite =
  [
    ( "error:redaction",
      [
        Alcotest.test_case "public diagnostics redact secret material" `Quick
          test_public_diagnostics_redact_secret_material;
        Alcotest.test_case "unsafe diagnostics preserve raw material" `Quick
          test_unsafe_diagnostics_preserve_raw_material;
        Alcotest.test_case "prefixed raw body is always replaced" `Quick
          test_prefixed_raw_body_is_always_replaced;
        Alcotest.test_case "validation_field redacts secret field names" `Quick
          test_validation_field_redacts_secret_field_names;
      ] );
  ]
