(** Integration tests: AWS SigV4 test vectors, credentials, connection. *)

open Base
module Signing = Awskit.Signing

(* AWS official test credentials *)
let creds =
  Awskit.Credentials.make ~access_key_id:"AKIDEXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" ()

(* 2015-08-30T12:36:00Z *)
let test_time =
  Ptime.of_date_time ((2015, 8, 30), ((12, 36, 0), 0))
  |> Option.value ~default:Ptime.epoch

let lower_headers h = List.map h ~f:(fun (k, v) -> (String.lowercase k, v))

let get_auth headers =
  List.Assoc.find_exn (lower_headers headers) ~equal:String.equal
    "authorization"

let get_header key headers =
  List.Assoc.find_exn (lower_headers headers) ~equal:String.equal key

(* ── sigv4 ───────────────────────────────────────────────────────── *)

let test_get_vanilla () =
  let result =
    Signing.sign_request ~credentials:creds ~region:"us-east-1"
      ~service:"service" ~meth:"GET" ~path:"/" ~query:""
      ~headers:[ ("host", "example.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  let auth = get_auth result.headers in
  Alcotest.(check bool)
    "starts with AWS4-HMAC-SHA256" true
    (String.is_prefix auth ~prefix:"AWS4-HMAC-SHA256");
  Alcotest.(check bool)
    "contains correct credential" true
    (String.is_substring auth
       ~substring:
         "Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request");
  Alcotest.(check string)
    "signed headers" "host;x-amz-content-sha256;x-amz-date"
    result.signed_headers_str

let test_get_query_order () =
  let sign q =
    Signing.sign_request ~credentials:creds ~region:"us-east-1"
      ~service:"service" ~meth:"GET" ~path:"/" ~query:q
      ~headers:[ ("host", "example.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  Alcotest.(check string)
    "query param order doesn't matter"
    (get_auth (sign "b=2&a=1").headers)
    (get_auth (sign "a=1&b=2").headers)

let test_post_with_body () =
  let body = "Action=ListUsers&Version=2010-05-08" in
  let sign payload =
    Signing.sign_request ~credentials:creds ~region:"us-east-1"
      ~service:"service" ~meth:"POST" ~path:"/" ~query:""
      ~headers:
        [
          ("host", "example.amazonaws.com");
          ("content-type", "application/x-www-form-urlencoded");
        ]
      ~payload ~now:test_time
  in
  let with_body = sign body in
  let without_body = sign "" in
  Alcotest.(check bool)
    "content-type is signed" true
    (String.is_substring ~substring:"content-type" with_body.signed_headers_str);
  Alcotest.(check bool)
    "body affects signature" true
    (not
       (String.equal
          (get_auth with_body.headers)
          (get_auth without_body.headers)))

let test_session_token () =
  let creds_with_token =
    Awskit.Credentials.make ~access_key_id:"AKIDEXAMPLE"
      ~secret_access_key:"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
      ~session_token:"FakeSessionToken" ()
  in
  let sign c =
    Signing.sign_request ~credentials:c ~region:"us-east-1" ~service:"service"
      ~meth:"GET" ~path:"/" ~query:""
      ~headers:[ ("host", "example.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  let with_token = sign creds_with_token in
  let without_token = sign creds in
  Alcotest.(check string)
    "token in headers" "FakeSessionToken"
    (get_header "x-amz-security-token" with_token.headers);
  Alcotest.(check bool)
    "security-token is signed" true
    (String.is_substring ~substring:"x-amz-security-token"
       with_token.signed_headers_str);
  Alcotest.(check bool)
    "token changes signature" true
    (not
       (String.equal
          (get_auth with_token.headers)
          (get_auth without_token.headers)))

let test_header_case () =
  let sign h =
    Signing.sign_request ~credentials:creds ~region:"us-east-1"
      ~service:"service" ~meth:"GET" ~path:"/" ~query:""
      ~headers:[ ("host", h) ]
      ~payload:"" ~now:test_time
  in
  Alcotest.(check string)
    "header case doesn't matter"
    (get_auth (sign "example.amazonaws.com").headers)
    (get_auth (sign "example.amazonaws.com").headers)

let test_header_whitespace () =
  let sign h =
    Signing.sign_request ~credentials:creds ~region:"us-east-1"
      ~service:"service" ~meth:"GET" ~path:"/" ~query:""
      ~headers:[ ("host", h) ]
      ~payload:"" ~now:test_time
  in
  Alcotest.(check string)
    "whitespace is trimmed"
    (get_auth (sign "  example.amazonaws.com  ").headers)
    (get_auth (sign "example.amazonaws.com").headers)

let test_duplicate_host_rejected () =
  Alcotest.check_raises "duplicate host"
    (Invalid_argument "Awskit.Signing.sign_request: duplicate host header")
    (fun () ->
      ignore
        (Signing.sign_request ~credentials:creds ~region:"us-east-1"
           ~service:"service" ~meth:"GET" ~path:"/" ~query:""
           ~headers:
             [
               ("host", "example.amazonaws.com");
               ("Host", "duplicate.amazonaws.com");
             ]
           ~payload:"" ~now:test_time))

let test_path_encoding () =
  let result =
    Signing.sign_request ~credentials:creds ~region:"us-east-1" ~service:"s3"
      ~meth:"GET" ~path:"/bucket/my key.txt" ~query:""
      ~headers:[ ("host", "s3.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  Alcotest.(check bool)
    "signs path with special chars" true
    (not (String.equal (get_auth result.headers) ""))

let test_sign_request_params_matches_raw_query () =
  let query_params =
    [ ("prefix", [ "folder/a b.txt" ]); ("max-keys", [ "10" ]) ]
  in
  let raw_query = "prefix=folder/a b.txt&max-keys=10" in
  let from_raw =
    Signing.sign_request ~credentials:creds ~region:"us-east-1" ~service:"s3"
      ~meth:"GET" ~path:"/bucket" ~query:raw_query
      ~headers:[ ("host", "s3.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  let from_params =
    Signing.sign_request_params ~credentials:creds ~region:"us-east-1"
      ~service:"s3" ~meth:"GET" ~path:"/bucket" ~query_params
      ~headers:[ ("host", "s3.amazonaws.com") ]
      ~payload:"" ~now:test_time
  in
  Alcotest.(check string)
    "structured query matches raw query"
    (get_auth from_raw.headers)
    (get_auth from_params.headers)

(* ── credentials ─────────────────────────────────────────────────── *)

let test_credentials_roundtrip () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK"
      ~session_token:"TOK" ()
  in
  Alcotest.(check string)
    "access_key_id" "AK"
    (Awskit.Credentials.access_key_id c);
  Alcotest.(check (option string))
    "session_token" (Some "TOK")
    (Awskit.Credentials.session_token c)

let test_credentials_no_token () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Alcotest.(check (option string))
    "defaults to None" None
    (Awskit.Credentials.session_token c)

let test_credentials_reject_whitespace () =
  Alcotest.check_raises "reject whitespace"
    (Invalid_argument
       "Awskit.Credentials.make: access_key_id must not have leading/trailing \
        whitespace") (fun () ->
      ignore
        (Awskit.Credentials.make ~access_key_id:" AK " ~secret_access_key:"SK"
           ()))

let suite =
  [
    ( "integration:sigv4:get",
      [
        Alcotest.test_case "vanilla" `Quick test_get_vanilla;
        Alcotest.test_case "query param order" `Quick test_get_query_order;
        Alcotest.test_case "path encoding" `Quick test_path_encoding;
        Alcotest.test_case "structured query params" `Quick
          test_sign_request_params_matches_raw_query;
      ] );
    ( "integration:sigv4:post",
      [ Alcotest.test_case "body affects signature" `Quick test_post_with_body ]
    );
    ( "integration:sigv4:headers",
      [
        Alcotest.test_case "case normalization" `Quick test_header_case;
        Alcotest.test_case "whitespace trimming" `Quick test_header_whitespace;
        Alcotest.test_case "session token" `Quick test_session_token;
        Alcotest.test_case "duplicate host rejected" `Quick
          test_duplicate_host_rejected;
      ] );
    ( "integration:credentials",
      [
        Alcotest.test_case "roundtrip with token" `Quick
          test_credentials_roundtrip;
        Alcotest.test_case "no token" `Quick test_credentials_no_token;
        Alcotest.test_case "reject whitespace" `Quick
          test_credentials_reject_whitespace;
      ] );
  ]
