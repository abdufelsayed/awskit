(** Property-based tests for Awskit.Signing. *)

module Signing = Awskit.Signing
module Payload_hash = Awskit.Body.Payload_hash

let creds =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let region = Awskit.Region.of_string_exn "us-east-1"

let fixed_time =
  Ptime.of_date_time ((2026, 1, 15), ((12, 0, 0), 0))
  |> Option.value ~default:Ptime.epoch

let qcheck_seed = 0xA5110
let to_alcotest = Awskit_test.Qcheck.to_alcotest ~seed:qcheck_seed

(* ── uri_encode ──────────────────────────────────────────────────── *)

let test_uri_encode_safe_chars_unchanged =
  QCheck.Test.make ~count:2000 ~name:"safe chars pass through"
    QCheck.(
      string_of
        (Gen.oneof
           [
             Gen.char_range 'A' 'Z';
             Gen.char_range 'a' 'z';
             Gen.char_range '0' '9';
             Gen.oneof_list [ '_'; '-'; '~'; '.' ];
           ]))
    (fun s -> String.equal (Signing.uri_encode s) s)

let test_uri_encode_output_is_safe =
  QCheck.Test.make ~count:2000 ~name:"output only safe chars or %XX"
    QCheck.string (fun s ->
      let encoded = Signing.uri_encode s in
      let rec check i =
        if i >= String.length encoded then true
        else
          match encoded.[i] with
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.' ->
              check (i + 1)
          | '%' ->
              i + 2 < String.length encoded
              &&
              let h1 = encoded.[i + 1] in
              let h2 = encoded.[i + 2] in
              let is_hex c =
                (c >= '0' && c <= '9')
                || (c >= 'A' && c <= 'F')
                || (c >= 'a' && c <= 'f')
              in
              is_hex h1 && is_hex h2 && check (i + 3)
          | _ -> false
      in
      check 0)

let test_uri_encode_slash_flag =
  QCheck.Test.make ~count:500 ~name:"slash preserved when flag unset"
    QCheck.string (fun s ->
      let with_slash = Signing.uri_encode ~encode_slash:false s in
      let slash_count_in s =
        String.fold_left (fun n c -> if Char.equal c '/' then n + 1 else n) 0 s
      in
      let original_slashes = slash_count_in s in
      let encoded_slashes = slash_count_in with_slash in
      original_slashes = encoded_slashes)

let test_uri_encode_no_double_encode =
  QCheck.Test.make ~count:1000 ~name:"already-safe string is identity"
    QCheck.(
      string_of
        (Gen.oneof
           [
             Gen.char_range 'A' 'Z';
             Gen.char_range 'a' 'z';
             Gen.char_range '0' '9';
           ]))
    (fun s ->
      let once = Signing.uri_encode s in
      String.equal once s)

(* ── canonical_query ─────────────────────────────────────────────── *)

let query_pair_gen =
  let key_gen =
    QCheck.Gen.(
      map
        (fun s -> "k" ^ s)
        (string_size ~gen:(char_range 'a' 'z') (int_range 1 8)))
  in
  let val_gen =
    QCheck.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 0 8))
  in
  QCheck.Gen.pair key_gen val_gen

let test_canonical_query_sorted =
  QCheck.Test.make ~count:1000 ~name:"output is sorted"
    QCheck.(make Gen.(list_size (int_range 1 10) query_pair_gen))
    (fun query ->
      let result =
        Signing.canonical_query_params
          (List.map (fun (key, value) -> (key, [ value ])) query)
      in
      let pairs = String.split_on_char '&' result in
      let rec is_sorted = function
        | [] | [ _ ] -> true
        | a :: b :: rest -> String.compare a b <= 0 && is_sorted (b :: rest)
      in
      is_sorted pairs)

let test_canonical_query_empty =
  QCheck.Test.make ~count:1 ~name:"empty in, empty out"
    QCheck.(always [])
    (fun params -> String.equal (Signing.canonical_query_params params) "")

let test_canonical_query_preserves_pairs =
  QCheck.Test.make ~count:1000 ~name:"preserves number of pairs"
    QCheck.(make Gen.(list_size (int_range 1 10) query_pair_gen))
    (fun pairs ->
      let result =
        Signing.canonical_query_params
          (List.map (fun (key, value) -> (key, [ value ])) pairs)
      in
      let result_pairs = String.split_on_char '&' result in
      List.length pairs = List.length result_pairs)

(* ── sign_request ────────────────────────────────────────────────── *)

let test_sign_deterministic =
  QCheck.Test.make ~count:500 ~name:"deterministic"
    QCheck.(pair string string)
    (fun (path, payload) ->
      let sign () =
        Signing.sign_request_params_exn ~credentials:creds ~region ~service:"s3"
          ~method_:`GET ~path:("/" ^ path) ~query_params:[]
          ~headers:[ ("host", "s3.amazonaws.com") ]
          ~payload_hash:(Payload_hash.sha256_of_string payload)
          ~now:fixed_time
      in
      let r1 = sign () in
      let r2 = sign () in
      let get_auth h =
        List.assoc "authorization"
          (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) h)
      in
      String.equal (get_auth r1.headers) (get_auth r2.headers))

let test_sign_has_required_headers =
  QCheck.Test.make ~count:500 ~name:"always has required headers"
    QCheck.(pair string string)
    (fun (path, payload) ->
      let result =
        Signing.sign_request_params_exn ~credentials:creds ~region ~service:"s3"
          ~method_:`GET ~path:("/" ^ path) ~query_params:[]
          ~headers:[ ("host", "s3.amazonaws.com") ]
          ~payload_hash:(Payload_hash.sha256_of_string payload)
          ~now:fixed_time
      in
      let lower_keys =
        List.map (fun (k, _) -> String.lowercase_ascii k) result.headers
      in
      List.mem "authorization" lower_keys
      && List.mem "x-amz-date" lower_keys
      && List.mem "x-amz-content-sha256" lower_keys)

let test_sign_signature_is_hex =
  QCheck.Test.make ~count:500 ~name:"signature is 64 hex chars" QCheck.string
    (fun payload ->
      let result =
        Signing.sign_request_params_exn ~credentials:creds ~region ~service:"s3"
          ~method_:`GET ~path:"/bucket/key" ~query_params:[]
          ~headers:[ ("host", "s3.amazonaws.com") ]
          ~payload_hash:(Payload_hash.sha256_of_string payload)
          ~now:fixed_time
      in
      let auth =
        List.assoc "authorization"
          (List.map
             (fun (k, v) -> (String.lowercase_ascii k, v))
             result.headers)
      in
      let prefix = "Signature=" in
      let idx = ref None in
      for i = 0 to String.length auth - String.length prefix - 1 do
        if
          Option.is_none !idx
          && String.sub auth i (String.length prefix) = prefix
        then idx := Some (i + String.length prefix)
      done;
      match !idx with
      | None -> false
      | Some start ->
          let sig_str = String.sub auth start (String.length auth - start) in
          String.length sig_str = 64
          && String.to_seq sig_str
             |> Seq.for_all (fun c ->
                 (c >= '0' && c <= '9')
                 || (c >= 'a' && c <= 'f')
                 || (c >= 'A' && c <= 'F')))

let test_sign_different_payloads =
  QCheck.Test.make ~count:500 ~name:"different payloads → different signatures"
    QCheck.(pair string string)
    (fun (p1, p2) ->
      QCheck.assume (not (String.equal p1 p2));
      let sign payload =
        Signing.sign_request_params_exn ~credentials:creds ~region ~service:"s3"
          ~method_:`GET ~path:"/bucket/key" ~query_params:[]
          ~headers:[ ("host", "s3.amazonaws.com") ]
          ~payload_hash:(Payload_hash.sha256_of_string payload)
          ~now:fixed_time
      in
      let get_auth h =
        List.assoc "authorization"
          (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) h)
      in
      not
        (String.equal (get_auth (sign p1).headers) (get_auth (sign p2).headers)))

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let expect_credentials label = function
  | Error error when Awskit.Error.is_credentials error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected credentials error" label

let test_sign_rejects_header_newline () =
  expect_validation "sign header newline"
    (Signing.sign_request_params ~credentials:creds ~region ~service:"s3"
       ~method_:`GET ~path:"/bucket/key" ~query_params:[]
       ~headers:
         [ ("host", "s3.amazonaws.com"); ("x-test", "ok\r\nInjected: yes") ]
       ~payload_hash:(Payload_hash.sha256_of_string "")
       ~now:fixed_time)

let test_sign_rejects_expired_credentials () =
  let expires_at = Ptime.add_span fixed_time (Ptime.Span.of_int_s (-1)) in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ?expires_at ()
  in
  expect_credentials "sign expired credentials"
    (Signing.sign_request_params ~credentials ~region ~service:"s3"
       ~method_:`GET ~path:"/bucket/key" ~query_params:[]
       ~headers:[ ("host", "s3.amazonaws.com") ]
       ~payload_hash:(Payload_hash.sha256_of_string "")
       ~now:fixed_time)

(* ── ptime_to_date_time ──────────────────────────────────────────── *)

let test_ptime_datestamp_format =
  QCheck.Test.make ~count:1000 ~name:"datestamp is YYYYMMDD"
    QCheck.(
      make
        Gen.(
          map
            (fun days ->
              Ptime.of_span (Ptime.Span.of_int_s (days * 86400))
              |> Option.value ~default:Ptime.epoch)
            (int_range 0 20000)))
    (fun t ->
      let datestamp, _ = Signing.ptime_to_date_time t in
      String.length datestamp = 8
      && String.to_seq datestamp |> Seq.for_all (fun c -> c >= '0' && c <= '9'))

let test_ptime_amzdate_format =
  QCheck.Test.make ~count:1000 ~name:"amz_date is YYYYMMDDTHHMMSSZ"
    QCheck.(
      make
        Gen.(
          map
            (fun secs ->
              Ptime.of_span (Ptime.Span.of_int_s secs)
              |> Option.value ~default:Ptime.epoch)
            (int_range 0 (20000 * 86400))))
    (fun t ->
      let _, amz_date = Signing.ptime_to_date_time t in
      String.length amz_date = 16 && amz_date.[8] = 'T' && amz_date.[15] = 'Z')

let suite =
  [
    ( "pbt:signing:uri-encode",
      List.map to_alcotest
        [
          test_uri_encode_safe_chars_unchanged;
          test_uri_encode_output_is_safe;
          test_uri_encode_slash_flag;
          test_uri_encode_no_double_encode;
        ] );
    ( "pbt:signing:canonical-query",
      List.map to_alcotest
        [
          test_canonical_query_sorted;
          test_canonical_query_empty;
          test_canonical_query_preserves_pairs;
        ] );
    ( "pbt:signing:sign-request",
      List.map to_alcotest
        [
          test_sign_deterministic;
          test_sign_has_required_headers;
          test_sign_signature_is_hex;
          test_sign_different_payloads;
        ] );
    ( "signing:validation",
      [
        Alcotest.test_case "rejects header newline" `Quick
          test_sign_rejects_header_newline;
        Alcotest.test_case "rejects expired credentials" `Quick
          test_sign_rejects_expired_credentials;
      ] );
    ( "pbt:signing:ptime",
      List.map to_alcotest
        [ test_ptime_datestamp_format; test_ptime_amzdate_format ] );
  ]
