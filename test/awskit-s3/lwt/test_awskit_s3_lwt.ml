open Awskit_s3
module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)
module Client_contract : Awskit_s3.S = S3

type response = { status : int; body : string }

type call = {
  meth : Cohttp.Code.meth;
  uri : Uri.t;
  headers : Cohttp.Header.t;
  body : string;
}

let test_time = Ptime.of_date_time ((2026, 7, 11), ((0, 0, 0), 0)) |> Option.get

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let install_transport responses =
  let pending = ref responses in
  let calls = ref [] in
  Cohttp_lwt_unix.Client.set_cache
    (fun
      ?(headers = Cohttp.Header.init ())
      ?(body = Cohttp_lwt.Body.empty)
      ?absolute_form:_
      meth
      uri
    ->
      let open Lwt.Syntax in
      let* body = Cohttp_lwt.Body.to_string body in
      calls := { meth; uri; headers; body } :: !calls;
      match !pending with
      | [] -> Alcotest.fail "controlled transport exhausted"
      | stub :: rest ->
          pending := rest;
          let status = Cohttp.Code.status_of_code stub.status in
          let response = Cohttp.Response.make ~status () in
          Lwt.return (response, Cohttp_lwt.Body.of_string stub.body));
  fun () -> List.rev !calls

let create_client ?(retry_policy = Awskit.Retry.disabled) ?sleep () =
  S3.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> test_time)
    ~retry_policy ?sleep
    ~random_float:(fun ~upper_bound:_ -> 0.)
    ~timeout_policy:Awskit.Timeout.disabled ()
  |> ok_or_fail "create generic Lwt S3 client"

let bucket = Bucket_name.of_string_exn "test-bucket"
let key = Object_key.of_string_exn "object.txt"
let put client body = S3.Object.put client ~bucket ~key ~body () |> Lwt_main.run

let retry_policy =
  Awskit.Retry.create_exn ~max_attempts:3 ~base_delay:(Ptime.Span.of_int_s 1)
    ~max_delay:(Ptime.Span.of_int_s 1) ~jitter:0. ()

let retry_context error =
  Awskit.Error.context error
  |> List.find_map (function
    | Awskit.Error.Retry retry -> Some retry
    | Message _ | Operation _ | Sexp _ -> None)

let retry_reason error =
  Option.map
    (fun (retry : Awskit.Error.retry) -> retry.reason)
    (retry_context error)

let slow_down =
  {
    status = 503;
    body = "<Error><Code>SlowDown</Code><Message>retry later</Message></Error>";
  }

let success = { status = 200; body = "" }

let test_generic_client_and_lwt_stream_body () =
  let calls = install_transport [ success ] in
  let client = create_client () in
  let body =
    S3.Body.of_lwt_stream ~content_length:4L (Lwt_stream.of_list [ "ab"; "cd" ])
    |> ok_or_fail "create Lwt stream body"
  in
  Alcotest.(check (option int64))
    "known content length" (Some 4L)
    (S3.Body.content_length body);
  Alcotest.(check bool) "one-shot stream" false (S3.Body.replayable body);
  ignore (put client body |> ok_or_fail "put Lwt stream body");
  match calls () with
  | [ call ] ->
      Alcotest.(check string)
        "request method" "PUT"
        (Cohttp.Code.string_of_method call.meth);
      Alcotest.(check string)
        "request host" "test-bucket.s3.us-east-1.amazonaws.com"
        (Uri.host call.uri |> Option.value ~default:"");
      Alcotest.(check string) "request body" "abcd" call.body;
      Alcotest.(check bool)
        "authorization header present" true
        (Cohttp.Header.mem call.headers "authorization")
  | calls -> Alcotest.failf "expected one request, got %d" (List.length calls)

let test_non_replayable_body_is_not_retried () =
  let calls = install_transport [ slow_down; success ] in
  let sleeps = ref [] in
  let sleep span =
    sleeps := span :: !sleeps;
    Lwt.return_unit
  in
  let client = create_client ~retry_policy ~sleep () in
  let body =
    S3.Body.of_lwt_stream ~content_length:4L (Lwt_stream.of_list [ "body" ])
    |> ok_or_fail "create one-shot body"
  in
  match put client body with
  | Ok _ -> Alcotest.fail "expected SlowDown error"
  | Error error ->
      Alcotest.(check int) "one HTTP attempt" 1 (List.length (calls ()));
      Alcotest.(check int) "no retry sleep" 0 (List.length !sleeps);
      Alcotest.(check (option string))
        "retry reason"
        (Some "not retried because request body is not replayable")
        (retry_reason error)

let test_replayable_body_follows_retry_policy () =
  let calls = install_transport [ slow_down; success ] in
  let sleeps = ref [] in
  let sleep span =
    sleeps := span :: !sleeps;
    Lwt.return_unit
  in
  let client = create_client ~retry_policy ~sleep () in
  let body = S3.Body.of_string "body" in
  ignore (put client body |> ok_or_fail "retry replayable body");
  let calls = calls () in
  Alcotest.(check int) "two HTTP attempts" 2 (List.length calls);
  Alcotest.(check (list string))
    "identical replayed bodies" [ "body"; "body" ]
    (List.map (fun call -> call.body) calls);
  Alcotest.(check int) "one retry sleep" 1 (List.length !sleeps)

let test_replayable_exhaustion_reports_reason () =
  let calls = install_transport [ slow_down; slow_down; slow_down ] in
  let sleeps = ref [] in
  let sleep span =
    sleeps := span :: !sleeps;
    Lwt.return_unit
  in
  let client = create_client ~retry_policy ~sleep () in
  match put client (S3.Body.of_string "body") with
  | Ok _ -> Alcotest.fail "expected retry exhaustion"
  | Error error -> (
      Alcotest.(check int) "three HTTP attempts" 3 (List.length (calls ()));
      Alcotest.(check int) "two retry sleeps" 2 (List.length !sleeps);
      Alcotest.(check (option string))
        "retry reason" (Some "retry attempts exhausted") (retry_reason error);
      match retry_context error with
      | None -> Alcotest.fail "expected retry context"
      | Some retry ->
          Alcotest.(check int) "terminal attempt" 3 retry.attempt;
          Alcotest.(check (option int))
            "maximum attempts" (Some 3) retry.max_attempts)

let () =
  Alcotest.run "awskit-s3-lwt"
    [
      ( "unit:awskit-s3-lwt:composition",
        [
          Alcotest.test_case "generic client and Lwt stream body" `Quick
            test_generic_client_and_lwt_stream_body;
        ] );
      ( "unit:awskit-s3-lwt:retry",
        [
          Alcotest.test_case "one-shot body is not retried" `Quick
            test_non_replayable_body_is_not_retried;
          Alcotest.test_case "replayable body follows retry policy" `Quick
            test_replayable_body_follows_retry_policy;
          Alcotest.test_case "replayable exhaustion reports reason" `Quick
            test_replayable_exhaustion_reports_reason;
        ] );
    ]
