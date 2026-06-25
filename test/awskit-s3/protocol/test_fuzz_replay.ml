open Awskit_s3
open Awskit_s3_test

let corpus_path parts =
  List.fold_left Filename.concat "../fixtures/protocol/fuzz-replay" parts

let sorted_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.map (Filename.concat dir)
  |> List.filter (fun path ->
      (not (Sys.is_directory path))
      && not (Filename.check_suffix path ".expected"))

let retry_class_to_string = function
  | Awskit.Error.Retryable -> "retryable"
  | Throttled -> "throttled"
  | Auth -> "auth"
  | Conflict -> "conflict"
  | Not_found -> "not-found"
  | Fatal -> "fatal"
  | Unknown -> "unknown"

let error_category (error : Awskit.Error.t) =
  let category =
    match Awskit.Error.kind error with
    | Validation validation ->
        Fmt.str "kind=validation\nfield=%s"
          (Option.value ~default:"none" validation.field)
    | Endpoint endpoint ->
        Fmt.str "kind=endpoint\nuri=%s"
          (Option.value ~default:"none" endpoint.uri)
    | Decode _ -> "kind=decode"
    | Body _ -> "kind=body"
    | Transport transport ->
        Fmt.str "kind=transport\nretryable=%b" transport.retryable
    | Service service -> Fmt.str "kind=service\nstatus=%d" service.status
    | Credentials _ -> "kind=credentials"
    | Signing _ -> "kind=signing"
    | Timeout _ -> "kind=timeout"
    | Cancelled _ -> "kind=cancelled"
    | Retry_exhausted retry ->
        Fmt.str "kind=retry-exhausted\nattempts=%d" retry.attempts
    | Not_supported not_supported ->
        Fmt.str "kind=not-supported\nfeature=%s"
          (Option.value ~default:"none" not_supported.feature)
    | Multiple errors -> Fmt.str "kind=multiple\ncount=%d" (List.length errors)
  in
  Fmt.str "%s\nretry-class=%s" category
    (retry_class_to_string (Awskit.Error.retry_class error))

let check_expected_error path error =
  Fixture_diff.check_file
    (path ^ " replay error category")
    (path ^ ".expected") ~actual:(error_category error)

let test_endpoint_replay_corpus () =
  let dir = corpus_path [ "endpoint" ] in
  sorted_files dir
  |> List.iter (fun path ->
      let value = Fixture_diff.read_text_fixture_value path in
      match Awskit.Endpoint.of_string value with
      | Error error -> check_expected_error path error
      | Ok endpoint ->
          Alcotest.failf "%s unexpectedly parsed as %a" path Awskit.Endpoint.pp
            endpoint)

let test_header_replay_corpus () =
  let dir = corpus_path [ "headers" ] in
  sorted_files dir
  |> List.iter (fun path ->
      let value = Fixture_diff.read_file path in
      match Awskit.Request.validate_headers [ ("x-replay", value) ] with
      | Error error -> check_expected_error path error
      | Ok () -> Alcotest.failf "%s unexpectedly validated as a header" path)

let test_xml_replay_corpus () =
  let dir = corpus_path [ "xml" ] in
  sorted_files dir
  |> List.iter (fun path ->
      let conn =
        Recording_runtime.connect [ response 200 (Fixture_diff.read_file path) ]
      in
      match
        Recording_s3.Bucket.Tagging.get conn ~bucket:(bucket_name "bucket") ()
      with
      | Error error
        when Awskit.Error.(
               match kind error with Decode _ -> true | _ -> false) ->
          check_expected_error path error
      | Error error ->
          Alcotest.failf "%s unexpected error: %a" path Error.pp error
      | Ok _ -> Alcotest.failf "%s unexpectedly decoded as tagging XML" path)

let suite =
  [
    ( "replay:awskit-s3:fuzz",
      [
        Alcotest.test_case "endpoint corpus" `Quick test_endpoint_replay_corpus;
        Alcotest.test_case "header corpus" `Quick test_header_replay_corpus;
        Alcotest.test_case "xml corpus" `Quick test_xml_replay_corpus;
      ] );
  ]

let () = Alcotest.run "awskit-s3-fuzz-replay" suite
