open Awskit_s3

let check_expected_error path error =
  Protocol_fixture_diff.check_file
    (path ^ " replay error category")
    (path ^ ".expected")
    ~actual:(Protocol_replay.error_category error)

let test_endpoint_replay_corpus () =
  let dir = Protocol_replay.corpus_path [ "endpoint" ] in
  Protocol_replay.sorted_files dir
  |> List.iter (fun path ->
      let value = Protocol_fixture_diff.read_text_fixture_value path in
      match Awskit.Endpoint.of_string value with
      | Error error -> check_expected_error path error
      | Ok endpoint ->
          Alcotest.failf "%s unexpectedly parsed as %a" path Awskit.Endpoint.pp
            endpoint)

let test_header_replay_corpus () =
  let dir = Protocol_replay.corpus_path [ "headers" ] in
  Protocol_replay.sorted_files dir
  |> List.iter (fun path ->
      let value = Protocol_fixture_diff.read_file path in
      match Awskit.Request.validate_headers [ ("x-replay", value) ] with
      | Error error -> check_expected_error path error
      | Ok () -> Alcotest.failf "%s unexpectedly validated as a header" path)

let test_xml_replay_corpus () =
  let dir = Protocol_replay.corpus_path [ "xml" ] in
  Protocol_replay.sorted_files dir
  |> List.iter (fun path ->
      let conn =
        Protocol_recording_runtime.connect
          [
            Protocol_recording_runtime.response 200
              (Protocol_fixture_diff.read_file path);
          ]
      in
      match
        Protocol_recording_runtime.S3.Bucket.Tagging.get conn
          ~bucket:(Protocol_support.bucket_name "bucket")
          ()
      with
      | Error error when Protocol_support.is_decode_error error ->
          check_expected_error path error
      | Error error ->
          Alcotest.failf "%s unexpected error: %a" path Error.pp error
      | Ok _ -> Alcotest.failf "%s unexpectedly decoded as tagging XML" path)

let suite =
  [
    ( "replay:awskit-s3:protocol-wire",
      [
        Alcotest.test_case "endpoint corpus" `Quick test_endpoint_replay_corpus;
        Alcotest.test_case "header corpus" `Quick test_header_replay_corpus;
        Alcotest.test_case "xml corpus" `Quick test_xml_replay_corpus;
      ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-replay" suite
