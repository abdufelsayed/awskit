open Awskit_s3
module O = Awskit.Observability
module P = O.For_projection

let observation_name completion =
  completion |> P.Operation.Completion.info |> P.Operation.Info.name

let check_failed_logical_observation path conn =
  let logical =
    Protocol_recording_runtime.observations conn
    |> List.filter (fun completion ->
        String.equal "awskit.s3.operation" (observation_name completion))
  in
  Alcotest.(check int)
    (path ^ " logical completion count")
    1 (List.length logical);
  match logical with
  | [ completion ] ->
      Alcotest.(check string)
        (path ^ " logical completion outcome")
        "error"
        (completion |> P.Operation.Completion.outcome |> O.Outcome.to_string)
  | _ -> ()

let check_record_metadata path record =
  Protocol_fixture_diff.check_string
    (path ^ " replay fixture path")
    ~expected:(Protocol_replay.relative_fixture_path path)
    ~actual:record.Protocol_replay.fixture_path;
  Protocol_fixture_diff.check_string
    (path ^ " replay input kind")
    ~expected:
      (Protocol_replay.input_kind_to_string
         (Protocol_replay.expected_input_kind record.operation))
    ~actual:(Protocol_replay.input_kind_to_string record.input_kind);
  let actual_input =
    match record.input_kind with
    | Protocol_replay.Normalized_text ->
        Protocol_fixture_diff.read_text_fixture_value path
    | Protocol_replay.Bytes_hex ->
        Protocol_replay.hex_encode (Protocol_fixture_diff.read_file path)
  in
  Protocol_fixture_diff.check_string (path ^ " replay input")
    ~expected:record.input ~actual:actual_input

let check_expected_error path record error =
  Protocol_fixture_diff.check_string
    (path ^ " replay error category")
    ~expected:record.Protocol_replay.expected_error_category
    ~actual:(Protocol_replay.error_category error)

let run_replay path =
  let record = Protocol_replay.read_record path in
  check_record_metadata path record;
  match record.operation with
  | Protocol_replay.Endpoint_of_string -> (
      let value = Protocol_fixture_diff.read_text_fixture_value path in
      match Awskit.Endpoint.of_string value with
      | Error error -> check_expected_error path record error
      | Ok endpoint ->
          Alcotest.failf "%s unexpectedly parsed as %a" path Awskit.Endpoint.pp
            endpoint)
  | Protocol_replay.Request_validate_headers -> (
      let value = Protocol_fixture_diff.read_file path in
      match Awskit.Request.validate_headers [ ("x-replay", value) ] with
      | Error error -> check_expected_error path record error
      | Ok () -> Alcotest.failf "%s unexpectedly validated as a header" path)
  | Protocol_replay.S3_bucket_tagging_get -> (
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
          check_expected_error path record error;
          check_failed_logical_observation path conn
      | Error error ->
          Alcotest.failf "%s unexpected error: %a" path Error.pp error
      | Ok _ -> Alcotest.failf "%s unexpectedly decoded as tagging XML" path)

let test_replay_corpus () =
  Protocol_replay.replay_files (Protocol_replay.corpus_path [])
  |> List.iter run_replay

let suite =
  [
    ( "replay:awskit-s3:protocol-wire",
      [ Alcotest.test_case "fuzz replay corpus" `Quick test_replay_corpus ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-replay" suite
