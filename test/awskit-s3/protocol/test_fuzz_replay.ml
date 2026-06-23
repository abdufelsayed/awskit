open Awskit_s3
open Awskit_s3_test

let corpus_path parts =
  List.fold_left Filename.concat "../fixtures/protocol/fuzz-replay" parts

let sorted_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.map (Filename.concat dir)
  |> List.filter (fun path -> not (Sys.is_directory path))

let test_endpoint_replay_corpus () =
  let dir = corpus_path [ "endpoint" ] in
  sorted_files dir
  |> List.iter (fun path ->
      let value = Fixture_diff.read_text_fixture_value path in
      match Awskit.Endpoint.of_string value with
      | Error _ -> ()
      | Ok endpoint ->
          Alcotest.failf "%s unexpectedly parsed as %a" path Awskit.Endpoint.pp
            endpoint)

let test_header_replay_corpus () =
  let dir = corpus_path [ "headers" ] in
  sorted_files dir
  |> List.iter (fun path ->
      let value = Fixture_diff.read_file path in
      match Awskit.Request.validate_headers [ ("x-replay", value) ] with
      | Error _ -> ()
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
          ()
      | Error error ->
          Alcotest.failf "%s unexpected error: %a" path Error.pp error
      | Ok _ -> Alcotest.failf "%s unexpectedly decoded as tagging XML" path)

let suite =
  [
    ( "fuzz replay",
      [
        Alcotest.test_case "endpoint corpus" `Quick test_endpoint_replay_corpus;
        Alcotest.test_case "header corpus" `Quick test_header_replay_corpus;
        Alcotest.test_case "xml corpus" `Quick test_xml_replay_corpus;
      ] );
  ]

let () = Alcotest.run "awskit-s3-fuzz-replay" suite
