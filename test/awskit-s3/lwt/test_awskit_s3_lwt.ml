let test_lwt_body_stream_api_compiles () =
  let module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client) in
  let stream = Lwt_stream.of_list [ "hello "; "world" ] in
  let _body = S3.Body.of_lwt_stream ~content_length:11L stream in
  ()

let suite =
  [
    ( "api",
      [
        Alcotest.test_case "lwt stream body api compiles" `Quick
          test_lwt_body_stream_api_compiles;
      ] );
  ]

let () = Alcotest.run "awskit-s3-lwt" (Test_integration.suite @ suite)
