let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-s3-eio"
    (Integration_tests.suite env @ Transfer_tests.suite env)
