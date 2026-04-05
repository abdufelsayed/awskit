let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "aws-s3-eio" (Integration_tests.suite env)
