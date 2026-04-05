let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "aws-eio" (Integration_tests.suite env)
