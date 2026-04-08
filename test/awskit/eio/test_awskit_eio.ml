let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-eio" (Integration_tests.suite env)
