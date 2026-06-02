let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-eio" (Test_integration.suite env)
