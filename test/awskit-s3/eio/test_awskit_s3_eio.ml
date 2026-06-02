let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-s3-eio"
    (Test_integration.suite env @ Test_transfer.suite env)
