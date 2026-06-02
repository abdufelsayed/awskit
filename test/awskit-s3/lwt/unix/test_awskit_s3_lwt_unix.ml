let () =
  Alcotest.run "awskit-s3-lwt-unix"
    (Test_integration.suite () @ Test_transfer.suite ())
