let () =
  Alcotest.run "awskit-s3-lwt-unix"
    (Integration_tests.suite () @ Transfer_tests.suite ())
