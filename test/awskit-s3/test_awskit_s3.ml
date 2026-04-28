let () = Alcotest.run "awskit-s3" (Core_tests.suite @ Contract_tests.suite)
