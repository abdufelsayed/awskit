let () =
  Alcotest.run "awskit-lwt" (Test_integration.suite @ Http_contract_lwt.suite)
