let () =
  Alcotest.run "awskit-s3-sim"
    (Test_simulator_stateful_pbt.suite @ Test_transfer_fault_workload.suite)
