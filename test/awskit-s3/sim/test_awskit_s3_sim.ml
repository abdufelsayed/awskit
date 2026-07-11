module Client_contract : Awskit_s3.S = Awskit_s3_sim

let () =
  Alcotest.run "awskit-s3-sim"
    (Test_sim_multipart_validation.suite
    @ Test_sim_bucket_configurations.suite
    @ Test_sim_list_pagination.suite
    @ Test_sim_response_metadata.suite
    @ Test_sim_body_lifecycle.suite
    @ Test_sim_contract_parity.suite
    @ Test_simulator_stateful_pbt.suite
    @ Test_transfer_fault_workload.suite)
