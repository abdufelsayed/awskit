let () =
  Alcotest.run "awskit-s3-sim"
    (List.concat [ Test_simulator.suite; Test_simulator_contract.suite ])
