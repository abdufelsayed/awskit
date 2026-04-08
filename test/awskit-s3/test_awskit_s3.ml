let () =
  Alcotest.run "awskit-s3"
    (List.concat [ Core_tests.suite; Sim_tests.suite; Faults_tests.suite ])
