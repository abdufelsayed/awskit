let () =
  Alcotest.run "aws-s3" (List.concat [ Sim_tests.suite; Faults_tests.suite ])
