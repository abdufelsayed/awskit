let () =
  Alcotest.run "awskit"
    (List.concat [ Signing_tests.suite; Integration_tests.suite ])
