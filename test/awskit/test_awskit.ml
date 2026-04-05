let () =
  Alcotest.run "aws"
    (List.concat [ Signing_tests.suite; Integration_tests.suite ])
