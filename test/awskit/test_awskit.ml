let () =
  Alcotest.run "awskit"
    (List.concat
       [
         Core_contract_tests.suite; Signing_tests.suite; Integration_tests.suite;
       ])
