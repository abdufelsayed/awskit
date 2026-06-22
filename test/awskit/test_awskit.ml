let () =
  Alcotest.run "awskit"
    (List.concat
       [
         Test_core_contract.suite;
         Test_error_redaction.suite;
         Test_signing.suite;
         Test_integration.suite;
       ])
