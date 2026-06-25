let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-eio"
    (Test_integration.suite env @ Http_contract_eio.suite env)
