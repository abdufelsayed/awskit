let () =
  Alcotest.run "awskit-s3"
    (List.concat
       [
         Test_api.suite;
         Test_endpoint.suite;
         Test_presigned.suite;
         Test_bucket_request.suite;
         Test_bucket_xml.suite;
         Test_object_request.suite;
         Test_retry.suite;
         Test_paginator.suite;
         Test_multipart_request.suite;
         Test_simulator.suite;
         Test_simulator_contract.suite;
       ])
