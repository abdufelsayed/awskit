let () =
  Eio_main.run @@ fun env ->
  let minio_smoke_requested =
    Array.exists (( = ) "integration:awskit-s3-eio:minio-smoke") Sys.argv
  in
  let minio_smoke_suite =
    if minio_smoke_requested then Test_minio_smoke.suite env else []
  in
  Alcotest.run "awskit-s3-eio"
    (Test_integration.suite env @ Test_transfer.suite env @ minio_smoke_suite)
