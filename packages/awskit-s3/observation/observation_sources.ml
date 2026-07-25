let operation =
  Logs.Src.create "awskit.s3.operation" ~doc:"Awskit S3 logical operations"

let attempt =
  Logs.Src.create "awskit.s3.attempt" ~doc:"Awskit S3 retry attempts"

let signing =
  Logs.Src.create "awskit.s3.signing" ~doc:"Awskit S3 request signing"

let artifact =
  Logs.Src.create "awskit.s3.artifact"
    ~doc:"Awskit S3 presigned-artifact generation"

let artifact_signing =
  Logs.Src.create "awskit.s3.artifact.signing"
    ~doc:"Awskit S3 presigned-artifact signing"

let retry = Logs.Src.create "awskit.s3.retry" ~doc:"Awskit S3 retry decisions"

let transfer =
  Logs.Src.create "awskit.s3.transfer" ~doc:"Awskit S3 high-level transfers"
