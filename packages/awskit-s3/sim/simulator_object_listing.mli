val page :
  default_max_keys:int ->
  bucket:string ->
  Simulator_state.bucket_state ->
  Awskit_s3.Object.List.options ->
  response:Awskit.Response.t ->
  Awskit_s3.Object.List.page
