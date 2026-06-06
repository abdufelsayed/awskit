val page :
  default_max_keys:int ->
  bucket:string ->
  Simulator_state.bucket_state ->
  Object.List.options ->
  response:Awskit.Response.t ->
  Object.List.page
