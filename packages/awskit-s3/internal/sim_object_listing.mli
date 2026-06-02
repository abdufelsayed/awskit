val page :
  default_max_keys:int ->
  bucket:string ->
  Sim_state.bucket_state ->
  Object.List.options ->
  response:Awskit.Response.t ->
  Object.List.page
