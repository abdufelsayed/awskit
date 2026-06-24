module Tagging : sig
  val get :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Awskit_s3.Object.Tagging.options ->
    unit ->
    (Awskit_s3.Object.Tagging.result, Awskit.Error.t) result

  val put :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Awskit_s3.Object.Tagging.options ->
    tags:Awskit_s3.Tag.Set.t ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result

  val delete :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Awskit_s3.Object.Tagging.options ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result
end
