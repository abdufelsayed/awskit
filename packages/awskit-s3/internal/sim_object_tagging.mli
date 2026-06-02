module Tagging : sig
  val get :
    Sim_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    unit ->
    (Object.Tagging.result, Awskit.Error.t) result

  val put :
    Sim_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    Tag.t list ->
    (Awskit.Response.t, Awskit.Error.t) result

  val delete :
    Sim_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result
end
