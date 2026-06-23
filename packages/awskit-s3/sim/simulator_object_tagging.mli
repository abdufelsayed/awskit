open Awskit_s3

module Tagging : sig
  val get :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    unit ->
    (Object.Tagging.result, Awskit.Error.t) result

  val put :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    tags:Tag.Set.t ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result

  val delete :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?options:Object.Tagging.options ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result
end
