module Tagging : sig
  val get :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?expected_bucket_owner:Awskit_s3.Account_id.t ->
    unit ->
    (Awskit_s3.Object.Tagging.result, Awskit.Error.t) result

  val put :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?expected_bucket_owner:Awskit_s3.Account_id.t ->
    tags:Awskit_s3.Tag.Set.t ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result

  val delete :
    Simulator_state.t ->
    bucket:string ->
    key:string ->
    ?expected_bucket_owner:Awskit_s3.Account_id.t ->
    unit ->
    (Awskit.Response.t, Awskit.Error.t) result
end
