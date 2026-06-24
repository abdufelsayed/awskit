(** Internal simulator [GetObject] and [HeadObject] operations. *)

val get :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Awskit_s3.Object.Get.options ->
  consume:
    (Simulator_runtime.Runtime.response_body_reader ->
    ('a, Awskit.Error.t) result) ->
  unit ->
  ('a Awskit_s3.Object.Get.result, Awskit.Error.t) result

val find :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Awskit_s3.Object.Get.options ->
  consume:
    (Simulator_runtime.Runtime.response_body_reader ->
    ('a, Awskit.Error.t) result) ->
  unit ->
  ('a Awskit_s3.Object.Get.result option, Awskit.Error.t) result

val head :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Awskit_s3.Object.Head.options ->
  unit ->
  (Awskit_s3.Object.Head.result, Awskit.Error.t) result

val exists :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Awskit_s3.Object.Head.options ->
  unit ->
  (bool, Awskit.Error.t) result
