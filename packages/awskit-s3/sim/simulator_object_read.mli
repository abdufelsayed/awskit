open Awskit_s3

val get :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Object.Get.options ->
  consume:
    (Simulator_runtime.Runtime.response_body_reader ->
    ('a, Awskit.Error.t) result) ->
  unit ->
  ('a Object.Get.result, Awskit.Error.t) result

val find :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Object.Get.options ->
  consume:
    (Simulator_runtime.Runtime.response_body_reader ->
    ('a, Awskit.Error.t) result) ->
  unit ->
  ('a Object.Get.result option, Awskit.Error.t) result

val head :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Object.Head.options ->
  unit ->
  (Object.Head.result, Awskit.Error.t) result

val exists :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  (bool, Awskit.Error.t) result

val get_as_string :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  max_bytes:int64 ->
  ?options:Object.Get.options ->
  unit ->
  (string Object.Get.result, Awskit.Error.t) result

val get_as_bytes :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  max_bytes:int64 ->
  ?options:Object.Get.options ->
  unit ->
  (bytes Object.Get.result, Awskit.Error.t) result
