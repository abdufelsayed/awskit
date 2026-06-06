open Awskit_s3

val delete_result :
  ?delete_marker:bool ->
  ?version_id:Object.Version_id.t ->
  unit ->
  Object.Delete.result

val delete_objects_error :
  string -> string -> string -> Object.Delete_many.item_error

val delete_objects_conditions_match :
  Object.Delete_many.object_ -> Simulator_state.stored_version option -> bool

val delete :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Object.Delete.options ->
  unit ->
  (Object.Delete.result, Awskit.Error.t) result

val delete_objects :
  Simulator_state.t ->
  bucket:string ->
  objects:Object.Delete_many.object_ list ->
  ?options:Object.Delete_many.options ->
  unit ->
  (Object.Delete_many.result, Awskit.Error.t) result
