val delete_result :
  ?delete_marker:bool ->
  ?version_id:Awskit_s3.Object.Version_id.t ->
  unit ->
  Awskit_s3.Object.Delete.result

val delete_objects_error :
  Awskit_s3.Object_key.t ->
  string ->
  string ->
  Awskit_s3.Object.Delete_many.item_error

val delete_objects_conditions_match :
  Awskit_s3.Object.Delete_many.object_ ->
  Simulator_state.stored_version option ->
  bool

val delete :
  Simulator_state.t ->
  bucket:string ->
  key:string ->
  ?options:Awskit_s3.Object.Delete.options ->
  unit ->
  (Awskit_s3.Object.Delete.result, Awskit.Error.t) result

val delete_objects :
  Simulator_state.t ->
  bucket:string ->
  objects:Awskit_s3.Object.Delete_many.object_ list ->
  ?options:Awskit_s3.Object.Delete_many.options ->
  unit ->
  (Awskit_s3.Object.Delete_many.result, Awskit.Error.t) result
