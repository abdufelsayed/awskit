type object_metadata = {
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

val object_metadata :
  Simulator_state.store -> bucket:string -> key:string -> object_metadata option

val keys : Simulator_state.store -> bucket:string -> string list
val history : Simulator_state.store -> Simulator_state.operation_record list
val clear_history : Simulator_state.store -> unit

val objects_as_strings :
  Simulator_state.store -> bucket:string -> (string * string) list

val inject_fault : Simulator_state.t -> Simulator_state.fault -> unit
val inject_faults : Simulator_state.t -> Simulator_state.fault list -> unit
val clear_faults : Simulator_state.t -> unit
val enable_random_faults : Simulator_state.t -> seed:int -> prob:float -> unit
val disable_random_faults : Simulator_state.t -> unit

val info_of_object :
  ?content_length:int ->
  Awskit.Response.t ->
  Simulator_state.stored_object ->
  Object.Get.result
