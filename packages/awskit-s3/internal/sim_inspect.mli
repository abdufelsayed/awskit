type object_metadata = {
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

val object_metadata :
  Sim_state.store -> bucket:string -> key:string -> object_metadata option

val keys : Sim_state.store -> bucket:string -> string list
val history : Sim_state.store -> Sim_state.operation_record list
val clear_history : Sim_state.store -> unit

val objects_as_strings :
  Sim_state.store -> bucket:string -> (string * string) list

val inject_fault : Sim_state.t -> Sim_state.fault -> unit
val inject_faults : Sim_state.t -> Sim_state.fault list -> unit
val clear_faults : Sim_state.t -> unit
val enable_random_faults : Sim_state.t -> seed:int -> prob:float -> unit
val disable_random_faults : Sim_state.t -> unit

val info_of_object :
  ?content_length:int ->
  Awskit.Response.t ->
  Sim_state.stored_object ->
  Object.Get.result
