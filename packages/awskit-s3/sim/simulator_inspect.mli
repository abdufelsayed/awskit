(** Internal inspection helpers behind {!module:Awskit_s3_sim}. *)

type object_metadata = {
  etag : Awskit_s3.Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

val object_metadata :
  Simulator_state.store -> bucket:string -> key:string -> object_metadata option

val keys : Simulator_state.store -> bucket:string -> string list
(** Return current object keys in lexicographic order. *)

val history : Simulator_state.store -> Simulator_state.operation_record list
(** Return operation history in chronological order. *)

val clear_history : Simulator_state.store -> unit

val objects_as_strings :
  Simulator_state.store -> bucket:string -> (string * string) list
(** Return current object bodies as strings in deterministic key order. *)

val inject_fault : Simulator_state.t -> Simulator_state.fault -> unit
(** Queue one explicit FIFO fault. *)

val inject_faults : Simulator_state.t -> Simulator_state.fault list -> unit
(** Queue explicit faults in FIFO order. *)

val clear_faults : Simulator_state.t -> unit
val enable_random_faults : Simulator_state.t -> seed:int -> prob:float -> unit
val disable_random_faults : Simulator_state.t -> unit

val info_of_object :
  ?content_length:int ->
  Awskit.Response.t ->
  Simulator_state.stored_object ->
  Awskit_s3.Object.Get.info
