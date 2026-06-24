(** Internal simulator service-error and fault helpers. *)

val service :
  ?message:string ->
  ?headers:(string * string) list ->
  status:int ->
  code:string ->
  unit ->
  Awskit.Error.t

val no_such_bucket : unit -> Awskit.Error.t
val no_such_key : unit -> Awskit.Error.t
val no_such_upload : unit -> Awskit.Error.t

val method_not_allowed :
  ?headers:(string * string) list -> unit -> Awskit.Error.t

val precondition_failed : unit -> Awskit.Error.t
val not_modified : unit -> Awskit.Error.t
val response : ?headers:(string * string) list -> int -> Awskit.Response.t

val record :
  ?faulted:bool ->
  Simulator_state.t ->
  Simulator_state.operation ->
  string ->
  string option ->
  unit

val fault_error : Simulator_state.fault -> Awskit.Error.t
val take_fault : Simulator_state.t -> Simulator_state.fault option

val operation_fault :
  Simulator_state.t ->
  Simulator_state.operation ->
  string ->
  string option ->
  Awskit.Error.t option
