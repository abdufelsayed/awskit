val with_connection :
  < net : _ Eio.Net.t ; clock : _ Eio.Time.clock ; .. > ->
  ?status:int ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?responses:(int * (string * string) list * string) list ->
  ?response_delay:float ->
  ?on_request:(string -> (string * string) list -> string -> unit) ->
  ?observability:Awskit_eio.Observability.t ->
  (Awskit_s3_eio.t -> calls:int Atomic.t -> 'a) ->
  'a

val with_connection_without_server :
  < net : _ Eio.Net.t ; clock : _ Eio.Time.clock ; .. > ->
  ?observability:Awskit_eio.Observability.t ->
  (Awskit_s3_eio.t -> calls:int Atomic.t -> 'a) ->
  'a

val get :
  Awskit_s3_eio.t -> (string Awskit_s3.Object.Get.result, Awskit.Error.t) result

val get_value : Awskit_s3_eio.t -> (string, Awskit.Error.t) result
