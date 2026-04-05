(** Eio runtime adapter. [type 'a t = 'a] (direct-style).

    {[
    let conn = Awskit_eio.create ~env ~region:"us-east-1" ~credentials ()
    ]} *)

type t = Runtime.conn

module Runtime : Awskit.Runtime.S with type 'a t = 'a and type connection = t

val create :
  env:< net : _ Eio.Net.t ; .. > ->
  region:string ->
  credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?endpoint:string ->
  ?port:int ->
  unit ->
  t
