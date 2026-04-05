(** Lwt runtime adapter functor. Plug in any [Cohttp_lwt.S.Client].

    Most users should use {!Awskit_lwt_unix} directly.

    {[
    module My_runtime = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
    ]} *)

(** Create a Lwt runtime adapter from a Cohttp client module. *)
module Make (Client : Cohttp_lwt.S.Client) : sig
  type t
  (** Connection handle. Create with {!val:create}. *)

  (** [type 'a t = 'a Lwt.t]. *)
  module Runtime :
    Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

  val create :
    ?ctx:Client.ctx ->
    ?scheme:[ `Http | `Https ] ->
    region:string ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?endpoint:string ->
    ?port:int ->
    unit ->
    t
  (** [scheme] defaults to [`Https]. [endpoint] and [port] override for
      LocalStack, MinIO, etc. *)
end
