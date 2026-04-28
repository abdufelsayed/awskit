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
    ?endpoint:Awskit.Endpoint.t ->
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?max_response_body_bytes:int ->
    unit ->
    t
  (** [endpoint] overrides the default AWS HTTPS endpoint for LocalStack, MinIO,
      or other S3-compatible services. [max_response_body_bytes] defaults to 64
      MiB. *)
end
