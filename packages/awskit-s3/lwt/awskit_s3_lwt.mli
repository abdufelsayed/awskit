(** Lwt adapter functor. Compose {!module:Awskit_s3} with {!module:Awskit_lwt}.

    Most Unix users should use {!module:Awskit_s3_lwt_unix} directly.

    {[
    module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)

    let conn =
      S3.create ~region:"us-east-1"
        ~credentials:
          (Awskit_s3.Credentials.make ~access_key_id:"..."
             ~secret_access_key:"..." ())
        ~clock:Ptime_clock.now ()
    ]} *)

module Make (Client : Cohttp_lwt.S.Client) : sig
  type t
  (** S3 connection handle. *)

  (** Underlying Lwt runtime module. *)
  module Runtime :
    Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

  val create :
    ?ctx:Client.ctx ->
    ?endpoint:Awskit_s3.Endpoint.t ->
    region:string ->
    credentials:Awskit_s3.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?max_response_body_bytes:int ->
    unit ->
    t
  (** Create an S3 connection for the given Lwt HTTP client. *)

  include module type of Awskit_s3.Make (Runtime)
end
