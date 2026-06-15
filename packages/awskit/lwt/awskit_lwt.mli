(** Lwt runtime adapter functor. Plug in any [Cohttp_lwt.S.Client].

    Most users should use [Awskit_lwt_unix] directly.

    {[
    module My_runtime = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
    ]} *)

module Credentials : sig
  module Provider : sig
    type credentials = Awskit.Credentials.t
    (** AWS credentials resolved by this provider. *)

    type t
    (** Asynchronous credential provider. *)

    val create : (unit -> (credentials, Awskit.Error.t) result Lwt.t) -> t
    (** Wrap an asynchronous credential lookup function. *)

    val resolve : t -> (credentials, Awskit.Error.t) result Lwt.t
    (** Resolve credentials. *)

    val static : credentials -> t
    (** Provider that always returns the same credentials. *)

    val chain : t list -> t
    (** Try providers in order and return the first successful credentials. *)
  end
end

(** Create a Lwt runtime adapter from a Cohttp client module. *)
module Make (Client : Cohttp_lwt.S.Client) : sig
  type t
  (** Connection handle. Create with {!val:create}. *)

  (** [type 'a t = 'a Lwt.t]. *)
  module Runtime :
    Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

  val create :
    ?ctx:Client.ctx ->
    ?endpoint:string ->
    region:string ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?max_response_drain_bytes:int ->
    unit ->
    (t, Awskit.Error.t) result
  (** [endpoint] overrides the default AWS HTTPS endpoint for local test
      services or custom service endpoints. [region] and [endpoint] are parsed
      and validated when the connection is created; validation failures are
      returned as structured [Awskit.Error.t] values. [retry_policy] defaults to
      [Awskit.Retry.default]. [sleep] is used between retries and defaults to no
      delay for custom Lwt backends. [max_response_drain_bytes] defaults to 64
      MiB. If a response consumer succeeds but the remaining body exceeds this
      drain limit, the operation fails with a body-limit error. If the consumer
      fails, the consumer error is returned. *)

  val create_with_credentials_provider :
    ?ctx:Client.ctx ->
    ?endpoint:string ->
    region:string ->
    credentials_provider:Credentials.Provider.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?max_response_drain_bytes:int ->
    unit ->
    (t, Awskit.Error.t) result
  (** Like {!val:create}, but resolves credentials through a provider for each
      signed request. Use this for refreshable credential sources. *)
end
