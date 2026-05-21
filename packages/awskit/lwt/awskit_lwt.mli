(** Lwt runtime adapter functor. Plug in any [Cohttp_lwt.S.Client].

    Most users should use [Awskit_lwt_unix] directly.

    {[
    module My_runtime = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
    ]} *)

module Credentials : sig
  module Provider : sig
    type credentials = Awskit.Credentials.t
    type t

    val create : (unit -> (credentials, Awskit.Error.t) result Lwt.t) -> t
    val resolve : t -> (credentials, Awskit.Error.t) result Lwt.t
    val static : credentials -> t
    val chain : t list -> t
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
    ?endpoint:Awskit.Endpoint.t ->
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?max_response_body_bytes:int ->
    unit ->
    t
  (** [endpoint] overrides the default AWS HTTPS endpoint for local test
      services or custom service endpoints. [retry_policy] defaults to
      {!val:Awskit.Retry.default}. [sleep] is used between retries and defaults
      to no delay for custom Lwt backends. [max_response_body_bytes] defaults to
      64 MiB. *)

  val create_with_credentials_provider :
    ?ctx:Client.ctx ->
    ?endpoint:Awskit.Endpoint.t ->
    region:Awskit.Region.t ->
    credentials_provider:Credentials.Provider.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?max_response_body_bytes:int ->
    unit ->
    t
  (** Like {!val:create}, but resolves credentials through a provider for each
      signed request. Use this for refreshable credential sources. *)
end
