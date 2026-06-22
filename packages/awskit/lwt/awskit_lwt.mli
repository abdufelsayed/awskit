(** Lwt runtime adapter functor. Plug in any [Cohttp_lwt.S.Client].

    Most users should use [Awskit_lwt_unix] directly.

    {[
    module My_runtime = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
    ]} *)

module Credentials : sig
  module Provider : sig
    type credentials = Awskit.Credentials.t
    (** AWS credentials resolved by this provider. *)

    type source = Awskit.Credentials.Provider.source
    (** Credential source that produced, skipped, or failed resolution. *)

    type unavailable = { source : source; reason : string }
    (** Provider was not configured or not applicable, so a chain may continue.
    *)

    (** Credential resolution outcome. Chains continue only on [Unavailable]. *)
    type resolution =
      | Resolved of credentials
      | Unavailable of unavailable
      | Invalid of Awskit.Error.t
      | Failed of Awskit.Error.t

    type t
    (** Asynchronous credential provider. *)

    val create : (unit -> resolution Lwt.t) -> t
    (** Wrap an asynchronous credential lookup function. *)

    val resolve : t -> resolution Lwt.t
    (** Resolve credentials. *)

    val static : credentials -> t
    (** Provider that always returns the same credentials. *)

    val chain : t list -> t
    (** Try providers in order. The chain continues only when a provider returns
        [Unavailable]; [Resolved], [Invalid], and [Failed] stop resolution. *)

    val source_label : source -> string
    (** Stable, human-readable label for a credential provider source. *)
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
