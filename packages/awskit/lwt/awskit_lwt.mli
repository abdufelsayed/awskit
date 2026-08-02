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
    (** Asynchronous credential provider. Native [Lwt.Canceled] from the lookup
        function is preserved. *)

    val create : (unit -> resolution Lwt.t) -> t
    (** Wrap an asynchronous credential lookup function. The function is called
        each time {!val:resolve} is called. *)

    val resolve : t -> resolution Lwt.t
    (** Resolve credentials from the provider. *)

    val static : credentials -> t
    (** Provider that always returns the same credentials. *)

    val chain : t list -> t
    (** Try providers in order. The chain continues only when a provider returns
        [Unavailable]; [Resolved], [Invalid], and [Failed] stop resolution. *)

    val source_label : source -> string
    (** Stable, human-readable label for a credential provider source. *)
  end
end

module Observability : sig
  (** Per-client Lwt observability configuration. *)

  module Trace_sink : sig
    (** Safe trace adapter with promise-local context activation. *)

    type activation = {
      within : 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t;
          (** Install trace context around the supplied SDK callback.

              Invoke the callback at most once and return its result unchanged:
              do not store, replay, map, or rebuild it. Awskit defends callback
              invocation and result semantics when this wrapper misbehaves — a
              repeated, skipped, or substituted callback and a raised exception
              are contained and counted in observer health — but trusts the
              wrapper to remain live: a wrapper that does not return can stall
              the SDK operation even if it already invoked the callback. *)
      correlation : Awskit.Observability.Diagnostic.Public.t list;
          (** Validated public trace-correlation diagnostics. *)
      finish :
        Awskit.Observability.For_projection.Operation.Completion.t -> unit;
          (** Close the trace operation from its safe terminal completion. *)
    }

    type t

    val create :
      name:string ->
      needs_clock:bool ->
      enabled:(Awskit.Observability.For_projection.Operation.Info.t -> bool) ->
      start:
        (Awskit.Observability.For_projection.Operation.Start.t -> activation) ->
      event_enabled:(Awskit.Observability.For_projection.Event.Info.t -> bool) ->
      event:(Awskit.Observability.For_projection.Event.t -> unit) ->
      t
    (** Construct a synchronous, failure-contained trace sink. *)
  end

  type t

  val default : unit -> t
  (** Fresh observer with the built-in Logs projection enabled. *)

  val none : t
  (** Shared hard-off observer with no callbacks, clock reads, or health
      mutation. *)

  val create :
    ?logs:bool ->
    ?clock:(unit -> int64) ->
    ?metric_sinks:Awskit.Observability.Metric_sink.t list ->
    ?trace_sinks:Trace_sink.t list ->
    unit ->
    t
  (** Create fresh observer-local state. [logs] defaults to [true]. Supply a
      monotonic nanosecond [clock] whenever a configured sink requires one. *)

  val health : t -> Awskit.Observability.Health.snapshot
  (** Inspect contained projection failures. Observer values own no exporter or
      reporter resources and therefore have no shutdown operation. *)

  val instrument_snapshot :
    t -> Awskit.Observability.For_projection.Metric.Observation.t list
  (** Read current owned-state gauges for application-controlled polling. *)
end

module For_connector : sig
  (** Expert connector capability for runtimes that own the physical response
      connection. [call] returns a private in-flight handle immediately, before
      response headers are available, and [response] awaits those headers. The
      runtime can therefore abort a request that is still connecting or waiting
      for response headers. It awaits the idempotent abort before returning the
      physical attempt while preserving the primary SDK result, exception, or
      native [Lwt.Canceled]. *)

  module type S = sig
    module Client : Cohttp_lwt.S.Client

    type call

    val call :
      ?ctx:Client.ctx ->
      headers:Cohttp.Header.t ->
      body:Cohttp_lwt.Body.t ->
      Cohttp.Code.meth ->
      Uri.t ->
      call

    val response : call -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

    val abort : call -> unit Lwt.t
    (** [abort] is idempotent, closes or abandons the physical call, and
        completes before runtime cancellation or cleanup is released. Its
        failure must not replace the primary SDK result. *)
  end

  module Make (Connector : S) : sig
    type t

    module Runtime :
      Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

    module Runtime_observer :
      Awskit.Observability.For_service.Observer
        with type 'a io = 'a Lwt.t
         and type connection = t

    val create :
      ?ctx:Connector.Client.ctx ->
      ?endpoint:string ->
      region:string ->
      credentials:Awskit.Credentials.t ->
      clock:(unit -> Ptime.t) ->
      ?retry_policy:Awskit.Retry.t ->
      ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
      ?random_float:(upper_bound:float -> float) ->
      ?timeout_policy:Awskit.Timeout.policy ->
      ?max_response_drain_bytes:int ->
      ?observability:Observability.t ->
      unit ->
      (t, Awskit.Error.t) result

    val create_with_credentials_provider :
      ?ctx:Connector.Client.ctx ->
      ?endpoint:string ->
      region:string ->
      credentials_provider:Credentials.Provider.t ->
      clock:(unit -> Ptime.t) ->
      ?retry_policy:Awskit.Retry.t ->
      ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
      ?random_float:(upper_bound:float -> float) ->
      ?timeout_policy:Awskit.Timeout.policy ->
      ?max_response_drain_bytes:int ->
      ?observability:Observability.t ->
      unit ->
      (t, Awskit.Error.t) result
  end
end

(** Create a Lwt runtime adapter from a Cohttp client module. *)
module Make (Client : Cohttp_lwt.S.Client) : sig
  type t
  (** Connection handle. Create with {!val:create}. *)

  (** [type 'a t = 'a Lwt.t]. *)
  module Runtime :
    Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

  (** Expert service-package observation port bound to this runtime. *)
  module Runtime_observer :
    Awskit.Observability.For_service.Observer
      with type 'a io = 'a Lwt.t
       and type connection = t

  val create :
    ?ctx:Client.ctx ->
    ?endpoint:string ->
    region:string ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?random_float:(upper_bound:float -> float) ->
    ?timeout_policy:Awskit.Timeout.policy ->
    ?max_response_drain_bytes:int ->
    ?observability:Observability.t ->
    unit ->
    (t, Awskit.Error.t) result
  (** [endpoint] overrides the default AWS HTTPS endpoint for local test
      services or custom service endpoints. [region] and [endpoint] are parsed
      and validated when the connection is created; validation failures are
      returned as structured [Awskit.Error.t] values. Invalid
      [max_response_drain_bytes] values are also returned as structured
      validation errors. [retry_policy] defaults to [Awskit.Retry.default]. When
      retries are enabled, custom Lwt backends must provide both [sleep] and
      [random_float]; use [Awskit.Retry.disabled] for deterministic runtimes
      without real delay/random capabilities. [timeout_policy] defaults to
      [Awskit.Timeout.default] when [sleep] is supplied and to
      [Awskit.Timeout.disabled] otherwise. Passing an explicit timeout policy
      with configured spans requires [sleep], which this generic runtime uses as
      its timeout clock. [max_response_drain_bytes] defaults to 64 MiB. If a
      response consumer succeeds but the remaining body exceeds this drain
      limit, the operation fails with a body-limit error. If the consumer fails,
      the consumer error is returned. Response body read timeouts invalidate the
      reader; native [Lwt.Canceled] from user callbacks and body reads is
      preserved. The generic [Cohttp_lwt.S.Client] contract exposes no owned
      in-flight handle to abort. The functor protects the connector response
      while cleanup runs, but it cannot promptly stop a [Client.call] that
      ignores Lwt cancellation before headers arrive, nor can it guarantee that
      an abandoned response connection is reusable. Use [For_connector.Make]
      when the connector can close an owned in-flight call. [operation] timeouts
      in [timeout_policy] apply to one runtime transport operation;
      service-level retries get a fresh operation timer for each attempt. *)

  val create_with_credentials_provider :
    ?ctx:Client.ctx ->
    ?endpoint:string ->
    region:string ->
    credentials_provider:Credentials.Provider.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?random_float:(upper_bound:float -> float) ->
    ?timeout_policy:Awskit.Timeout.policy ->
    ?max_response_drain_bytes:int ->
    ?observability:Observability.t ->
    unit ->
    (t, Awskit.Error.t) result
  (** Like {!val:create}, but resolves credentials through a provider for each
      signed request. Use this for refreshable credential sources, including
      provider chains that may return [Unavailable], [Invalid], or [Failed]. *)
end
