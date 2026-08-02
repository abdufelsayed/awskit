(** Eio runtime adapter. [type 'a t = 'a] (direct-style).

    {[
    Eio.Switch.run @@ fun sw ->
    let conn =
      Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
        ~credentials ~endpoint:"http://127.0.0.1:9000" ()
      |> Result.get_ok
    in
    ignore conn
    ]} *)

type 'flow https =
  (Uri.t -> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r -> 'flow)
  option
  constraint 'flow = [> Eio.Resource.close_ty ] Eio.Flow.two_way
(** HTTPS connector policy forwarded to [Cohttp_eio.Client.make].

    It matches Cohttp Eio's HTTPS policy shape: the application decides how to
    wrap a connected TCP flow for HTTPS, including TLS configuration, CA roots,
    RNG setup, and platform/runtime choices. *)

val http_only : 'flow https
(** Disable HTTPS connections. Use only with plain HTTP endpoints, such as local
    tests. *)

module Observability : sig
  (** Per-client Eio observability configuration. *)

  module Trace_sink : sig
    (** Safe trace adapter with fiber-local context activation. *)

    type activation = {
      within : 'a. (unit -> 'a) -> 'a;
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

(** Direct-style runtime implementation used by service packages. *)
module Runtime : sig
  type conn
  (** Concrete Eio connection record used by {!type:Awskit_eio.t}. The value is
      scoped by the [Eio.Switch.t] supplied to {!val:create}. *)

  include Awskit.Runtime.S with type 'a t = 'a and type connection = conn
end

type t = Runtime.conn
(** Eio connection handle. Create with {!val:create}. *)

(** Expert service-package observation port bound to this runtime. *)
module Runtime_observer :
  Awskit.Observability.For_service.Observer
    with type 'a io = 'a
     and type connection = t

val create :
  env:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  sw:Eio.Switch.t ->
  https:'flow https ->
  region:string ->
  credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?random_float:(upper_bound:float -> float) ->
  ?timeout_policy:Awskit.Timeout.policy ->
  ?endpoint:string ->
  ?max_response_drain_bytes:int ->
  ?observability:Observability.t ->
  unit ->
  (t, Awskit.Error.t) result
(** Create an Eio connection.

    [https] is forwarded to [Cohttp_eio.Client.make]. Use {!val:http_only} only
    with plain HTTP endpoints, such as local tests; HTTPS endpoints require a
    connector supplied by the application. The returned connection must not
    outlive [sw]. Native Eio cancellation is preserved rather than converted
    into an SDK error. [clock] defaults to [env#clock]. Defaults to AWS HTTPS
    endpoints. Pass an explicit [endpoint] for local test services or custom
    service endpoints. [region] and [endpoint] are parsed and validated when the
    connection is created; validation failures are returned as structured
    [Awskit.Error.t] values. Invalid [max_response_drain_bytes] values are also
    returned as structured validation errors. [retry_policy] defaults to
    [Awskit.Retry.default]. [random_float] defaults to a connection-local random
    state for retry jitter. [timeout_policy] defaults to
    [Awskit.Timeout.default]. [max_response_drain_bytes] defaults to 64 MiB. If
    a response consumer succeeds but the remaining body exceeds this drain
    limit, the operation fails with a body-limit error. If the consumer fails,
    the consumer error is returned. Each physical HTTP call runs in a nested Eio
    switch, so bounded body cleanup and switch teardown finish before the
    attempt returns while native cancellation remains native. [operation]
    timeouts in [timeout_policy] apply to one runtime transport operation;
    service-level retries get a fresh operation timer for each attempt. *)
