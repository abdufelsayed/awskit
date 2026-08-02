(** Runtime-neutral observability values and extension contracts.

    Applications normally configure observers through [Awskit_lwt.Observability]
    or [Awskit_eio.Observability]. The [For_service], [For_runtime], and
    [For_projection] modules are explicit expert roles for service authors,
    runtime authors, and telemetry adapters. *)

module Outcome : sig
  (** Bounded terminal outcome shared by logs, metrics, and traces. *)

  type t =
    | Ok
    | Not_found
    | Conflict
    | Throttled
    | Error
    | Exception
    | Cancelled
    | Timeout

  val to_string : t -> string
  val of_error : Error.t -> t
  val pp : t Fmt.t
end

module Span_kind : sig
  (** Runtime-neutral trace span role. *)

  type t = Internal | Client
end

module Sources : sig
  (** Logs sources owned by the core HTTP and credential domains. *)

  val http : Logs.src
  val credentials : Logs.src
end

module Diagnostic : sig
  module Public : sig
    (** A diagnostic reviewed as safe for Logs and trace callbacks.

        Public diagnostics may still be high-cardinality, so metric sinks do not
        receive this type. *)

    type value =
      | String of string
      | Bool of bool
      | Int of int
      | Int64 of int64
      | Float of float

    type t

    val name : t -> string
    val value : t -> value
    val pp : t Fmt.t
  end
end

module Correlation : sig
  (** Validated trace-correlation diagnostics. Invalid or all-zero identifiers
      are rejected before they can enter a canonical observation. *)

  val trace_id : string -> (Diagnostic.Public.t, string) result
  val span_id : string -> (Diagnostic.Public.t, string) result
  val trace_flags : int -> (Diagnostic.Public.t, string) result
end

module Health : sig
  (** Observer-local, bounded projection failure counters. *)

  module Projection : sig
    type kind = Logs | Metric | Trace | Engine
    type t

    val id : t -> int
    val kind : t -> kind
    val name : t -> string
  end

  type phase =
    | Enablement
    | Start
    | Finish
    | Event
    | Instrument
    | Context  (** Projection phase in which a contained failure occurred. *)

  type failure
  type snapshot

  val projection : failure -> Projection.t
  val phase : failure -> phase
  val count : failure -> int64
  val projections : snapshot -> Projection.t list
  val failures : snapshot -> failure list
end

module For_projection : sig
  (** Read-only safe views for Logs, metrics, tracing, and telemetry bridges.

      Operation and event diagnostics are public-only. Metric observations
      expose exact labels and a number, with no diagnostic accessor. *)

  module Dimension : sig
    type t

    val name : t -> string
    val value : t -> string
  end

  module Measurement : sig
    type value = Int of int | Int64 of int64 | Float of float
    type t

    val name : t -> string
    val unit_ : t -> string option
    val value : t -> value
  end

  module Operation : sig
    module Info : sig
      type t

      val name : t -> string
      val doc : t -> string
      val source : t -> Logs.src
      val span_kind : t -> Span_kind.t
    end

    module Start : sig
      type t

      val info : t -> Info.t
      val dimensions : t -> Dimension.t list
      val measurements : t -> Measurement.t list
      val diagnostics : t -> Diagnostic.Public.t list
    end

    module Completion : sig
      type t

      val info : t -> Info.t
      val outcome : t -> Outcome.t
      val duration_ns : t -> int64 option
      val dimensions : t -> Dimension.t list
      val measurements : t -> Measurement.t list
      val diagnostics : t -> Diagnostic.Public.t list
      val pp : t Fmt.t
    end
  end

  module Event : sig
    module Info : sig
      type t

      val name : t -> string
      val doc : t -> string
      val source : t -> Logs.src
    end

    type t

    val info : t -> Info.t
    val dimensions : t -> Dimension.t list
    val measurements : t -> Measurement.t list
    val diagnostics : t -> Diagnostic.Public.t list
    val pp : t Fmt.t
  end

  module Metric : sig
    module Label : sig
      type t
      type value

      val name : t -> string
      val allowed_values : t -> string list
      val label : value -> t
      val encoded : value -> string
    end

    module Family : sig
      type aggregation = Counter | Histogram | Gauge
      type number = Int | Int64 | Float
      type t

      val id : t -> int
      val name : t -> string
      val doc : t -> string
      val unit_ : t -> string option
      val aggregation : t -> aggregation
      val number : t -> number
      val labels : t -> Label.t list
      val equal : t -> t -> bool
    end

    module Value : sig
      type t = Int of int | Int64 of int64 | Float of float
    end

    module Observation : sig
      type t

      val family : t -> Family.t
      val labels : t -> Label.value list
      val value : t -> Value.t
    end
  end
end

module Metric_sink : sig
  (** Application-supplied projection for exact metric observations. *)

  type t

  val create :
    name:string ->
    needs_clock:bool ->
    enabled:(For_projection.Metric.Family.t -> bool) ->
    observe:(For_projection.Metric.Observation.t -> unit) ->
    t
  (** [create] constructs a synchronous, failure-contained sink. [enabled]
      receives family metadata only. [needs_clock] requires observers using the
      sink to supply a monotonic nanosecond clock. *)
end

module Logs_tags : sig
  (** Typed values attached to records emitted by the built-in Logs projection.
  *)

  val operation_completion : For_projection.Operation.Completion.t Logs.Tag.def
  val event : For_projection.Event.t Logs.Tag.def
end

module For_service : sig
  (** Expert authoring contract for Awskit service packages.

      Definitions live beside their domain lifecycle. Production call sites
      should invoke small semantic wrappers, rather than construct fields,
      choose log levels, or manipulate observation scopes. *)

  type ('start, 'finish) operation_completion
  (** Typed completion supplied to the operation's domain-owned log and metric
      policies. This view exists before generic projection fields are encoded.
  *)

  module Dimension : sig
    type t = For_projection.Dimension.t

    module Enum : sig
      type 'a t

      val define :
        name:string ->
        equal:('a -> 'a -> bool) ->
        values:('a * string) list ->
        'a t

      val value : 'a t -> 'a -> For_projection.Dimension.t
      val cardinality : 'a t -> int
    end
  end

  module Measurement : sig
    type t

    val int : ?unit_:string -> name:string -> int -> t
    val int64 : ?unit_:string -> name:string -> int64 -> t
    val float : ?unit_:string -> name:string -> float -> t
  end

  module Diagnostic : sig
    (** Allowlisted diagnostics for canonical observations.

        Constructors fix both the diagnostic name and its validation policy.
        They return [None] for invalid values so untrusted provider metadata is
        omitted. There is deliberately no constructor for arbitrary text, human
        errors, exceptions, URLs, headers, credentials, authorization material,
        tokens, or signatures. *)

    type t

    val attempt_number : int -> t option
    (** A strictly positive physical-attempt number. *)

    val http_status_code : int -> t option
    (** An HTTP status code in the inclusive range 100 to 599. *)

    val aws_request_id : string -> t option
    (** An opaque, nonempty printable-ASCII AWS request identifier of at most
        1024 bytes. *)

    val aws_extended_request_id : string -> t option
    (** An opaque, nonempty printable-ASCII AWS extended request identifier of
        at most 1024 bytes. *)

    val aws_region : string -> t option
    (** An opaque, nonempty printable-ASCII AWS region name of at most 1024
        bytes. *)

    val bucket_name : string -> t option
    (** An opaque, nonempty printable-ASCII S3 bucket name of at most 1024
        bytes. *)
  end

  module Fields : sig
    (** Canonical fields. Every diagnostic in this container has already passed
        one of the reviewed constructors above; there is no later sensitive
        filtering or arbitrary-string promotion step. *)

    type t

    val empty : t

    val create :
      ?dimensions:Dimension.t list ->
      ?measurements:Measurement.t list ->
      ?diagnostics:Diagnostic.t list ->
      unit ->
      t
  end

  module Terminal : sig
    type 'a t

    val result : 'a t -> ('a, exn) result
    val default_outcome : 'a t -> Outcome.t
  end

  module Metric : sig
    (** Exact typed metric families and projections. Each family owns one label
        record, so unrelated or absent labels cannot be supplied. *)

    type counter
    type histogram
    type gauge

    module Number : sig
      type _ t = Int : int t | Int64 : int64 t | Float : float t
    end

    module Labels : sig
      type 'labels t

      val empty : unit -> 'labels t

      val add :
        'value Dimension.Enum.t ->
        get:('labels -> 'value) ->
        'labels t ->
        'labels t
    end

    module Family : sig
      type ('aggregation, 'labels, 'value) t

      val counter :
        name:string ->
        doc:string ->
        ?unit_:string ->
        labels:'labels Labels.t ->
        value:'value Number.t ->
        unit ->
        (counter, 'labels, 'value) t

      val histogram :
        name:string ->
        doc:string ->
        ?unit_:string ->
        labels:'labels Labels.t ->
        value:'value Number.t ->
        unit ->
        (histogram, 'labels, 'value) t

      val gauge :
        name:string ->
        doc:string ->
        ?unit_:string ->
        labels:'labels Labels.t ->
        value:'value Number.t ->
        unit ->
        (gauge, 'labels, 'value) t
    end

    module Projection : sig
      type 'input t

      val sample :
        ?needs_duration:bool ->
        ('aggregation, 'labels, 'value) Family.t ->
        get:('input -> ('labels * 'value) option) ->
        'input t
    end
  end

  module Log : sig
    (** Lazy domain-owned Logs policies.

        Operation policies receive the typed completion, and event policies
        receive the typed payload, before either value is encoded into generic
        projection fields. A decision is evaluated only when at least one
        declared level is enabled for the definition's source; its message is
        forced only if Logs accepts the selected level.

        An [Emit] decision must use a level from the policy's declared [levels].
        Emitting an undeclared level is a policy error: the emission is
        suppressed, the failure is counted in observer health, and the SDK call
        is unaffected. *)

    type decision =
      | Skip
      | Emit of { level : Logs.level; message : string Lazy.t }

    type ('start, 'finish) operation_policy
    type 'payload event_policy

    val silent_operation : ('start, 'finish) operation_policy

    val operation :
      levels:Logs.level list ->
      decide:(('start, 'finish) operation_completion -> decision) ->
      ('start, 'finish) operation_policy

    val silent_event : 'payload event_policy

    val event :
      levels:Logs.level list ->
      decide:('payload -> decision) ->
      'payload event_policy
  end

  module Operation : sig
    (** A typed timed operation with one terminal completion. *)

    module Completion : sig
      type ('start, 'finish) t = ('start, 'finish) operation_completion

      val start : ('start, 'finish) t -> 'start
      val finish : ('start, 'finish) t -> 'finish option
      val outcome : ('start, 'finish) t -> Outcome.t
      val duration_ns : ('start, 'finish) t -> int64 option
    end

    type ('start, 'result, 'finish) t

    val define :
      name:string ->
      doc:string ->
      source:Logs.src ->
      span_kind:Span_kind.t ->
      start:('start -> Fields.t) ->
      classify:('result Terminal.t -> Outcome.t * 'finish) ->
      finish:('finish -> Fields.t) ->
      log:('start, 'finish) Log.operation_policy ->
      metrics:('start, 'finish) Completion.t Metric.Projection.t list ->
      unit ->
      ('start, 'result, 'finish) t

    val project :
      ('start, 'result, 'finish) t ->
      start:'start ->
      result:('result, exn) result ->
      default_outcome:Outcome.t ->
      duration_ns:int64 option ->
      For_projection.Operation.Completion.t
    (** Purely classify and encode one service-domain completion. This does not
        create an observer, activate context, emit a signal, or imply runtime
        execution. It is intended for service-owned simulators that need the
        same safe terminal shape as their network client. *)
  end

  module Event : sig
    (** A discrete fact that is not an operation completion or owned state. *)

    type 'payload t

    val define :
      name:string ->
      doc:string ->
      source:Logs.src ->
      fields:('payload -> Fields.t) ->
      log:'payload Log.event_policy ->
      metrics:'payload Metric.Projection.t list ->
      unit ->
      'payload t
  end

  module Instrument : sig
    (** Lifecycle-owned gauge definition. *)

    type 'labels t

    val define :
      family:(Metric.gauge, 'labels, int64) Metric.Family.t -> 'labels t
  end

  module Credential_resolution : sig
    type finish

    val operation : (unit, (Credentials.t, Error.t) result, finish) Operation.t
  end

  module type Observer = sig
    (** Minimum execution capability supplied by a runtime package to service
        semantic wrappers. *)

    type +'a io
    type connection
    type lease

    val with_operation :
      connection ->
      operation:(unit -> ('start, 'result, 'finish) Operation.t) ->
      start:(unit -> 'start) ->
      (unit -> 'result io) ->
      'result io

    val emit_event :
      connection -> 'payload Event.t -> data:(unit -> 'payload) -> unit

    val acquire :
      connection ->
      'labels Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      lease

    val add : lease -> int64 -> unit
    val release : lease -> unit

    val with_instrument :
      connection ->
      'labels Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      (unit -> 'a io) ->
      'a io
  end
end

module For_runtime : sig
  (** Runtime context and semantic observer role used by Lwt, Eio, and
      synchronous contract runtimes. The lifecycle engine and its scopes are
      deliberately private to the functor result. *)

  module type Context = sig
    type +'a io
    type 'a key

    val create : unit -> 'a key
    val get : 'a key -> 'a option
    val with_binding : 'a key -> 'a -> (unit -> 'b io) -> 'b io
    val bind : 'a io -> ('a -> 'b io) -> 'b io
    val return : 'a -> 'a io
    val fail : exn -> 'a io
    val capture : (unit -> 'a io) -> ('a, exn) Result.t io

    val finalize : (unit -> 'a io) -> (('a, exn) Result.t -> unit) -> 'a io
    (** Run the callback and invoke the hook exactly once with its real result.
        A hook failure must not replace that result, including native runtime
        cancellation. *)

    val raised_outcome : exn -> Outcome.t
  end

  module type Trace_sink = sig
    (** Runtime-specific trace delivery used by the semantic observer. Trace
        context activation is the only asynchronous part of the sink; the
        projection engine remains synchronous and failure-contained. *)

    type +'a io
    type t
    type activation

    val name : t -> string
    val needs_clock : t -> bool
    val enabled : t -> For_projection.Operation.Info.t -> bool
    val start : t -> For_projection.Operation.Start.t -> activation
    val correlation : activation -> Diagnostic.Public.t list

    val within : activation -> (unit -> 'a io) -> 'a io
    (** Run the supplied SDK callback with this activation's trace context
        installed as current, so application spans emitted inside the callback
        nest under this span.

        The callback must be invoked at most once, and its result must be
        returned unchanged: do not store, replay, map, or rebuild it. Awskit
        defends itself against a misbehaving wrapper — a repeated, skipped, or
        substituted callback and a raised exception are contained, counted in
        observer health, and the SDK always proceeds with the callback's real
        result — but it cannot defend against a wrapper that never returns,
        which stalls the SDK operation. *)

    val finish : activation -> For_projection.Operation.Completion.t -> unit
    val event_enabled : t -> For_projection.Event.Info.t -> bool
    val event : t -> For_projection.Event.t -> unit
  end

  module Make
      (Context : Context)
      (Trace_sink : Trace_sink with type 'a io = 'a Context.io) : sig
    type t
    type lease

    val default : unit -> t
    val none : t

    val create :
      ?logs:bool ->
      ?clock:(unit -> int64) ->
      ?metric_sinks:Metric_sink.t list ->
      ?trace_sinks:Trace_sink.t list ->
      unit ->
      t

    val health : t -> Health.snapshot
    val snapshot : t -> For_projection.Metric.Observation.t list

    val with_operation :
      t ->
      operation:(unit -> ('start, 'result, 'finish) For_service.Operation.t) ->
      start:(unit -> 'start) ->
      (unit -> 'result Context.io) ->
      'result Context.io

    val emit_event :
      t -> 'payload For_service.Event.t -> data:(unit -> 'payload) -> unit

    val acquire :
      t ->
      'labels For_service.Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      lease

    val add : lease -> int64 -> unit
    val release : lease -> unit

    val with_instrument :
      t ->
      'labels For_service.Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      (unit -> 'a Context.io) ->
      'a Context.io
  end

  module Http : sig
    (** Definitions and owned-state instruments for physical HTTP operations and
        the adapter-owned phase boundaries Awskit can directly observe. *)

    type replayability = Replayable | Non_replayable
    type request_start
    type request_finish
    type response
    type request_stats
    type phase_start
    type phase_finish
    type request_state
    type streaming_direction = Request | Response
    type streaming_state

    val request_start :
      method_:Request.Method.t -> replayability:replayability -> request_start

    val response :
      status:int -> ?request_id:string -> ?host_id:string -> unit -> response

    val request_stats :
      connector_request_bytes:int64 option ->
      connector_response_bytes:int64 ->
      connector_drained_bytes:int64 ->
      request_stats
    (** Per-attempt bytes observed at the configured connector boundary.

        [connector_request_bytes] is absent for native static Lwt and Eio bodies
        because their connectors do not expose a pull count.
        [connector_response_bytes] counts caller-consumed response pulls;
        [connector_drained_bytes] counts cleanup or explicit-discard pulls. None
        of these values claims socket or wire transmission. *)

    val request :
      response:(unit -> response option) ->
      stats:(unit -> request_stats) ->
      ( request_start,
        ('a, Error.t) result,
        request_finish )
      For_service.Operation.t

    val phase_start : method_:Request.Method.t -> phase_start

    val request_body_production :
      bytes:(unit -> int64) ->
      (phase_start, ('a, Error.t) result, phase_finish) For_service.Operation.t

    val response_headers_wait :
      unit ->
      (phase_start, ('a, Error.t) result, phase_finish) For_service.Operation.t

    val response_body_consumption :
      bytes:(unit -> int64) ->
      (phase_start, ('a, Error.t) result, phase_finish) For_service.Operation.t

    val response_body_drain :
      bytes:(unit -> int64) ->
      (phase_start, ('a, Error.t) result, phase_finish) For_service.Operation.t

    val attempts_in_flight : request_state For_service.Instrument.t
    val request_state : method_:Request.Method.t -> request_state
    val streaming_bytes_in_flight : streaming_state For_service.Instrument.t
    val streaming_state : streaming_direction -> streaming_state
  end
end
