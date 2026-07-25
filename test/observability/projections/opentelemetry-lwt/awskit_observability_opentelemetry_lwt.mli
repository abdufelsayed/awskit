(** OpenTelemetry projection for Lwt applications. *)

type t

val setup_ambient_context : unit -> unit
(** Install OpenTelemetry's Lwt-local ambient context. *)

val create :
  ?tracer:Opentelemetry.Tracer.t -> ?meter:Opentelemetry.Meter.t -> unit -> t
(** Construct projections over caller-supplied or dynamically resolved SDK
    providers. This does not configure resources, sampling, exporters, queues,
    flushing, or shutdown. Sensitive diagnostics are never exported. *)

val trace_sink : t -> Awskit_lwt.Observability.Trace_sink.t
val metric_sink : t -> Awskit.Observability.Metric_sink.t
