(** OCaml Metrics projection for Awskit's typed observations. *)

val sources : unit -> Metrics.Src.t list
val enable : unit -> unit
val disable : unit -> unit
val sink : Awskit.Observability.Metric_sink.t

val poll_instruments :
  Awskit.Observability.For_projection.Metric.Observation.t list -> unit
(** Project one application-requested snapshot of owned-state gauges. *)
