(** Eio-aware OCaml Trace projection for Awskit's typed observations. *)

val install_context : unit -> unit
val sink : Awskit_eio.Observability.Trace_sink.t
