(** Eio runtime adapter for AWS. Thin wrapper over the pure aws package. *)

module Runtime = Runtime

type t = Runtime.conn

let create = Runtime.create
