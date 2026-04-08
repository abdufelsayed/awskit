(** Eio runtime adapter for AWS. Thin wrapper over the pure awskit package. *)

module Runtime = Runtime

type t = Runtime.conn

let create = Runtime.create
