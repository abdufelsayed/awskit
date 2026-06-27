(** Eio runtime adapter for AWS. Thin wrapper over the pure awskit package. *)

module Runtime = Runtime

type t = Runtime.conn
type 'flow https = 'flow Runtime.https

let http_only = Runtime.http_only
let create = Runtime.create
