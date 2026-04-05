(** Runtime abstraction for concurrency adapters.

    Implement {!S} once per concurrency model; all service packages work
    automatically. Built-in adapters:
    - [awskit.eio] — [type 'a t = 'a] (Eio, OCaml 5)
    - [awskit.lwt] — [type 'a t = 'a Lwt.t]

    [connection] bundles region, credentials, clock, and optional endpoint
    overrides. [call] is the only IO operation. *)

module type S = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  type connection

  val region : connection -> string
  val credentials : connection -> Credentials.t

  val clock : connection -> Ptime.t
  (** Current time. Used for signing timestamps and presigned URL expiry. *)

  val endpoint_host : connection -> string option
  (** Override for LocalStack, MinIO, etc. *)

  val endpoint_port : connection -> int option

  val call : connection -> Request.t -> (Response.t, Error.base) result t
  (** The only IO operation. Adapters handle HTTP transport — TLS, connection
      management, and error mapping are the adapter's responsibility. *)
end
