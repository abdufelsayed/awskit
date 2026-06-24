(** Eio runtime adapter. [type 'a t = 'a] (direct-style).

    {[
    Eio.Switch.run @@ fun sw ->
    let conn =
      Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
        ~credentials ~endpoint:"http://127.0.0.1:9000" ()
      |> Result.get_ok
    in
    conn
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

(** Direct-style runtime implementation used by service packages. *)
module Runtime : sig
  type conn
  (** Concrete Eio connection record used by {!type:Awskit_eio.t}. *)

  include Awskit.Runtime.S with type 'a t = 'a and type connection = conn
end

type t = Runtime.conn
(** Eio connection handle. Create with {!val:create}. *)

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
  unit ->
  (t, Awskit.Error.t) result
(** Create an Eio connection.

    [https] is forwarded to [Cohttp_eio.Client.make]. Use {!val:http_only} only
    with plain HTTP endpoints, such as local tests; HTTPS endpoints require a
    connector supplied by the application. [clock] defaults to [env#clock].
    Defaults to AWS HTTPS endpoints. Pass an explicit [endpoint] for local test
    services or custom service endpoints. [region] and [endpoint] are parsed and
    validated when the connection is created; validation failures are returned
    as structured [Awskit.Error.t] values. Invalid [max_response_drain_bytes]
    values are also returned as structured validation errors. [retry_policy]
    defaults to [Awskit.Retry.default]. [random_float] defaults to a
    connection-local random state for retry jitter. [timeout_policy] defaults to
    [Awskit.Timeout.default]. [max_response_drain_bytes] defaults to 64 MiB. If
    a response consumer succeeds but the remaining body exceeds this drain
    limit, the operation fails with a body-limit error. If the consumer fails,
    the consumer error is returned. [operation] timeouts in [timeout_policy]
    apply to one runtime transport operation; service-level retries get a fresh
    operation timer for each attempt. *)
