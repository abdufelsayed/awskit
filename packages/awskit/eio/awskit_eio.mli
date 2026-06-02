(** Eio runtime adapter. [type 'a t = 'a] (direct-style).

    {[
    Eio.Switch.run @@ fun sw ->
    let region = Awskit.Region.of_string_exn "us-east-1" in
    let conn = Awskit_eio.create ~env ~sw ~region ~credentials ()
    ]} *)

type t = Runtime.conn

module Runtime : Awskit.Runtime.S with type 'a t = 'a and type connection = t

val create :
  env:< clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  sw:Eio.Switch.t ->
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?endpoint:Awskit.Endpoint.t ->
  ?max_response_drain_bytes:int ->
  unit ->
  t
(** Defaults to AWS HTTPS endpoints. Pass an explicit [endpoint] for local test
    services or custom service endpoints. [retry_policy] defaults to
    {!val:Awskit.Retry.default}. [max_response_drain_bytes] defaults to 64 MiB.
    If a response consumer succeeds but the remaining body exceeds this drain
    limit, the operation fails with a body-limit error. If the consumer fails,
    the consumer error is returned. *)
