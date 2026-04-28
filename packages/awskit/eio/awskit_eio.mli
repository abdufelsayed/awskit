(** Eio runtime adapter. [type 'a t = 'a] (direct-style).

    {[
    Eio.Switch.run @@ fun sw ->
    let region = Awskit.Region.of_string_exn "us-east-1" in
    let conn = Awskit_eio.create ~env ~sw ~region ~credentials ()
    ]} *)

type t = Runtime.conn

module Runtime : Awskit.Runtime.S with type 'a t = 'a and type connection = t

val create :
  env:< net : _ Eio.Net.t ; .. > ->
  sw:Eio.Switch.t ->
  region:Awskit.Region.t ->
  credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?endpoint:Awskit.Endpoint.t ->
  ?max_response_body_bytes:int ->
  unit ->
  t
(** Defaults to AWS HTTPS endpoints. Pass an explicit [endpoint] for LocalStack,
    MinIO, or other S3-compatible services. [max_response_body_bytes] defaults
    to 64 MiB. *)
