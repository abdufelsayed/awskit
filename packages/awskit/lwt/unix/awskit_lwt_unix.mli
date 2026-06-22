(** Lwt + Unix runtime adapter for AWS.

    Ready-to-use adapter for Lwt applications on Unix. Uses
    [Cohttp_lwt_unix.Client] for HTTP — just create a connection and start
    making AWS calls:

    {[
      let conn =
        Awskit_lwt_unix.create ~region:"us-east-1" ~credentials ()
        |> Result.get_ok
      in
      (* pass [conn] to any service package, e.g. Awskit_s3_lwt_unix *)
    ]}

    For MirageOS or custom Lwt HTTP backends, use the generic [Awskit_lwt.Make]
    functor instead. *)

type t
(** Connection handle. Create with {!val:create}, then pass to service
    operations. *)

(** Runtime module satisfying [Awskit.Runtime.S]. The monad is [Lwt.t]. *)
module Runtime :
  Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

module Credentials : sig
  module Provider = Awskit_lwt.Credentials.Provider

  type http_response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  type http_call =
    meth:Cohttp.Code.meth ->
    headers:(string * string) list ->
    Uri.t ->
    (http_response, Awskit.Error.t) result Lwt.t

  type imdsv1_fallback = [ `Enabled | `Disabled ]

  val local_provider :
    ?getenv:Awskit_unix.Credentials.Env.getenv ->
    ?home:string ->
    unit ->
    Provider.t
  (** Static AWS environment variables, then shared AWS profile files. *)

  val container_provider :
    ?getenv:Awskit_unix.Credentials.Env.getenv ->
    ?http_call:http_call ->
    ?clock:(unit -> Ptime.t) ->
    unit ->
    Provider.t
  (** ECS/container credential provider. Supports
      [AWS_CONTAINER_CREDENTIALS_RELATIVE_URI],
      [AWS_CONTAINER_CREDENTIALS_FULL_URI], [AWS_CONTAINER_AUTHORIZATION_TOKEN],
      and [AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE]. *)

  val instance_metadata_provider :
    ?getenv:Awskit_unix.Credentials.Env.getenv ->
    ?http_call:http_call ->
    ?clock:(unit -> Ptime.t) ->
    ?imdsv1_fallback:imdsv1_fallback ->
    unit ->
    Provider.t
  (** EC2 instance profile credential provider using IMDSv2 when available.
      Tokenless IMDSv1 fallback is attempted only for IMDS token endpoint HTTP
      403, 404, or 405 responses. Set [imdsv1_fallback] to [`Disabled] or
      [AWS_EC2_METADATA_V1_DISABLED=true] to reject tokenless fallback. *)

  val default_provider :
    ?getenv:Awskit_unix.Credentials.Env.getenv ->
    ?home:string ->
    ?http_call:http_call ->
    ?clock:(unit -> Ptime.t) ->
    ?imdsv1_fallback:imdsv1_fallback ->
    unit ->
    Provider.t
  (** AWS-style Unix credential chain: local static sources, container
      credentials, then EC2 instance profile credentials. *)
end

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?endpoint:string ->
  ?region:string ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?retry_policy:Awskit.Retry.t ->
  ?random_float:(upper_bound:float -> float) ->
  ?timeout_policy:Awskit.Timeout.policy ->
  ?max_response_drain_bytes:int ->
  ?imdsv1_fallback:Credentials.imdsv1_fallback ->
  unit ->
  (t, Awskit.Error.t) result
(** Create a connection to AWS.

    @param ctx Optional Cohttp client context (e.g., for custom TLS config)
    @param endpoint
      Explicit endpoint override for local test services or custom service
      endpoints. Parsed as [http://] or [https://] endpoint URL.
    @param region
      AWS region (e.g., ["us-east-1"]). If omitted, reads [AWS_REGION] and then
      [AWS_DEFAULT_REGION].
    @param credentials
      AWS credentials for request signing. If omitted, resolves the
      [awskit-unix] default credential chain: static AWS environment variables,
      shared AWS profile files, ECS/container credentials, then EC2 instance
      profile credentials.
    @param clock Time source for signing timestamps (default: OS clock)
    @param retry_policy
      Retry behavior for retryable AWS errors and transient transport failures
      (default: [Awskit.Retry.default])
    @param random_float
      Random delay source for retry jitter. Defaults to a connection-local
      random state.
    @param timeout_policy
      Runtime timeout policy (default: [Awskit.Timeout.default]).
    @param max_response_drain_bytes
      Maximum response body bytes to drain after callbacks (default: 64 MiB). If
      a response consumer succeeds but the remaining body exceeds this drain
      limit, the operation fails with a body-limit error. If the consumer fails,
      the consumer error is returned.
    @param imdsv1_fallback
      Controls tokenless IMDSv1 fallback when the default credential chain uses
      EC2 instance metadata credentials. *)
