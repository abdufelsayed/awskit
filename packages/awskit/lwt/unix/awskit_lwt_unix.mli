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

    For MirageOS or custom Lwt HTTP backends, use the generic {!Awskit_lwt.Make}
    functor instead. *)

type t
(** Connection handle. Create with {!val:create}, then pass to service
    operations. *)

(** Runtime module satisfying {!module-type:Awskit.Runtime.S}. The monad is
    [Lwt.t]. *)
module Runtime :
  Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?endpoint:Awskit.Endpoint.t ->
  ?region:string ->
  ?credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?max_response_body_bytes:int ->
  unit ->
  (t, Awskit.Error.base) result
(** Create a connection to AWS.

    @param ctx Optional Cohttp client context (e.g., for custom TLS config)
    @param endpoint Explicit endpoint override (for LocalStack, MinIO, etc.)
    @param region
      AWS region (e.g., ["us-east-1"]). If omitted, reads [AWS_REGION] and then
      [AWS_DEFAULT_REGION].
    @param credentials
      AWS credentials for request signing. If omitted, reads
      [AWS_ACCESS_KEY_ID], [AWS_SECRET_ACCESS_KEY], and optional
      [AWS_SESSION_TOKEN].
    @param clock Time source for signing timestamps (default: OS clock)
    @param max_response_body_bytes
      Maximum response size to buffer in memory (default: 64 MiB) *)
