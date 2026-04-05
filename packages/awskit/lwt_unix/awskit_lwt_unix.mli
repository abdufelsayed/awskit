(** Lwt + Unix runtime adapter for AWS.

    Ready-to-use adapter for Lwt applications on Unix. Uses
    [Cohttp_lwt_unix.Client] for HTTP — just create a connection and start
    making AWS calls:

    {[
      let conn = Awskit_lwt_unix.create
        ~region:"us-east-1"
        ~credentials
        ()
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
  ?scheme:[ `Http | `Https ] ->
  region:string ->
  credentials:Awskit.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?endpoint:string ->
  ?port:int ->
  unit ->
  t
(** Create a connection to AWS.

    @param ctx Optional Cohttp client context (e.g., for custom TLS config)
    @param scheme [\`Http] or [\`Https] (default: [\`Https])
    @param region AWS region (e.g., ["us-east-1"])
    @param credentials AWS credentials for request signing
    @param clock Time source for signing timestamps (default: OS clock)
    @param endpoint Override endpoint host (for LocalStack, MinIO, etc.)
    @param port Override port (implies HTTP) *)
