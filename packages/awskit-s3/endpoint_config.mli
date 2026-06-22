(** S3 endpoint policy.

    Endpoint configuration is an advanced application API shared by normal S3
    operations, presigned requests, and custom runtimes built with
    {!Awskit_s3.Make}. *)

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
(** Requested bucket addressing style. [`Auto] uses virtual-hosted addressing
    when the bucket and endpoint support it, otherwise path-style. *)

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]
(** AWS S3 endpoint variant. *)

type tls_policy = [ `Https_required | `Http_local_only | `Http_unsafe ]
(** TLS policy for explicit S3-compatible endpoints. [`Http_unsafe] is for
    controlled tests or non-production networks only. *)

type feature_policy = [ `Aws_strict | `S3_compatible ]
(** Feature policy for endpoint-specific S3 behavior. *)

type t
(** Opaque endpoint configuration. *)

val aws :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  unit ->
  t
(** Build the default AWS S3 endpoint policy. AWS endpoints are HTTPS. *)

val default : t
(** Default AWS S3 regional HTTPS endpoint policy. *)

val s3_compatible :
  endpoint:Awskit.Endpoint.t ->
  signing_region:Awskit.Region.t ->
  addressing_style:addressing_style ->
  tls_policy:tls_policy ->
  feature_policy:feature_policy ->
  unit ->
  (t, Awskit.Error.t) result
(** Build an explicit S3-compatible endpoint policy. *)

val local_plaintext :
  endpoint:Awskit.Endpoint.t ->
  signing_region:Awskit.Region.t ->
  addressing_style:[ `Path ] ->
  unit ->
  (t, Awskit.Error.t) result
(** Build a path-style plaintext loopback endpoint policy for local
    S3-compatible tests. *)

val unsafe_plaintext :
  endpoint:Awskit.Endpoint.t ->
  signing_region:Awskit.Region.t ->
  addressing_style:addressing_style ->
  unit ->
  t
(** Build a deliberately unsafe plaintext endpoint policy. *)

val addressing_style : t -> addressing_style
(** Return the configured addressing-style preference. *)

val endpoint_variant : t -> endpoint_variant option
(** Return the AWS endpoint variant, or [None] for explicit endpoints. *)

val endpoint :
  t -> region:Awskit.Region.t -> (Awskit.Endpoint.t, Awskit.Error.t) result
(** Resolve the base endpoint for the configured policy. *)

val signing_region : t -> client_region:Awskit.Region.t -> Awskit.Region.t
(** Return the region used in SigV4 credential scope. *)

val feature_policy : t -> feature_policy
(** Return the configured feature policy. *)
