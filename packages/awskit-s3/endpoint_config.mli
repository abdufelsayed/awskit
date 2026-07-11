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
(** TLS policy for explicit endpoints.

    [`Http_local_only] permits plaintext only for loopback/local test endpoints.
    [`Http_unsafe] permits plaintext for any endpoint and should be limited to
    controlled tests or non-production networks. *)

type feature_policy = [ `Aws_strict | `S3_compatible ]
(** Feature policy for endpoint-specific behavior.

    [`Aws_strict] keeps AWS S3 validation and feature assumptions.
    [`S3_compatible] relaxes only the compatibility checks modeled by awskit; it
    is not a guarantee that every third-party S3-compatible service supports
    every S3 operation. *)

type t
(** Opaque endpoint configuration. *)

val aws :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  unit ->
  t
(** Build the default AWS S3 endpoint policy. AWS endpoints are HTTPS. Regions
    in the AWS China partition ([cn-*]) resolve under [amazonaws.com.cn]. *)

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
(** Build an explicit endpoint policy for S3-compatible services.

    [endpoint] is the base service endpoint, [signing_region] is the SigV4
    credential-scope region, and [addressing_style] controls path-style versus
    virtual-hosted bucket addressing. *)

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
(** Build a deliberately unsafe plaintext endpoint policy.

    Prefer {!val:local_plaintext} for loopback/local tests. *)

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
