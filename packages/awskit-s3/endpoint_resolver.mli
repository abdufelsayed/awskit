(** S3 endpoint and addressing resolution.

    Most application code should pass endpoint options to an adapter [create]
    function or to {!Awskit_s3.Presigned}. This module is exposed so custom
    runtimes can use the same endpoint configuration type as {!Awskit_s3.Make}.
*)

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
(** AWS S3 endpoint variant used when no explicit endpoint is supplied. *)

type resolved_style = [ `Path | `Virtual_hosted ]
(** Concrete addressing style selected for one request. *)

module Request : sig
  type t = {
    endpoint : Awskit.Endpoint.t;
    path : string;
    signing_path : string;
    style : resolved_style;
  }
  (** Resolved endpoint and paths for one S3 request. *)
end

type t

val create :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  ?endpoint:Awskit.Endpoint.t ->
  unit ->
  t
(** Create S3 endpoint configuration. [endpoint] overrides AWS regional endpoint
    construction; addressing still applies to bucket/object paths. *)

val default : t
val addressing_style : t -> addressing_style
val endpoint_variant : t -> endpoint_variant
val scheme : t -> Awskit.Endpoint.Scheme.t

val endpoint :
  t -> region:Awskit.Region.t -> (Awskit.Endpoint.t, Awskit.Error.t) result
(** Resolve the base S3 endpoint for a region. *)

val resolve_bucket_request :
  t ->
  region:Awskit.Region.t ->
  bucket:string ->
  suffix:string ->
  signing_suffix:string ->
  (Request.t, Awskit.Error.t) result
(** Resolve endpoint, transport path, and signing path for a bucket-level
    request. *)

val resolve_object_request :
  t ->
  region:Awskit.Region.t ->
  bucket:string ->
  key:string ->
  (Request.t, Awskit.Error.t) result
(** Resolve endpoint, transport path, and signing path for an object request. *)
