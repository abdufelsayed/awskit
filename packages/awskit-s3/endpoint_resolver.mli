(** S3 endpoint and addressing resolution.

    Most application code should pass endpoint options to an adapter [create]
    function or to {!Awskit_s3.Presigned}. This module is exposed so custom
    runtimes can use the same endpoint configuration type as {!Awskit_s3.Make}.
*)

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type resolved_style = [ `Path | `Virtual_hosted ]

module Request : sig
  type t = {
    endpoint : Awskit.Endpoint.t;
    path : string;
    signing_path : string;
    style : resolved_style;
  }
end

type t

val create :
  ?addressing_style:addressing_style ->
  ?endpoint_variant:endpoint_variant ->
  ?scheme:Awskit.Endpoint.Scheme.t ->
  ?endpoint:Awskit.Endpoint.t ->
  unit ->
  t

val default : t
val addressing_style : t -> addressing_style
val endpoint_variant : t -> endpoint_variant
val scheme : t -> Awskit.Endpoint.Scheme.t

val endpoint :
  t -> region:Awskit.Region.t -> (Awskit.Endpoint.t, Awskit.Error.t) result

val resolve_bucket_request :
  t ->
  region:Awskit.Region.t ->
  bucket:string ->
  suffix:string ->
  signing_suffix:string ->
  (Request.t, Awskit.Error.t) result

val resolve_object_request :
  t ->
  region:Awskit.Region.t ->
  bucket:string ->
  key:string ->
  (Request.t, Awskit.Error.t) result
