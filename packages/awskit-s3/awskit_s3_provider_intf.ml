open Awskit_s3_common

module type PROVIDER = sig
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
      endpoint : Endpoint.t;
      path : string;
      signing_path : string;
      style : resolved_style;
    }
  end

  type t

  val aws :
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Endpoint.Scheme.t ->
    unit ->
    t

  val default : t
  val addressing_style : t -> addressing_style
  val endpoint_variant : t -> endpoint_variant
  val scheme : t -> Endpoint.Scheme.t
  val endpoint : t -> region:Region.t -> (Endpoint.t, Error.t) result

  val resolve_bucket_request :
    t ->
    region:Region.t ->
    bucket:string ->
    suffix:string ->
    signing_suffix:string ->
    (Request.t, Error.t) result

  val resolve_object_request :
    t ->
    region:Region.t ->
    bucket:string ->
    key:string ->
    (Request.t, Error.t) result
end
