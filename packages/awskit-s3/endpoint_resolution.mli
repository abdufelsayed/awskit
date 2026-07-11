(** Internal request-specific S3 endpoint and addressing resolution. *)

type resolved_style = [ `Path | `Virtual_hosted ]

module Request : sig
  type t = {
    endpoint : Awskit.Endpoint.t;
    path : string;
    signing_path : string;
    signing_region : Awskit.Region.t;
    style : resolved_style;
  }
end

val resolve_bucket_request :
  Endpoint_config.t ->
  region:Awskit.Region.t ->
  bucket:Bucket_name.t ->
  suffix:string ->
  signing_suffix:string ->
  (Request.t, Awskit.Error.t) result

val resolve_object_request :
  Endpoint_config.t ->
  region:Awskit.Region.t ->
  bucket:Bucket_name.t ->
  key:Object_key.t ->
  (Request.t, Awskit.Error.t) result
