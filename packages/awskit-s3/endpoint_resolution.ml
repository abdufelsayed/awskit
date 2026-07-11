module Endpoint = Awskit.Endpoint

let ( let* ) = S3_result.( let* )

type resolved_style = [ `Path | `Virtual_hosted ]

module Request = struct
  type t = {
    endpoint : Endpoint.t;
    path : string;
    signing_path : string;
    signing_region : Awskit.Region.t;
    style : resolved_style;
  }
end

let bucket_has_dot bucket = String.contains bucket '.'
let key_requires_path_style = String.equal "soap"

let is_accelerate_variant = function
  | Some (`Accelerate | `Accelerate_dualstack) -> true
  | Some (`Regional | `Dualstack | `Fips | `Fips_dualstack) | None -> false

let resolved_style ?key config endpoint bucket =
  let key_requires_path_style =
    Option.fold ~none:false ~some:key_requires_path_style key
  in
  match
    ( is_accelerate_variant (Endpoint_config.endpoint_variant config),
      bucket_has_dot bucket,
      key_requires_path_style )
  with
  | true, true, _ ->
      S3_error_context.invalid ~field:"bucket"
        "S3 Transfer Acceleration cannot be used with dotted bucket names"
  | true, false, true ->
      S3_error_context.invalid ~field:"key"
        {|object key "soap" requires path-style addressing and cannot be used with S3 Transfer Acceleration|}
  | true, false, false -> (
      match Endpoint_config.addressing_style config with
      | `Path ->
          S3_error_context.invalid ~field:"addressing_style"
            "S3 Transfer Acceleration requires virtual-hosted addressing"
      | `Auto | `Virtual_hosted -> Ok `Virtual_hosted)
  | false, _, true -> (
      match Endpoint_config.addressing_style config with
      | `Path | `Auto -> Ok `Path
      | `Virtual_hosted ->
          S3_error_context.invalid ~field:"key"
            {|object key "soap" requires path-style addressing|})
  | false, _, false -> (
      match Endpoint_config.addressing_style config with
      | `Path -> Ok `Path
      | `Virtual_hosted
        when Endpoint.scheme endpoint = `Https && bucket_has_dot bucket ->
          S3_error_context.invalid ~field:"addressing_style"
            "virtual-hosted HTTPS endpoints cannot be used with dotted bucket \
             names"
      | `Virtual_hosted -> Ok `Virtual_hosted
      | `Auto ->
          if Endpoint.scheme endpoint = `Https && bucket_has_dot bucket then
            Ok `Path
          else Ok `Virtual_hosted)

let bucket_endpoint endpoint bucket =
  Endpoint.create ~scheme:(Endpoint.scheme endpoint)
    ~host:(bucket ^ "." ^ Endpoint.host endpoint)
    ?port:(Endpoint.port endpoint) ()

let path_style_path bucket suffix = "/" ^ bucket ^ suffix

let resolve_request ?key config ~region ~bucket ~suffix ~signing_suffix =
  let bucket = Bucket_name.to_string bucket in
  let* endpoint = Endpoint_config.endpoint config ~region in
  let* style = resolved_style ?key config endpoint bucket in
  let signing_region =
    Endpoint_config.signing_region config ~client_region:region
  in
  match style with
  | `Path ->
      Ok
        {
          Request.endpoint;
          path = path_style_path bucket suffix;
          signing_path = path_style_path bucket signing_suffix;
          signing_region;
          style;
        }
  | `Virtual_hosted ->
      let* endpoint = bucket_endpoint endpoint bucket in
      Ok
        {
          Request.endpoint;
          path = suffix;
          signing_path = signing_suffix;
          signing_region;
          style;
        }

let resolve_bucket_request config ~region ~bucket ~suffix ~signing_suffix =
  resolve_request config ~region ~bucket ~suffix ~signing_suffix

let resolve_object_request config ~region ~bucket ~key =
  let key = Object_key.to_string key in
  let suffix = "/" ^ Awskit.Signing.uri_encode ~encode_slash:false key in
  let signing_suffix = "/" ^ key in
  resolve_request ~key config ~region ~bucket ~suffix ~signing_suffix
