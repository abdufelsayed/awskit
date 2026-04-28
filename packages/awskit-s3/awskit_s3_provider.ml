open Awskit_s3_common

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type resolved_style = [ `Path | `Virtual_hosted ]

module Request = struct
  type t = {
    endpoint : Endpoint.t;
    path : string;
    signing_path : string;
    style : resolved_style;
  }
end

type t = {
  addressing_style : addressing_style;
  endpoint_variant : endpoint_variant;
  scheme : Endpoint.Scheme.t;
}

let aws ?(addressing_style = `Auto) ?(endpoint_variant = `Regional)
    ?(scheme = `Https) () =
  { addressing_style; endpoint_variant; scheme }

let default = aws ()
let addressing_style t = t.addressing_style
let endpoint_variant t = t.endpoint_variant
let scheme t = t.scheme

let endpoint_host t ~region =
  let region = Region.to_string region in
  match t.endpoint_variant with
  | `Regional -> Fmt.str "s3.%s.amazonaws.com" region
  | `Dualstack -> Fmt.str "s3.dualstack.%s.amazonaws.com" region
  | `Fips -> Fmt.str "s3-fips.%s.amazonaws.com" region
  | `Fips_dualstack -> Fmt.str "s3-fips.dualstack.%s.amazonaws.com" region
  | `Accelerate -> "s3-accelerate.amazonaws.com"
  | `Accelerate_dualstack -> "s3-accelerate.dualstack.amazonaws.com"

let endpoint t ~region =
  Endpoint.create ~scheme:t.scheme ~host:(endpoint_host t ~region) ()

let bucket_has_dot bucket = String.contains bucket '.'

let resolved_style t endpoint bucket =
  match t.addressing_style with
  | `Path -> Ok `Path
  | `Virtual_hosted -> Ok `Virtual_hosted
  | `Auto ->
      if Endpoint.scheme endpoint = `Https && bucket_has_dot bucket then
        Ok `Path
      else Ok `Virtual_hosted

let bucket_endpoint endpoint bucket =
  Endpoint.create ~scheme:(Endpoint.scheme endpoint)
    ~host:(bucket ^ "." ^ Endpoint.host endpoint)
    ?port:(Endpoint.port endpoint) ()

let path_style_path bucket suffix = "/" ^ bucket ^ suffix

let resolve_bucket_request t ~region ~bucket ~suffix ~signing_suffix =
  let* () = validate_bucket bucket in
  let* endpoint = endpoint t ~region in
  let* style = resolved_style t endpoint bucket in
  match style with
  | `Path ->
      Ok
        {
          Request.endpoint;
          path = path_style_path bucket suffix;
          signing_path = path_style_path bucket signing_suffix;
          style;
        }
  | `Virtual_hosted ->
      let* endpoint = bucket_endpoint endpoint bucket in
      Ok
        {
          Request.endpoint;
          path = suffix;
          signing_path = signing_suffix;
          style;
        }

let resolve_object_request t ~region ~bucket ~key =
  let* () = validate_bucket_key bucket key in
  let suffix = "/" ^ Awskit.Signing.uri_encode ~encode_slash:false key in
  let signing_suffix = "/" ^ key in
  resolve_bucket_request t ~region ~bucket ~suffix ~signing_suffix
