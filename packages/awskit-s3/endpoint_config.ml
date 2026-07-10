module Endpoint = Awskit.Endpoint
module Region = Awskit.Region

let ( let* ) = S3_result.( let* )

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type tls_policy = [ `Https_required | `Http_local_only | `Http_unsafe ]
type feature_policy = [ `Aws_strict | `S3_compatible ]

type aws = {
  addressing_style : addressing_style;
  endpoint_variant : endpoint_variant;
}

type compatible = {
  endpoint : Endpoint.t;
  signing_region : Region.t;
  addressing_style : addressing_style;
  tls_policy : tls_policy;
  feature_policy : feature_policy;
}

type t = Aws of aws | S3_compatible of compatible

let invalid ?field message =
  Error (Awskit.Error.Producer.validation ?field message)

let aws ?(addressing_style = `Auto) ?(endpoint_variant = `Regional) () =
  Aws { addressing_style; endpoint_variant }

let default = aws ()

let is_localhost = function
  | "localhost" | "::1" -> true
  | host -> (
      match String.split_on_char '.' host with
      | "127" :: rest ->
          rest <> []
          && List.for_all
               (fun part ->
                 match S3_parse.int_of_string_opt part with
                 | Some value -> value >= 0 && value <= 255
                 | None -> false)
               rest
      | _ -> false)

let validate_plaintext_policy ~endpoint ~tls_policy =
  match (Endpoint.scheme endpoint, tls_policy) with
  | `Https, `Https_required -> Ok ()
  | `Http, `Https_required ->
      invalid ~field:"tls_policy" "S3-compatible endpoint must use HTTPS"
  | `Http, `Http_local_only when is_localhost (Endpoint.host endpoint) -> Ok ()
  | `Http, `Http_local_only ->
      invalid ~field:"endpoint"
        "plaintext S3 endpoint must be localhost or loopback"
  | `Https, `Http_local_only ->
      invalid ~field:"tls_policy" "local_plaintext endpoint must use HTTP"
  | `Http, `Http_unsafe | `Https, `Http_unsafe -> Ok ()

let s3_compatible ~endpoint ~signing_region ~addressing_style ~tls_policy
    ~feature_policy () =
  let* () = validate_plaintext_policy ~endpoint ~tls_policy in
  let* () =
    match (tls_policy, addressing_style) with
    | `Http_local_only, `Path -> Ok ()
    | `Http_local_only, _ ->
        invalid ~field:"addressing_style"
          "local plaintext S3 endpoints require path-style addressing"
    | _ -> Ok ()
  in
  Ok
    (S3_compatible
       {
         endpoint;
         signing_region;
         addressing_style;
         tls_policy;
         feature_policy;
       })

let local_plaintext ~endpoint ~signing_region ~(addressing_style : [ `Path ]) ()
    =
  let addressing_style = (addressing_style :> addressing_style) in
  s3_compatible ~endpoint ~signing_region ~addressing_style
    ~tls_policy:`Http_local_only ~feature_policy:`S3_compatible ()

let unsafe_plaintext ~endpoint ~signing_region ~addressing_style () =
  S3_compatible
    {
      endpoint;
      signing_region;
      addressing_style;
      tls_policy = `Http_unsafe;
      feature_policy = `S3_compatible;
    }

let addressing_style = function
  | Aws config -> config.addressing_style
  | S3_compatible config -> config.addressing_style

let endpoint_variant = function
  | Aws config -> Some config.endpoint_variant
  | S3_compatible _ -> None

let aws_dns_suffix ~region =
  if String.length region >= 3 && String.sub region 0 3 = "cn-" then
    "amazonaws.com.cn"
  else "amazonaws.com"

let endpoint_host variant ~region =
  let region = Region.to_string region in
  let dns_suffix = aws_dns_suffix ~region in
  match variant with
  | `Regional -> Fmt.str "s3.%s.%s" region dns_suffix
  | `Dualstack -> Fmt.str "s3.dualstack.%s.%s" region dns_suffix
  | `Fips -> Fmt.str "s3-fips.%s.%s" region dns_suffix
  | `Fips_dualstack -> Fmt.str "s3-fips.dualstack.%s.%s" region dns_suffix
  | `Accelerate -> Fmt.str "s3-accelerate.%s" dns_suffix
  | `Accelerate_dualstack -> Fmt.str "s3-accelerate.dualstack.%s" dns_suffix

let endpoint t ~region =
  match t with
  | Aws config ->
      Endpoint.create ~scheme:`Https
        ~host:(endpoint_host config.endpoint_variant ~region)
        ()
  | S3_compatible config -> Ok config.endpoint

let signing_region t ~client_region =
  match t with
  | Aws _ -> client_region
  | S3_compatible config -> config.signing_region

let feature_policy = function
  | Aws _ -> `Aws_strict
  | S3_compatible config -> config.feature_policy

module Resolver = struct
  type resolved_style = [ `Path | `Virtual_hosted ]

  module Request = struct
    type t = {
      endpoint : Endpoint.t;
      path : string;
      signing_path : string;
      signing_region : Region.t;
      style : resolved_style;
    }
  end

  let bucket_has_dot bucket = String.contains bucket '.'
  let key_requires_path_style = String.equal "soap"

  let is_accelerate_variant = function
    | Some (`Accelerate | `Accelerate_dualstack) -> true
    | Some (`Regional | `Dualstack | `Fips | `Fips_dualstack) | None -> false

  let resolved_style ?key t endpoint bucket =
    let key_requires_path_style =
      Option.fold ~none:false ~some:key_requires_path_style key
    in
    match
      ( is_accelerate_variant (endpoint_variant t),
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
        match addressing_style t with
        | `Path ->
            S3_error_context.invalid ~field:"addressing_style"
              "S3 Transfer Acceleration requires virtual-hosted addressing"
        | `Auto | `Virtual_hosted -> Ok `Virtual_hosted)
    | false, _, true -> (
        match addressing_style t with
        | `Path | `Auto -> Ok `Path
        | `Virtual_hosted ->
            S3_error_context.invalid ~field:"key"
              {|object key "soap" requires path-style addressing|})
    | false, _, false -> (
        match addressing_style t with
        | `Path -> Ok `Path
        | `Virtual_hosted
          when Endpoint.scheme endpoint = `Https && bucket_has_dot bucket ->
            S3_error_context.invalid ~field:"addressing_style"
              "virtual-hosted HTTPS endpoints cannot be used with dotted \
               bucket names"
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

  let resolve_request ?key t ~region ~bucket ~suffix ~signing_suffix =
    let bucket = Bucket_name.to_string bucket in
    let* endpoint = endpoint t ~region in
    let* style = resolved_style ?key t endpoint bucket in
    let signing_region = signing_region t ~client_region:region in
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

  let resolve_bucket_request t ~region ~bucket ~suffix ~signing_suffix =
    resolve_request t ~region ~bucket ~suffix ~signing_suffix

  let resolve_object_request t ~region ~bucket ~key =
    let key = Object_key.to_string key in
    let suffix = "/" ^ Awskit.Signing.uri_encode ~encode_slash:false key in
    let signing_suffix = "/" ^ key in
    resolve_request ~key t ~region ~bucket ~suffix ~signing_suffix
end
