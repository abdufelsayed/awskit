open Common
module Multipart = Multipart
module Object = Object
module Endpoint_resolver = Endpoint_resolver

type addressing_style = Endpoint_config.addressing_style
type endpoint_variant = Endpoint_config.endpoint_variant
type endpoint_config = Endpoint_resolver.t
type method_ = [ `GET | `PUT | `HEAD | `DELETE ]

type result = {
  url : string;
  method_ : method_;
  safe_uri : Uri.t;
  signed_headers : (string * string) list;
  requested_expires_in : Ptime.Span.t;
  effective_expires_in : Ptime.Span.t;
  expires_at : Ptime.t option;
}

let method_ t = t.method_
let safe_uri t = t.safe_uri
let signed_headers t = t.signed_headers
let requested_expires_in t = t.requested_expires_in
let effective_expires_in t = t.effective_expires_in
let expires_at t = t.expires_at
let reveal_url t = t.url

module Put_object = struct
  type options = {
    expires_in : Ptime.Span.t option;
    content_type : Content_type.t option;
    checksum : Object.Checksum.value option;
    server_side_encryption : Object.Encryption.request option;
    expected_bucket_owner : Account_id.t option;
    extra_signed_headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      content_type = None;
      checksum = None;
      server_side_encryption = None;
      expected_bucket_owner = None;
      extra_signed_headers = [];
    }
end

module Get_object = struct
  type options = {
    expires_in : Ptime.Span.t option;
    response_content_type : Content_type.t option;
    response_content_disposition : Header_value.t option;
    version_id : Object.Version_id.t option;
    expected_bucket_owner : Account_id.t option;
    extra_signed_headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      response_content_type = None;
      response_content_disposition = None;
      version_id = None;
      expected_bucket_owner = None;
      extra_signed_headers = [];
    }
end

module Upload_part = struct
  type options = {
    expires_in : Ptime.Span.t option;
    checksum : Object.Checksum.value option;
    expected_bucket_owner : Account_id.t option;
    extra_signed_headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      checksum = None;
      expected_bucket_owner = None;
      extra_signed_headers = [];
    }
end

module Delete_object = struct
  type options = {
    expires_in : Ptime.Span.t option;
    expected_bucket_owner : Account_id.t option;
    extra_signed_headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      expected_bucket_owner = None;
      extra_signed_headers = [];
    }
end

let default_expires = Ptime.Span.of_int_s 3600
let max_expires = 604_800
let max_expires_span = Ptime.Span.of_int_s max_expires

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let request_method = function
  | `GET -> `GET
  | `PUT -> `PUT
  | `HEAD -> `HEAD
  | `DELETE -> `DELETE

let pp_span fmt span =
  match Ptime.Span.to_int_s span with
  | Some seconds when Ptime.Span.equal span (Ptime.Span.of_int_s seconds) ->
      Format.fprintf fmt "%ds" seconds
  | None -> Format.fprintf fmt "%a" Ptime.Span.pp span
  | Some _ -> Format.fprintf fmt "%a" Ptime.Span.pp span

let pp_header_names fmt headers =
  match List.map fst headers with
  | [] -> Format.pp_print_string fmt "[]"
  | names ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           Format.pp_print_string)
        names

let pp fmt t =
  Format.fprintf fmt
    "@[<2>{ method = %s;@ safe_uri = %a;@ signed_header_names = %a;@ \
     requested_expires_in = %a;@ effective_expires_in = %a;@ expires_at = %a \
     }@]"
    (method_to_string t.method_)
    Uri.pp t.safe_uri pp_header_names t.signed_headers pp_span
    t.requested_expires_in pp_span t.effective_expires_in
    (Format.pp_print_option Ptime.pp)
    t.expires_at

let canonical_headers headers =
  headers
  |> List.map (fun (key, value) ->
      (String.lowercase_ascii key, String.trim value))
  |> List.sort compare

let signed_headers_str headers = String.concat ";" (List.map fst headers)

let canonical_headers_str headers =
  headers
  |> List.map (fun (key, value) -> key ^ ":" ^ value ^ "\n")
  |> String.concat ""

let option_header key = function None -> [] | Some value -> [ (key, value) ]

let option_content_type_header key value =
  option_header key (Option.map Content_type.to_string value)

let expected_owner_header value =
  option_header "x-amz-expected-bucket-owner"
    (Option.map Account_id.to_string value)

let validate_opt f = function None -> Ok () | Some value -> f value

let expires_seconds span =
  if Ptime.Span.compare span Ptime.Span.zero <= 0 then
    invalid ~field:"expires_in" "expires_in must be positive"
  else if Ptime.Span.compare span max_expires_span > 0 then
    invalid ~field:"expires_in" "expires_in must be <= %d seconds" max_expires
  else
    match Ptime.Span.to_int_s span with
    | None ->
        invalid ~field:"expires_in" "expires_in is outside supported range"
    | Some seconds when seconds <= 0 ->
        invalid ~field:"expires_in" "expires_in must be at least 1 second"
    | Some seconds when seconds > max_expires ->
        invalid ~field:"expires_in" "expires_in must be <= %d seconds"
          max_expires
    | Some seconds -> Ok seconds

let credentials_expire_too_soon credentials =
  Error
    (Awskit.Error.Producer.credentials
       ?source:(Awskit.Credentials.source_label credentials)
       "credentials expire before presigned request can be used")

let effective_expiration ~credentials ~now ~requested_seconds =
  let requested_span = Ptime.Span.of_int_s requested_seconds in
  match Awskit.Credentials.expires_at credentials with
  | None -> Ok (requested_seconds, requested_span)
  | Some credentials_expires_at ->
      if Ptime.compare credentials_expires_at now <= 0 then
        credentials_expire_too_soon credentials
      else
        let credential_lifetime = Ptime.diff credentials_expires_at now in
        let credential_seconds_float =
          floor (Ptime.Span.to_float_s credential_lifetime)
        in
        let credential_seconds =
          if credential_seconds_float >= float_of_int requested_seconds then
            requested_seconds
          else int_of_float credential_seconds_float
        in
        let effective_seconds = min requested_seconds credential_seconds in
        if effective_seconds <= 0 then credentials_expire_too_soon credentials
        else Ok (effective_seconds, Ptime.Span.of_int_s effective_seconds)

let is_sigv4_query_param name =
  match String.lowercase_ascii name with
  | "x-amz-algorithm" | "x-amz-credential" | "x-amz-date" | "x-amz-expires"
  | "x-amz-signedheaders" | "x-amz-signature" | "x-amz-security-token" ->
      true
  | _ -> false

let safe_uri_of_url url =
  let uri = Uri.of_string url in
  let query =
    Uri.query uri
    |> List.filter (fun (name, _) -> not (is_sigv4_query_param name))
  in
  Uri.with_query uri query

let validate_part_number part_number =
  if part_number <= 0 then
    invalid ~field:"part_number" "part number must be positive"
  else if part_number > 10_000 then
    invalid ~field:"part_number" "part number must be <= 10000"
  else Ok ()

let parse_region region = Awskit.Region.of_string region

let endpoint_config ?addressing_style ?endpoint_variant () =
  Endpoint_config.aws ?addressing_style ?endpoint_variant ()

let generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_ ~signed_headers ~query ?expires_in () =
  let expires_span = Option.value ~default:default_expires expires_in in
  let* () = Awskit.Credentials.validate_fresh credentials ~now in
  let* expires = expires_seconds expires_span in
  let requested_expires_in = expires_span in
  let* expires, effective_expires_in =
    effective_expiration ~credentials ~now ~requested_seconds:expires
  in
  let* request =
    Endpoint_resolver.resolve_object_request endpoint_config ~region ~bucket
      ~key
  in
  let datestamp, amz_date = Awskit.Signing.ptime_to_date_time now in
  let scope =
    Fmt.str "%s/%s/s3/aws4_request" datestamp
      (Awskit.Region.to_string request.signing_region)
  in
  let credential =
    Fmt.str "%s/%s" (Awskit.Credentials.access_key_id credentials) scope
  in
  let signed_header_values =
    ("host", Awskit.Endpoint.authority request.endpoint) :: signed_headers
  in
  let* () = Awskit.Request.validate_headers signed_header_values in
  let signed_header_values = canonical_headers signed_header_values in
  let signed_headers_param = signed_headers_str signed_header_values in
  let query =
    [
      ("X-Amz-Algorithm", [ "AWS4-HMAC-SHA256" ]);
      ("X-Amz-Credential", [ credential ]);
      ("X-Amz-Date", [ amz_date ]);
      ("X-Amz-Expires", [ string_of_int expires ]);
      ("X-Amz-SignedHeaders", [ signed_headers_param ]);
    ]
    @ query
  in
  let query =
    match Awskit.Credentials.session_token credentials with
    | None -> query
    | Some token -> ("X-Amz-Security-Token", [ token ]) :: query
  in
  let query_string = Awskit.Signing.canonical_query_params query in
  let canonical_request =
    String.concat "\n"
      [
        method_to_string method_;
        Awskit.Signing.uri_encode ~encode_slash:false request.signing_path;
        query_string;
        canonical_headers_str signed_header_values;
        signed_headers_param;
        "UNSIGNED-PAYLOAD";
      ]
  in
  let string_to_sign =
    String.concat "\n"
      [
        "AWS4-HMAC-SHA256";
        amz_date;
        scope;
        Digestif.SHA256.(digest_string canonical_request |> to_hex);
      ]
  in
  let signing_key =
    Awskit.Credentials.signing_key credentials ~datestamp
      ~region:request.signing_region ~service:"s3"
  in
  let signature =
    Digestif.SHA256.(hmac_string ~key:signing_key string_to_sign |> to_hex)
  in
  let url =
    Fmt.str "%s%s?%s&X-Amz-Signature=%s"
      (Awskit.Endpoint.to_url_prefix request.endpoint)
      request.path query_string signature
  in
  let safe_uri = safe_uri_of_url url in
  Ok
    {
      url;
      method_;
      safe_uri;
      signed_headers;
      requested_expires_in;
      effective_expires_in;
      expires_at = Ptime.add_span now effective_expires_in;
    }

let generate ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~bucket ~key ~method_ ~signed_headers ~query ?expires_in () =
  let* region = parse_region region in
  let endpoint_config =
    endpoint_config ?addressing_style ?endpoint_variant ()
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_ ~signed_headers ~query ?expires_in ()

let get_query (options : Get_object.options) =
  let add_opt key value acc =
    match value with None -> acc | Some v -> (key, [ v ]) :: acc
  in
  []
  |> add_opt "response-content-type"
       (Option.map Content_type.to_string options.response_content_type)
  |> add_opt "response-content-disposition"
       (Option.map Header_value.to_string options.response_content_disposition)
  |> add_opt "versionId"
       (Option.map Object.Version_id.to_string options.version_id)

let get_object ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate ~region ~credentials ~now ?addressing_style ?endpoint_variant ~bucket
    ~key ~method_:`GET ~signed_headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let head_object ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate ~region ~credentials ~now ?addressing_style ?endpoint_variant ~bucket
    ~key ~method_:`HEAD ~signed_headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let put_object ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~bucket ~key ?options () =
  let options = Option.value ~default:Put_object.default_options options in
  let* () = validate_opt Headers.validate_checksum_value options.checksum in
  let headers =
    option_content_type_header "content-type" options.content_type
    @ Headers.checksum_value_headers options.checksum
    @ Headers.encryption_request_headers options.server_side_encryption
    @ expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate ~region ~credentials ~now ?addressing_style ?endpoint_variant ~bucket
    ~key ~method_:`PUT ~signed_headers:headers ~query:[]
    ?expires_in:options.expires_in ()

let delete_object ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~bucket ~key ?options () =
  let options = Option.value ~default:Delete_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate ~region ~credentials ~now ?addressing_style ?endpoint_variant ~bucket
    ~key ~method_:`DELETE ~signed_headers ~query:[]
    ?expires_in:options.expires_in ()

let upload_part_query ~upload_id ~part_number =
  [
    ("partNumber", [ Multipart.Part_number.to_int part_number |> string_of_int ]);
    ("uploadId", [ Multipart.Upload_id.to_string upload_id ]);
  ]

let upload_part_headers (options : Upload_part.options) =
  Headers.checksum_value_headers options.checksum
  @ expected_owner_header options.expected_bucket_owner
  @ options.extra_signed_headers

let upload_part ~region ~credentials ~now ?addressing_style ?endpoint_variant
    ~upload ~part_number ?options () =
  let options = Option.value ~default:Upload_part.default_options options in
  let* () = validate_opt Headers.validate_checksum_value options.checksum in
  let bucket = Multipart.Upload.bucket upload in
  let key = Multipart.Upload.key upload in
  let upload_id = Multipart.Upload.upload_id upload in
  generate ~region ~credentials ~now ?addressing_style ?endpoint_variant ~bucket
    ~key ~method_:`PUT
    ~signed_headers:(upload_part_headers options)
    ~query:(upload_part_query ~upload_id ~part_number)
    ?expires_in:options.expires_in ()

let get_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`GET ~signed_headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let head_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`HEAD ~signed_headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let put_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Put_object.default_options options in
  let* () = validate_opt Headers.validate_checksum_value options.checksum in
  let headers =
    option_content_type_header "content-type" options.content_type
    @ Headers.checksum_value_headers options.checksum
    @ Headers.encryption_request_headers options.server_side_encryption
    @ expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`PUT ~signed_headers:headers ~query:[]
    ?expires_in:options.expires_in ()

let delete_object_with_endpoint_config ~region ~credentials ~now
    ~endpoint_config ~bucket ~key ?options () =
  let options = Option.value ~default:Delete_object.default_options options in
  let signed_headers =
    expected_owner_header options.expected_bucket_owner
    @ options.extra_signed_headers
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`DELETE ~signed_headers ~query:[]
    ?expires_in:options.expires_in ()

let upload_part_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~upload ~part_number ?options () =
  let options = Option.value ~default:Upload_part.default_options options in
  let* () = validate_opt Headers.validate_checksum_value options.checksum in
  let bucket = Multipart.Upload.bucket upload in
  let key = Multipart.Upload.key upload in
  let upload_id = Multipart.Upload.upload_id upload in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`PUT
    ~signed_headers:(upload_part_headers options)
    ~query:(upload_part_query ~upload_id ~part_number)
    ?expires_in:options.expires_in ()
