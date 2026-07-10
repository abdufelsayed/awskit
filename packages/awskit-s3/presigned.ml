module Multipart = Multipart
module Object = Object
module Resolver = Endpoint_config.Resolver

let ( let* ) = S3_result.( let* )

type method_ = [ `GET | `PUT | `HEAD | `DELETE ]

type result = {
  url : string;
  method_ : method_;
  safe_uri : Uri.t;
  signed_headers : (string * string) list;
  request_headers : (string * string) list;
  requested_expires_in : Ptime.Span.t;
  effective_expires_in : Ptime.Span.t;
  expires_at : Ptime.t;
}

let method_ t = t.method_
let safe_uri t = t.safe_uri
let signed_headers t = t.signed_headers
let request_headers t = t.request_headers
let requested_expires_in t = t.requested_expires_in
let effective_expires_in t = t.effective_expires_in
let expires_at t = t.expires_at
let reveal_url t = t.url

module Lifetime = struct
  type t = int

  let max_seconds = 604_800
  let default = 3600

  let of_span span =
    if Ptime.Span.compare span Ptime.Span.zero <= 0 then
      S3_error_context.invalid ~field:"expires_in" "expires_in must be positive"
    else
      match Ptime.Span.to_int_s span with
      | Some seconds
        when Ptime.Span.equal span (Ptime.Span.of_int_s seconds)
             && seconds <= max_seconds ->
          Ok seconds
      | Some seconds when seconds > max_seconds ->
          S3_error_context.invalid ~field:"expires_in"
            "expires_in must be <= %d seconds" max_seconds
      | Some _ | None ->
          S3_error_context.invalid ~field:"expires_in"
            "expires_in must be a whole number of seconds"

  let of_span_exn span = Awskit.Error.Producer.get_ok_exn (of_span span)
  let to_span seconds = Ptime.Span.of_int_s seconds
  let seconds t = t
end

module Additional_headers = struct
  type t = (string * string) list

  let empty = []

  let signer_owned name =
    match String.lowercase_ascii name with
    | "host" | "authorization" | "x-amz-algorithm" | "x-amz-credential"
    | "x-amz-date" | "x-amz-expires" | "x-amz-signedheaders" | "x-amz-signature"
    | "x-amz-security-token" ->
        true
    | _ -> false

  let validate_names headers =
    let rec loop seen = function
      | [] -> Ok ()
      | (name, _) :: rest ->
          let normalized = String.lowercase_ascii name in
          if signer_owned normalized then
            S3_error_context.invalid ~field:"header" "presigner owns header: %s"
              normalized
          else if List.exists (String.equal normalized) seen then
            S3_error_context.invalid ~field:"header"
              "duplicate additional header: %s" normalized
          else loop (normalized :: seen) rest
    in
    loop [] headers

  let of_list headers =
    let* () = Awskit.Request.validate_headers headers in
    let* () = validate_names headers in
    Ok headers

  let of_list_exn headers = Awskit.Error.Producer.get_ok_exn (of_list headers)
  let to_list t = t
end

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let pp_span fmt span =
  match Ptime.Span.to_int_s span with
  | Some seconds when Ptime.Span.equal span (Ptime.Span.of_int_s seconds) ->
      Format.fprintf fmt "%ds" seconds
  | None | Some _ -> Format.fprintf fmt "%a" Ptime.Span.pp span

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
    t.requested_expires_in pp_span t.effective_expires_in Ptime.pp t.expires_at

let validate_unique_header_names headers =
  let rec loop seen = function
    | [] -> Ok ()
    | (name, _) :: rest ->
        let name = String.lowercase_ascii name in
        if List.exists (String.equal name) seen then
          S3_error_context.invalid ~field:"header" "duplicate signed header: %s"
            name
        else loop (name :: seen) rest
  in
  loop [] headers

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

let generate ~region ~credentials ~now ~endpoint_config ~bucket ~key ~method_
    ~signed_headers ~query ?expires_in ?additional_headers () =
  let expires_in = Option.value ~default:Lifetime.default expires_in in
  let additional_headers =
    Option.value ~default:Additional_headers.empty additional_headers
    |> Additional_headers.to_list
  in
  let requested_seconds = Lifetime.seconds expires_in in
  let requested_expires_in = Lifetime.to_span expires_in in
  let* () = Awskit.Credentials.validate_fresh credentials ~now in
  let* expires, effective_expires_in =
    effective_expiration ~credentials ~now ~requested_seconds
  in
  let* expires_at =
    match Ptime.add_span now effective_expires_in with
    | Some expires_at -> Ok expires_at
    | None ->
        S3_error_context.invalid ~field:"now"
          "presigned expiration is outside the supported timestamp range"
  in
  let* request =
    Resolver.resolve_object_request endpoint_config ~region ~bucket ~key
  in
  let datestamp, amz_date = Awskit.Signing.ptime_to_date_time now in
  let scope =
    Fmt.str "%s/%s/s3/aws4_request" datestamp
      (Awskit.Region.to_string request.signing_region)
  in
  let credential =
    Fmt.str "%s/%s" (Awskit.Credentials.access_key_id credentials) scope
  in
  let signed_headers = signed_headers @ additional_headers in
  let raw_signed_header_values =
    ("host", Awskit.Endpoint.authority request.endpoint) :: signed_headers
  in
  let* () = Awskit.Request.validate_headers raw_signed_header_values in
  let* () = validate_unique_header_names raw_signed_header_values in
  let request_headers = Awskit.Signing.canonical_headers signed_headers in
  let signed_header_values =
    Awskit.Signing.canonical_headers raw_signed_header_values
  in
  let signed_headers_param =
    Awskit.Signing.signed_header_names signed_header_values
  in
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
        Awskit.Signing.canonical_headers_block signed_header_values;
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
  Ok
    {
      url;
      method_;
      safe_uri = safe_uri_of_url url;
      signed_headers = signed_header_values;
      request_headers;
      requested_expires_in;
      effective_expires_in;
      expires_at;
    }

let add_opt_header name value headers =
  match value with None -> headers | Some value -> (name, value) :: headers

let add_owner owner headers =
  add_opt_header "x-amz-expected-bucket-owner"
    (Option.map Account_id.to_string owner)
    headers

let response_override_query (overrides : Object.Response_overrides.t) =
  []
  |> add_opt_header "response-content-type"
       (Option.map Content_type.to_string overrides.content_type)
  |> add_opt_header "response-content-disposition"
       (Option.map Header_value.to_string overrides.content_disposition)
  |> List.map (fun (name, value) -> (name, [ value ]))

let version_query = function
  | None -> []
  | Some version_id ->
      [ ("versionId", [ Object.Version_id.to_string version_id ]) ]

let get_headers (options : Object.Get.options) =
  Headers.read_precondition_headers options.preconditions
  @ Headers.checksum_mode_header options.checksum_mode
  @ Headers.source_encryption_headers options.source_encryption
  |> add_opt_header "range" (Option.map Range.to_header options.range)
  |> add_owner options.expected_bucket_owner

let head_headers (options : Object.Head.options) =
  Headers.read_precondition_headers options.preconditions
  @ Headers.checksum_mode_header options.checksum_mode
  @ Headers.source_encryption_headers options.source_encryption
  |> add_owner options.expected_bucket_owner

let put_headers (options : Object.Put.options) =
  S3_metadata_headers.to_headers options.metadata
  @ Headers.write_precondition_headers options.preconditions
  @ Headers.checksum_value_headers options.checksum
  @ Headers.destination_encryption_headers options.encryption
  |> Headers.add_opt_content_type_header "content-type" options.content_type
  |> add_opt_header "cache-control"
       (Option.map Header_value.to_string options.cache_control)
  |> add_opt_header "content-encoding"
       (Option.map Header_value.to_string options.content_encoding)
  |> add_opt_header "content-disposition"
       (Option.map Header_value.to_string options.content_disposition)
  |> add_opt_header "x-amz-storage-class"
       (Option.map Storage_class.to_string options.storage_class)
  |> add_opt_header "x-amz-tagging" (Headers.tags_header options.tags)
  |> add_owner options.expected_bucket_owner

let delete_headers (options : Object.Delete.options) =
  Headers.delete_precondition_headers options.preconditions
  |> add_owner options.expected_bucket_owner

let upload_part_query upload part_number =
  [
    ("partNumber", [ Multipart.Part_number.to_int part_number |> string_of_int ]);
    ( "uploadId",
      [ Multipart.Upload.upload_id upload |> Multipart.Upload_id.to_string ] );
  ]

let upload_part_headers (options : Multipart.Upload_part.options) =
  Headers.checksum_value_headers options.checksum
  @ Headers.customer_key_headers options.customer_key
  |> add_owner options.expected_bucket_owner

module Signer = struct
  type t = {
    region : Awskit.Region.t;
    credentials : Awskit.Credentials.t;
    endpoint_config : Endpoint_config.t;
  }

  let create ~region ~credentials ?(endpoint_config = Endpoint_config.default)
      () =
    { region; credentials; endpoint_config }

  let get_object t ~now ~bucket ~key ?expires_in ?additional_headers
      ?(response_overrides = Object.Response_overrides.none) ?options () =
    let options = Option.value ~default:Object.Get.default_options options in
    generate ~region:t.region ~credentials:t.credentials ~now
      ~endpoint_config:t.endpoint_config ~bucket ~key ~method_:`GET
      ~signed_headers:(get_headers options)
      ~query:
        (version_query options.version_id
        @ response_override_query response_overrides)
      ?expires_in ?additional_headers ()

  let head_object t ~now ~bucket ~key ?expires_in ?additional_headers
      ?(response_overrides = Object.Response_overrides.none) ?options () =
    let options = Option.value ~default:Object.Head.default_options options in
    generate ~region:t.region ~credentials:t.credentials ~now
      ~endpoint_config:t.endpoint_config ~bucket ~key ~method_:`HEAD
      ~signed_headers:(head_headers options)
      ~query:
        (version_query options.version_id
        @ response_override_query response_overrides)
      ?expires_in ?additional_headers ()

  let put_object t ~now ~bucket ~key ?expires_in ?additional_headers ?options ()
      =
    let options = Option.value ~default:Object.Put.default_options options in
    generate ~region:t.region ~credentials:t.credentials ~now
      ~endpoint_config:t.endpoint_config ~bucket ~key ~method_:`PUT
      ~signed_headers:(put_headers options) ~query:[] ?expires_in
      ?additional_headers ()

  let delete_object t ~now ~bucket ~key ?expires_in ?additional_headers ?options
      () =
    let options = Option.value ~default:Object.Delete.default_options options in
    generate ~region:t.region ~credentials:t.credentials ~now
      ~endpoint_config:t.endpoint_config ~bucket ~key ~method_:`DELETE
      ~signed_headers:(delete_headers options)
      ~query:(version_query options.version_id)
      ?expires_in ?additional_headers ()

  let upload_part t ~now ~upload ~part_number ?expires_in ?additional_headers
      ?options () =
    let options =
      Option.value ~default:Multipart.Upload_part.default_options options
    in
    generate ~region:t.region ~credentials:t.credentials ~now
      ~endpoint_config:t.endpoint_config
      ~bucket:(Multipart.Upload.bucket upload)
      ~key:(Multipart.Upload.key upload)
      ~method_:`PUT
      ~signed_headers:(upload_part_headers options)
      ~query:(upload_part_query upload part_number)
      ?expires_in ?additional_headers ()
end
