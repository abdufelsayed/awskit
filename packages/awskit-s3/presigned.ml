open Common
module Multipart = Multipart
module Object = Object
module Endpoint_resolver = Endpoint_resolver

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type method_ = [ `GET | `PUT | `HEAD | `DELETE ]

type result = {
  url : string;
  method_ : method_;
  headers : (string * string) list;
  expires_at : Ptime.t option;
}

module Put_object = struct
  type options = {
    expires_in : Ptime.Span.t option;
    content_type : string option;
    checksum : Object.Checksum.request option;
    server_side_encryption : Object.Encryption.request option;
    headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      content_type = None;
      checksum = None;
      server_side_encryption = None;
      headers = [];
    }
end

module Get_object = struct
  type options = {
    expires_in : Ptime.Span.t option;
    response_content_type : string option;
    response_content_disposition : string option;
    version_id : Object.Version_id.t option;
    headers : (string * string) list;
  }

  let default_options =
    {
      expires_in = None;
      response_content_type = None;
      response_content_disposition = None;
      version_id = None;
      headers = [];
    }
end

module Upload_part = struct
  type options = {
    expires_in : Ptime.Span.t option;
    checksum : Object.Checksum.request option;
    headers : (string * string) list;
  }

  let default_options = { expires_in = None; checksum = None; headers = [] }
end

let default_expires = Ptime.Span.of_int_s 3600
let max_expires = 604_800

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

let expires_seconds span =
  match Ptime.Span.to_int_s span with
  | None -> invalid ~field:"expires_in" "expires_in is outside supported range"
  | Some seconds when seconds <= 0 ->
      invalid ~field:"expires_in" "expires_in must be positive"
  | Some seconds when seconds > max_expires ->
      invalid ~field:"expires_in" "expires_in must be <= %d seconds" max_expires
  | Some seconds -> Ok seconds

let validate_part_number part_number =
  if part_number <= 0 then
    invalid ~field:"part_number" "part number must be positive"
  else if part_number > 10_000 then
    invalid ~field:"part_number" "part number must be <= 10000"
  else Ok ()

let endpoint_config ?addressing_style ?endpoint_variant ?scheme ?endpoint () =
  Endpoint_resolver.create ?addressing_style ?endpoint_variant ?scheme ?endpoint
    ()

let generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_ ~headers ~query ?expires_in () =
  let expires_span = Option.value ~default:default_expires expires_in in
  let* expires = expires_seconds expires_span in
  let* request =
    Endpoint_resolver.resolve_object_request endpoint_config ~region ~bucket
      ~key
  in
  let datestamp, amz_date = Awskit.Signing.ptime_to_date_time now in
  let scope =
    Fmt.str "%s/%s/s3/aws4_request" datestamp (Awskit.Region.to_string region)
  in
  let credential =
    Fmt.str "%s/%s" (Awskit.Credentials.access_key_id credentials) scope
  in
  let signed_header_values =
    ("host", Awskit.Endpoint.authority request.endpoint) :: headers
    |> canonical_headers
  in
  let signed_headers = signed_headers_str signed_header_values in
  let query =
    [
      ("X-Amz-Algorithm", [ "AWS4-HMAC-SHA256" ]);
      ("X-Amz-Credential", [ credential ]);
      ("X-Amz-Date", [ amz_date ]);
      ("X-Amz-Expires", [ string_of_int expires ]);
      ("X-Amz-SignedHeaders", [ signed_headers ]);
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
        signed_headers;
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
    Awskit.Credentials.signing_key credentials ~datestamp ~region ~service:"s3"
  in
  let signature =
    Digestif.SHA256.(hmac_string ~key:signing_key string_to_sign |> to_hex)
  in
  let url =
    Fmt.str "%s%s?%s&X-Amz-Signature=%s"
      (Awskit.Endpoint.to_url_prefix request.endpoint)
      request.path query_string signature
  in
  Ok { url; method_; headers; expires_at = Ptime.add_span now expires_span }

let generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_ ~headers ~query ?expires_in
    () =
  let endpoint_config =
    endpoint_config ?addressing_style ?endpoint_variant ?scheme ?endpoint ()
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_ ~headers ~query ?expires_in ()

let get_query (options : Get_object.options) =
  let add_opt key value acc =
    match value with None -> acc | Some v -> (key, [ v ]) :: acc
  in
  []
  |> add_opt "response-content-type" options.response_content_type
  |> add_opt "response-content-disposition" options.response_content_disposition
  |> add_opt "versionId"
       (Option.map Object.Version_id.to_string options.version_id)

let get_object ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_:`GET
    ~headers:options.headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let head_object ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_:`HEAD
    ~headers:options.headers ~query:(get_query options)
    ?expires_in:options.expires_in ()

let put_object ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ?options () =
  let options = Option.value ~default:Put_object.default_options options in
  let headers =
    option_header "content-type" options.content_type
    @ Headers.checksum_request_headers options.checksum
    @ Headers.encryption_request_headers options.server_side_encryption
    @ options.headers
  in
  generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_:`PUT ~headers ~query:[]
    ?expires_in:options.expires_in ()

let delete_object ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ?expires_in () =
  generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_:`DELETE ~headers:[]
    ~query:[] ?expires_in ()

let upload_part_query ~upload_id ~part_number =
  [
    ("partNumber", [ string_of_int part_number ]);
    ("uploadId", [ Multipart.Upload_id.to_string upload_id ]);
  ]

let upload_part_headers (options : Upload_part.options) =
  Headers.checksum_request_headers options.checksum @ options.headers

let upload_part ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~upload_id ~part_number ?options () =
  let* () = validate_part_number part_number in
  let options = Option.value ~default:Upload_part.default_options options in
  generate ~region ~credentials ~now ?endpoint ?addressing_style
    ?endpoint_variant ?scheme ~bucket ~key ~method_:`PUT
    ~headers:(upload_part_headers options)
    ~query:(upload_part_query ~upload_id ~part_number)
    ?expires_in:options.expires_in ()

let get_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`GET ~headers:options.headers
    ~query:(get_query options) ?expires_in:options.expires_in ()

let head_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Get_object.default_options options in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`HEAD ~headers:options.headers
    ~query:(get_query options) ?expires_in:options.expires_in ()

let put_object_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ?options () =
  let options = Option.value ~default:Put_object.default_options options in
  let headers =
    option_header "content-type" options.content_type
    @ Headers.checksum_request_headers options.checksum
    @ Headers.encryption_request_headers options.server_side_encryption
    @ options.headers
  in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`PUT ~headers ~query:[] ?expires_in:options.expires_in
    ()

let delete_object_with_endpoint_config ~region ~credentials ~now
    ~endpoint_config ~bucket ~key ?expires_in () =
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`DELETE ~headers:[] ~query:[] ?expires_in ()

let upload_part_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~upload_id ~part_number ?options () =
  let* () = validate_part_number part_number in
  let options = Option.value ~default:Upload_part.default_options options in
  generate_with_endpoint_config ~region ~credentials ~now ~endpoint_config
    ~bucket ~key ~method_:`PUT
    ~headers:(upload_part_headers options)
    ~query:(upload_part_query ~upload_id ~part_number)
    ?expires_in:options.expires_in ()
