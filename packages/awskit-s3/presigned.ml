open Base
module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint

module Expires_in = struct
  let default = 3600
  let max = 604_800

  let validate expires_in =
    if expires_in <= 0 then
      Error (`Invalid_request "expires_in must be positive")
    else if expires_in > max then
      Error (`Invalid_request (Fmt.str "expires_in must be <= %d seconds" max))
    else Ok expires_in
end

let canonical_headers headers =
  List.map headers ~f:(fun (key, value) ->
      (String.lowercase key, String.strip value))
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)

let signed_headers_str headers =
  String.concat ~sep:";" (List.map headers ~f:fst)

let canonical_headers_str headers =
  String.concat ~sep:"" (List.map headers ~f:(fun (k, v) -> k ^ ":" ^ v ^ "\n"))

let generate ~region ~credentials ~now ~endpoint ~bucket ~key ~meth ?expires_in
    ?content_type () =
  let expires = Option.value expires_in ~default:Expires_in.default in
  let open Internal.Validation in
  match validate_bucket bucket with
  | Error _ as error -> error
  | Ok () -> (
      match validate_key key with
      | Error _ as error -> error
      | Ok () -> (
          match Expires_in.validate expires with
          | Error _ as error -> error
          | Ok expires -> (
              match
                Internal.Validation.validate_range_header_value "content_type"
                  content_type
              with
              | Error _ as error -> error
              | Ok () ->
                  let datestamp, amz_date =
                    Awskit.Signing.ptime_to_date_time now
                  in
                  let scope =
                    Fmt.str "%s/%s/s3/aws4_request" datestamp region
                  in
                  let credential =
                    Fmt.str "%s/%s"
                      (Awskit.Credentials.access_key_id credentials)
                      scope
                  in
                  let raw_path =
                    Internal.raw_object_key_path key |> fun object_path ->
                    Fmt.str "/%s%s" bucket object_path
                  in
                  let path =
                    Internal.encode_object_key_path key |> fun object_path ->
                    Fmt.str "/%s%s" bucket object_path
                  in
                  let headers =
                    ("host", Awskit.Endpoint.authority endpoint)
                    ::
                    (match content_type with
                    | Some value -> [ ("content-type", value) ]
                    | None -> [])
                  in
                  let canonical_headers = canonical_headers headers in
                  let signed_headers = signed_headers_str canonical_headers in
                  let base_query =
                    [
                      ("X-Amz-Algorithm", [ "AWS4-HMAC-SHA256" ]);
                      ("X-Amz-Credential", [ credential ]);
                      ("X-Amz-Date", [ amz_date ]);
                      ("X-Amz-Expires", [ Int.to_string expires ]);
                      ("X-Amz-SignedHeaders", [ signed_headers ]);
                    ]
                  in
                  let base_query =
                    match Awskit.Credentials.session_token credentials with
                    | Some token ->
                        ("X-Amz-Security-Token", [ token ]) :: base_query
                    | None -> base_query
                  in
                  let query_str =
                    Awskit.Signing.canonical_query_params base_query
                  in
                  let canonical_request =
                    String.concat ~sep:"\n"
                      [
                        meth;
                        Awskit.Signing.uri_encode ~encode_slash:false raw_path;
                        query_str;
                        canonical_headers_str canonical_headers;
                        signed_headers;
                        "UNSIGNED-PAYLOAD";
                      ]
                  in
                  let string_to_sign =
                    String.concat ~sep:"\n"
                      [
                        "AWS4-HMAC-SHA256";
                        amz_date;
                        scope;
                        Digestif.SHA256.(
                          digest_string canonical_request |> to_hex);
                      ]
                  in
                  let hmac ~key data =
                    Digestif.SHA256.(hmac_string ~key data |> to_raw_string)
                  in
                  let signing_key =
                    hmac
                      ~key:
                        ("AWS4"
                        ^ Awskit.Credentials.secret_access_key credentials)
                      datestamp
                    |> fun key ->
                    hmac ~key region |> fun key ->
                    hmac ~key "s3" |> fun key -> hmac ~key "aws4_request"
                  in
                  let signature =
                    Digestif.SHA256.(
                      hmac_string ~key:signing_key string_to_sign |> to_hex)
                  in
                  Ok
                    (Fmt.str "%s%s?%s&X-Amz-Signature=%s"
                       (Awskit.Endpoint.to_url_prefix endpoint)
                       path query_str signature))))

let get_object ~region ~credentials ~now ~endpoint ~bucket ~key ?expires_in () =
  generate ~region ~credentials ~now ~endpoint ~bucket ~key ~meth:"GET"
    ?expires_in ()

let put_object ~region ~credentials ~now ~endpoint ~bucket ~key ?expires_in
    ?content_type () =
  generate ~region ~credentials ~now ~endpoint ~bucket ~key ~meth:"PUT"
    ?expires_in ?content_type ()
