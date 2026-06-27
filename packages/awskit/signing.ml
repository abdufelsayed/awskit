module Aws_error = Error
open Base

type signed_headers = {
  headers : (string * string) list;
  signed_headers_str : string;
}

let ptime_to_date_time (t : Ptime.t) =
  let (y, m, d), ((hh, mm, ss), _tz) = Ptime.to_date_time t in
  let datestamp = Fmt.str "%04d%02d%02d" y m d in
  let amz_date = Fmt.str "%04d%02d%02dT%02d%02d%02dZ" y m d hh mm ss in
  (datestamp, amz_date)

let sha256_hex s = Digestif.SHA256.(digest_string s |> to_hex)
let uri_encode ?(encode_slash = true) value = Aws_uri.encode ~encode_slash value

let canonical_query_params query_params =
  Aws_uri.canonical_query_params query_params

let normalize_header_value value =
  String.strip value
  |> String.split_on_chars ~on:[ ' '; '\t'; '\r'; '\n' ]
  |> List.filter ~f:(fun piece -> not (String.is_empty piece))
  |> String.concat ~sep:" "

let canonical_headers headers =
  let grouped =
    List.map headers ~f:(fun (key, value) ->
        (String.lowercase key, normalize_header_value value))
    |> List.sort ~compare:(fun (key_a, _) (key_b, _) ->
        String.compare key_a key_b)
    |> List.group ~break:(fun (key_a, _) (key_b, _) ->
        not (String.equal key_a key_b))
  in
  List.map grouped ~f:(function
    | [] -> assert false
    | (key, value) :: rest ->
        (key, String.concat ~sep:"," (value :: List.map rest ~f:snd)))

let signed_header_names headers =
  String.concat ~sep:";" (List.map headers ~f:fst)

let canonical_headers_block headers =
  String.concat
    (List.map headers ~f:(fun (key, value) -> key ^ ":" ^ value ^ "\n"))

let validate_host_header headers =
  let hosts =
    List.filter_map headers ~f:(fun (key, value) ->
        if String.Caseless.equal key "host" then
          Some (normalize_header_value value)
        else None)
  in
  match hosts with
  | [] ->
      Error
        (Aws_error.Producer.validation ~field:"host"
           "signing requires exactly one host header")
  | [ host ] when String.is_empty host ->
      Error (Aws_error.Producer.validation ~field:"host" "host header is empty")
  | [ _host ] -> Ok ()
  | _ ->
      Error
        (Aws_error.Producer.validation ~field:"host"
           "signing requires exactly one host header")

let sign_request_params ~credentials ~region ~service ~method_ ~path
    ~query_params ~headers ~payload_hash ~now =
  match Credentials.validate_fresh credentials ~now with
  | Error _ as error -> error
  | Ok () -> (
      let datestamp, amz_date = ptime_to_date_time now in
      let payload_hash_header =
        Body.Payload_hash.to_header_value payload_hash
      in
      let base_headers =
        ("x-amz-date", amz_date)
        :: ("x-amz-content-sha256", payload_hash_header)
        :: headers
      in
      let base_headers =
        match Credentials.session_token credentials with
        | Some token -> ("x-amz-security-token", token) :: base_headers
        | None -> base_headers
      in
      match Request.validate_headers base_headers with
      | Error _ as error -> error
      | Ok () -> (
          match validate_host_header base_headers with
          | Error _ as error -> error
          | Ok () ->
              let sorted_headers = canonical_headers base_headers in
              let signed_headers_str = signed_header_names sorted_headers in
              let canonical_headers = canonical_headers_block sorted_headers in
              let canonical_request =
                String.concat ~sep:"\n"
                  [
                    Request.Method.to_string method_;
                    uri_encode ~encode_slash:false path;
                    canonical_query_params query_params;
                    canonical_headers;
                    signed_headers_str;
                    payload_hash_header;
                  ]
              in
              let scope =
                String.concat ~sep:"/"
                  [
                    datestamp; Region.to_string region; service; "aws4_request";
                  ]
              in
              let string_to_sign =
                String.concat ~sep:"\n"
                  [
                    "AWS4-HMAC-SHA256";
                    amz_date;
                    scope;
                    sha256_hex canonical_request;
                  ]
              in
              let signing_key =
                Credentials.signing_key credentials ~datestamp ~region ~service
              in
              let signature =
                Digestif.SHA256.(
                  hmac_string ~key:signing_key string_to_sign |> to_hex)
              in
              let authorization =
                Fmt.str
                  "AWS4-HMAC-SHA256 Credential=%s/%s, SignedHeaders=%s, \
                   Signature=%s"
                  (Credentials.access_key_id credentials)
                  scope signed_headers_str signature
              in
              Ok
                {
                  headers = ("authorization", authorization) :: base_headers;
                  signed_headers_str;
                }))

let sign_request_params_exn ~credentials ~region ~service ~method_ ~path
    ~query_params ~headers ~payload_hash ~now =
  Aws_error.Producer.get_ok_exn
    (sign_request_params ~credentials ~region ~service ~method_ ~path
       ~query_params ~headers ~payload_hash ~now)
