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

let hmac_sha256 ~key data =
  Digestif.SHA256.(hmac_string ~key data |> to_raw_string)

let signing_key ~credentials ~datestamp ~region ~service =
  Credentials.signing_key credentials ~datestamp ~region ~service

let uri_encode ?(encode_slash = true) s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.' ->
          Buffer.add_char buf c
      | '/' when not encode_slash -> Buffer.add_char buf c
      | c -> Buffer.add_string buf (Fmt.str "%%%02X" (Char.to_int c)));
  Buffer.contents buf

let parse_raw_query query =
  if String.is_empty query then []
  else
    String.split query ~on:'&'
    |> List.filter ~f:(fun piece -> not (String.is_empty piece))
    |> List.map ~f:(fun pair ->
        match String.lsplit2 pair ~on:'=' with
        | Some (k, v) -> (k, [ v ])
        | None -> (pair, [ "" ]))

let canonical_query_params params =
  let pairs =
    List.concat_map params ~f:(fun (key, values) ->
        match values with
        | [] -> [ (key, "") ]
        | values -> List.map values ~f:(fun value -> (key, value)))
    |> List.map ~f:(fun (key, value) -> (uri_encode key, uri_encode value))
    |> List.sort ~compare:(fun (k1, v1) (k2, v2) ->
        let c = String.compare k1 k2 in
        if c <> 0 then c else String.compare v1 v2)
  in
  String.concat ~sep:"&" (List.map pairs ~f:(fun (k, v) -> k ^ "=" ^ v))

let canonical_query query = canonical_query_params (parse_raw_query query)

let normalize_header_value value =
  String.strip value
  |> String.split_on_chars ~on:[ ' '; '\t'; '\r'; '\n' ]
  |> List.filter ~f:(fun piece -> not (String.is_empty piece))
  |> String.concat ~sep:" "

let canonicalize_headers headers =
  let grouped =
    List.map headers ~f:(fun (key, value) ->
        (String.lowercase key, normalize_header_value value))
    |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
    |> List.group ~break:(fun (k1, _) (k2, _) -> not (String.equal k1 k2))
  in
  List.map grouped ~f:(function
    | [] ->
        invalid_arg "Awskit.Signing.canonicalize_headers: empty header group"
    | (key, value) :: rest ->
        (key, String.concat ~sep:"," (value :: List.map rest ~f:snd)))

let require_host_header headers =
  let hosts =
    List.filter_map headers ~f:(fun (key, value) ->
        if String.Caseless.equal key "host" then
          Some (normalize_header_value value)
        else None)
  in
  match hosts with
  | [] -> invalid_arg "Awskit.Signing.sign_request: missing host header"
  | [ host ] when String.is_empty host ->
      invalid_arg "Awskit.Signing.sign_request: host header is empty"
  | [ _host ] -> ()
  | _ -> invalid_arg "Awskit.Signing.sign_request: duplicate host header"

let sign_request_params ~credentials ~region ~service ~meth ~path ~query_params
    ~headers ~payload ~now =
  let datestamp, amz_date = ptime_to_date_time now in
  let payload_hash = sha256_hex payload in
  let base_headers =
    ("x-amz-date", amz_date)
    :: ("x-amz-content-sha256", payload_hash)
    :: headers
  in
  let base_headers =
    match Credentials.session_token credentials with
    | Some token -> ("x-amz-security-token", token) :: base_headers
    | None -> base_headers
  in
  require_host_header base_headers;
  let sorted_headers = canonicalize_headers base_headers in
  let signed_headers_str =
    String.concat ~sep:";" (List.map sorted_headers ~f:fst)
  in
  let canonical_headers =
    String.concat
      (List.map sorted_headers ~f:(fun (k, v) -> k ^ ":" ^ v ^ "\n"))
  in
  let canonical_request =
    String.concat ~sep:"\n"
      [
        meth;
        uri_encode ~encode_slash:false path;
        canonical_query_params query_params;
        canonical_headers;
        signed_headers_str;
        payload_hash;
      ]
  in
  let scope =
    String.concat ~sep:"/" [ datestamp; region; service; "aws4_request" ]
  in
  let string_to_sign =
    String.concat ~sep:"\n"
      [ "AWS4-HMAC-SHA256"; amz_date; scope; sha256_hex canonical_request ]
  in
  let signing_key = signing_key ~credentials ~datestamp ~region ~service in
  let signature =
    Digestif.SHA256.(hmac_string ~key:signing_key string_to_sign |> to_hex)
  in
  let authorization =
    Fmt.str "AWS4-HMAC-SHA256 Credential=%s/%s, SignedHeaders=%s, Signature=%s"
      (Credentials.access_key_id credentials)
      scope signed_headers_str signature
  in
  let final_headers = ("authorization", authorization) :: base_headers in
  { headers = final_headers; signed_headers_str }

let sign_request ~credentials ~region ~service ~meth ~path ~query ~headers
    ~payload ~now =
  sign_request_params ~credentials ~region ~service ~meth ~path
    ~query_params:(parse_raw_query query) ~headers ~payload ~now
