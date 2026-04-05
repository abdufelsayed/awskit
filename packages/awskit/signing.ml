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

let uri_encode ?(encode_slash = true) s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.' ->
          Buffer.add_char buf c
      | '/' when not encode_slash -> Buffer.add_char buf c
      | c -> Buffer.add_string buf (Fmt.str "%%%02X" (Char.to_int c)));
  Buffer.contents buf

let canonical_query query =
  if String.is_empty query then ""
  else
    let pairs =
      String.split query ~on:'&'
      |> List.map ~f:(fun pair ->
          match String.lsplit2 pair ~on:'=' with
          | Some (k, v) -> (uri_encode k, uri_encode v)
          | None -> (uri_encode pair, ""))
      |> List.sort ~compare:(fun (k1, v1) (k2, v2) ->
          let c = String.compare k1 k2 in
          if c <> 0 then c else String.compare v1 v2)
    in
    String.concat ~sep:"&" (List.map pairs ~f:(fun (k, v) -> k ^ "=" ^ v))

let sign_request ~credentials ~region ~service ~meth ~path ~query ~headers
    ~payload ~now =
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
  let sorted_headers =
    List.map base_headers ~f:(fun (k, v) ->
        (String.lowercase k, String.strip v))
    |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  in
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
        canonical_query query;
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
  let signing_key =
    hmac_sha256
      ~key:("AWS4" ^ Credentials.secret_access_key credentials)
      datestamp
    |> fun key ->
    hmac_sha256 ~key region |> fun key ->
    hmac_sha256 ~key service |> fun key -> hmac_sha256 ~key "aws4_request"
  in
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
