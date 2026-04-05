open Base

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

let generate ~region ~credentials ~now ~host ?port ~bucket ~key ~meth
    ?expires_in ?(extra_query = []) () =
  let expires = Option.value expires_in ~default:Expires_in.default in
  match Expires_in.validate expires with
  | Error _ as error -> error
  | Ok expires ->
      let datestamp, amz_date = Awskit.Signing.ptime_to_date_time now in
      let scope = Fmt.str "%s/%s/s3/aws4_request" datestamp region in
      let credential =
        Fmt.str "%s/%s" (Awskit.Credentials.access_key_id credentials) scope
      in
      let path = "/" ^ bucket ^ "/" ^ key in
      let base_query =
        [
          ("X-Amz-Algorithm", "AWS4-HMAC-SHA256");
          ("X-Amz-Credential", credential);
          ("X-Amz-Date", amz_date);
          ("X-Amz-Expires", Int.to_string expires);
          ("X-Amz-SignedHeaders", "host");
        ]
        @ extra_query
      in
      let query_str =
        base_query
        |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
        |> List.map ~f:(fun (key, value) ->
            Awskit.Signing.uri_encode key
            ^ "="
            ^ Awskit.Signing.uri_encode value)
        |> String.concat ~sep:"&"
      in
      let canonical_request =
        String.concat ~sep:"\n"
          [
            meth;
            Awskit.Signing.uri_encode ~encode_slash:false path;
            query_str;
            "host:" ^ host ^ "\n";
            "host";
            "UNSIGNED-PAYLOAD";
          ]
      in
      let string_to_sign =
        String.concat ~sep:"\n"
          [
            "AWS4-HMAC-SHA256";
            amz_date;
            scope;
            Digestif.SHA256.(digest_string canonical_request |> to_hex);
          ]
      in
      let hmac ~key data =
        Digestif.SHA256.(hmac_string ~key data |> to_raw_string)
      in
      let signing_key =
        hmac
          ~key:("AWS4" ^ Awskit.Credentials.secret_access_key credentials)
          datestamp
        |> fun key ->
        hmac ~key region |> fun key ->
        hmac ~key "s3" |> fun key -> hmac ~key "aws4_request"
      in
      let signature =
        Digestif.SHA256.(hmac_string ~key:signing_key string_to_sign |> to_hex)
      in
      let port_suffix =
        match port with Some port -> ":" ^ Int.to_string port | None -> ""
      in
      let scheme = if Option.is_some port then "http" else "https" in
      Ok
        (Fmt.str "%s://%s%s%s?%s&X-Amz-Signature=%s" scheme host port_suffix
           path query_str signature)

let get_object ~region ~credentials ~now ~host ?port ~bucket ~key ?expires_in ()
    =
  generate ~region ~credentials ~now ~host ?port ~bucket ~key ~meth:"GET"
    ?expires_in ()

let put_object ~region ~credentials ~now ~host ?port ~bucket ~key ?expires_in
    ?content_type () =
  let extra_query =
    match content_type with
    | Some content_type -> [ ("Content-Type", content_type) ]
    | None -> []
  in
  generate ~region ~credentials ~now ~host ?port ~bucket ~key ~meth:"PUT"
    ?expires_in ~extra_query ()
