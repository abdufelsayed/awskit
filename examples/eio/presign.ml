module S3 = Awskit_s3_eio

exception Example_error of string

let fail fmt =
  Format.kasprintf (fun message -> raise (Example_error message)) fmt

let env name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> fail "set %s" name

let env_default name default =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> default

let unwrap label = function
  | Ok value -> value
  | Error error -> fail "%s: %a" label Awskit_s3.Error.pp error

let bucket_name value = Awskit_s3.Bucket_name.of_string_exn value
let object_key value = Awskit_s3.Object_key.of_string_exn value

let or_fail_msg label = function
  | Ok value -> value
  | Error (`Msg message) -> fail "%s: %s" label message

let https_connector () =
  Mirage_crypto_rng_unix.use_default ();
  let authenticator =
    Ca_certs.authenticator () |> or_fail_msg "load CA certificates"
  in
  let tls_config =
    Tls.Config.client ~authenticator () |> or_fail_msg "create TLS config"
  in
  Some
    (fun uri raw ->
      let host =
        match Uri.host uri with
        | Some host -> Domain_name.host_exn (Domain_name.of_string_exn host)
        | None -> fail "HTTPS URI is missing a host"
      in
      (Tls_eio.client_of_flow tls_config ~host raw
        :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Flow.two_way))

let create_s3 stdenv sw =
  let region =
    match Sys.getenv_opt "AWS_REGION" with
    | Some value when String.trim value <> "" -> value
    | _ -> env "AWS_DEFAULT_REGION"
  in
  let credentials =
    Awskit_unix.Credentials.default_chain () |> unwrap "load AWS credentials"
  in
  S3.create ~sw ~env:stdenv ~https:(https_connector ()) ~region ~credentials ()
  |> unwrap "create S3 client"

let expires_in =
  Awskit_s3.Presigned.Lifetime.of_span_exn (Ptime.Span.of_int_s (15 * 60))

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let span_to_string span =
  match Ptime.Span.to_int_s span with
  | Some seconds -> Format.sprintf "%ds" seconds
  | None -> Format.asprintf "%a" Ptime.Span.pp span

let print_presigned label (result : Awskit_s3.Presigned.result) =
  Format.printf "%s@." label;
  Format.printf "method: %s@."
    (method_to_string (Awskit_s3.Presigned.method_ result));
  Format.printf "safe uri: %a@." Uri.pp (Awskit_s3.Presigned.safe_uri result);
  Format.printf "requested expiry: %s@."
    (span_to_string (Awskit_s3.Presigned.requested_expires_in result));
  Format.printf "effective expiry: %s@."
    (span_to_string (Awskit_s3.Presigned.effective_expires_in result));
  Format.printf "expires at: %s@."
    (Awskit_s3.Presigned.expires_at result |> Ptime.to_rfc3339);
  (match Awskit_s3.Presigned.request_headers result with
  | [] -> ()
  | headers ->
      Format.printf "request headers:@.";
      List.iter (fun (name, _) -> Format.printf "  %s@." name) headers);
  Format.printf
    "bearer URL: call Awskit_s3.Presigned.reveal_url only when handing off to \
     the HTTP client@.";
  Format.printf "@."

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let key =
    object_key
      (env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/presigned.txt")
  in
  let s3 = create_s3 stdenv sw in
  let content_type = Awskit_s3.Content_type.of_string_exn "text/plain" in
  let put_options = Awskit_s3.Object.Put.options ~content_type () in
  let response_overrides =
    Awskit_s3.Object.Response_overrides.create ~content_type ()
  in
  let put =
    S3.Presigned.put_object s3 ~bucket ~key ~expires_in ~options:put_options ()
    |> unwrap "presign put"
  in
  print_presigned "presigned PUT" put;
  let get =
    S3.Presigned.get_object s3 ~bucket ~key ~expires_in ~response_overrides ()
    |> unwrap "presign get"
  in
  print_presigned "presigned GET" get

let main stdenv =
  try
    run stdenv;
    0
  with Example_error message ->
    Format.eprintf "error: %s@." message;
    1

let () = exit (Eio_main.run main)
