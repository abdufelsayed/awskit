module S3 = Awskit_s3_eio

let env name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ ->
      Awskit.Error.validation ~field:name "environment variable is required"
      |> Awskit.Error.raise

let env_default name default =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> default

let unwrap label = function
  | Ok value -> value
  | Error error -> Awskit.Error.with_context label error |> Awskit.Error.raise

let or_raise_msg label = function
  | Ok value -> value
  | Error (`Msg message) ->
      Awskit.Error.transport ~retryable:false message
      |> Awskit.Error.with_context label
      |> Awskit.Error.raise

let https_connector () =
  Mirage_crypto_rng_unix.use_default ();
  let authenticator =
    Ca_certs.authenticator () |> or_raise_msg "load CA certificates"
  in
  let tls_config =
    Tls.Config.client ~authenticator () |> or_raise_msg "create TLS config"
  in
  Some
    (fun uri raw ->
      let host =
        match Uri.host uri with
        | Some host -> Domain_name.host_exn (Domain_name.of_string_exn host)
        | None ->
            Awskit.Error.validation ~field:"uri" "HTTPS URI is missing a host"
            |> Awskit.Error.raise
      in
      (Tls_eio.client_of_flow tls_config ~host raw
        :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Flow.two_way))

let create_s3 env sw =
  let region = Awskit_unix.Region.from_env () |> unwrap "load AWS region" in
  let credentials =
    Awskit_unix.Credentials.default_chain () |> unwrap "load AWS credentials"
  in
  S3.create ~sw ~env ~https:(https_connector ()) ~region ~credentials ()

let expires_in = Ptime.Span.of_int_s (15 * 60)

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let print_presigned label (result : Awskit_s3.Presigned.result) =
  Format.printf "%s@." label;
  Format.printf "method: %s@." (method_to_string result.method_);
  Format.printf "url: %s@." result.url;
  (match result.signed_headers with
  | [] -> ()
  | headers ->
      Format.printf "signed headers:@.";
      List.iter
        (fun (name, value) -> Format.printf "  %s: %s@." name value)
        headers);
  Format.printf "@."

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/presigned.txt" in
  let s3 = create_s3 stdenv sw in
  let put_options =
    {
      Awskit_s3.Presigned.Put_object.default_options with
      expires_in = Some expires_in;
      content_type = Some "text/plain";
    }
  in
  let get_options =
    {
      Awskit_s3.Presigned.Get_object.default_options with
      expires_in = Some expires_in;
      response_content_type = Some "text/plain";
    }
  in
  let put =
    S3.Presigned.put_object s3 ~bucket ~key ~options:put_options ()
    |> unwrap "presign put"
  in
  print_presigned "presigned PUT" put;
  let get =
    S3.Presigned.get_object s3 ~bucket ~key ~options:get_options ()
    |> unwrap "presign get"
  in
  print_presigned "presigned GET" get

let main stdenv =
  try
    run stdenv;
    0
  with Awskit.Error.Awskit_error error ->
    Format.eprintf "error: %s@." (Awskit.Error.to_string_hum error);
    1

let () = exit (Eio_main.run main)
