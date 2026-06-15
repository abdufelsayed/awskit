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

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/put-get.txt" in
  let body = env_default "AWSKIT_EXAMPLE_BODY" "Hello from awskit live S3." in
  let s3 = create_s3 stdenv sw in
  let put =
    S3.Object.put s3 ~bucket ~key ~body:(S3.Body.of_string body) ()
    |> unwrap "put object"
  in
  Format.printf "uploaded s3://%s/%s@." bucket key;
  Format.printf "etag: %a@."
    (Format.pp_print_option Awskit_s3.Object.Etag.pp)
    put.etag;
  let _info, downloaded =
    S3.Object.get s3 ~bucket ~key
      ~consume:(S3.Reader.to_string ~max_bytes:1_048_576L)
      ()
    |> unwrap "get object"
  in
  Format.printf "downloaded: %S@." downloaded

let main stdenv =
  try
    run stdenv;
    0
  with Awskit.Error.Awskit_error error ->
    Format.eprintf "error: %s@." (Awskit.Error.to_string_hum error);
    1

let () = exit (Eio_main.run main)
