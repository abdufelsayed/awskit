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

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/put-get.txt" in
  let body = env_default "AWSKIT_EXAMPLE_BODY" "Hello from awskit live S3." in
  let s3 = create_s3 stdenv sw in
  let put =
    S3.Object.put_string s3 ~bucket ~key ~contents:body ()
    |> unwrap "put object"
  in
  Format.printf "uploaded s3://%a/%a@." Awskit_s3.Bucket_name.pp bucket
    Awskit_s3.Object_key.pp key;
  Format.printf "etag: %a@."
    (Format.pp_print_option Awskit_s3.Object.Etag.pp)
    put.etag;
  let downloaded =
    S3.Object.get_string s3 ~bucket ~key ~max_bytes:1_048_576L ()
    |> unwrap "get object"
    |> fun result -> result.Awskit_s3.Object.Get.value
  in
  Format.printf "downloaded: %S@." downloaded

let main stdenv =
  try
    run stdenv;
    0
  with Example_error message ->
    Format.eprintf "error: %s@." message;
    1

let () = exit (Eio_main.run main)
