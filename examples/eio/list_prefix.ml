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

let bucket_name value = value

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
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let prefix = env_default "AWSKIT_EXAMPLE_PREFIX" "awskit-examples/" in
  let s3 = create_s3 stdenv sw in
  let options = Awskit_s3.Object.List.options_exn ~prefix ~max_keys:25 () in
  let objects =
    S3.Object.List.objects s3 ~bucket ~options ~max_pages:4 ()
    |> unwrap "list objects"
  in
  Format.printf "s3://%s/%s@." bucket prefix;
  List.iter
    (fun (object_ : Awskit_s3.Object.List.object_summary) ->
      Format.printf "- %s" (Awskit_s3.Object_key.to_string object_.key);
      Option.iter (Format.printf " (%Ld bytes)") object_.size;
      Format.printf "@.")
    objects;
  if objects = [] then Format.printf "no objects found@."

let main stdenv =
  try
    run stdenv;
    0
  with Example_error message ->
    Format.eprintf "error: %s@." message;
    1

let () = exit (Eio_main.run main)
