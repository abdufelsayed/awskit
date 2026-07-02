module S3 = Awskit_s3_eio

let ( / ) = Eio.Path.( / )

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

let path_of_string env path =
  if Filename.is_relative path then Eio.Stdenv.cwd env / path
  else Eio.Stdenv.fs env / path

let strategy_to_string = function
  | `Put -> "put-object"
  | `Multipart -> "multipart"

let download_strategy_to_string = function
  | `Get -> "get-object"
  | `Ranged -> "ranged"

let progress label (event : Awskit_s3.Transfer.progress) =
  match event.total with
  | None -> Format.printf "%s %Ld bytes@." label event.transferred
  | Some total ->
      Format.printf "%s %Ld/%Ld bytes@." label event.transferred total

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let upload_path = env_default "AWSKIT_EXAMPLE_FILE" "README.md" in
  let key =
    env_default "AWSKIT_EXAMPLE_KEY"
      ("awskit-examples/files/" ^ Filename.basename upload_path)
  in
  let download_path =
    env_default "AWSKIT_EXAMPLE_DOWNLOAD"
      (Filename.temp_file "awskit-s3-" ".bin")
  in
  let s3 = create_s3 stdenv sw in
  let upload_file = path_of_string stdenv upload_path in
  let download_file = path_of_string stdenv download_path in
  let uploaded =
    S3.Object.Transfer.upload_file s3 ~bucket ~key ~path:upload_file
      ~on_progress:(progress "uploaded") ()
    |> unwrap "upload file"
  in
  Format.printf "uploaded %s to s3://%a/%a with %s@." upload_path
    Awskit_s3.Bucket_name.pp bucket Awskit_s3.Object_key.pp key
    (Awskit_s3.Transfer.upload_strategy uploaded |> strategy_to_string);
  let downloaded =
    S3.Object.Transfer.download_file s3 ~bucket ~key ~path:download_file
      ~on_progress:(progress "downloaded") ()
    |> unwrap "download file"
  in
  Format.printf "downloaded to %s with %s@." download_path
    (Awskit_s3.Transfer.download_strategy downloaded
    |> download_strategy_to_string)

let main stdenv =
  try
    run stdenv;
    0
  with Example_error message ->
    Format.eprintf "error: %s@." message;
    1

let () = exit (Eio_main.run main)
