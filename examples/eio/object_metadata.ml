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

let put_options =
  Awskit_s3.Object.Put.options
    ~content_type:(Awskit_s3.Content_type.of_string_exn "text/plain")
    ~metadata:
      (Awskit_s3.Metadata.of_list_exn
         [ ("source", "awskit-example"); ("kind", "metadata-demo") ])
    ~tags:
      (Awskit_s3.Tag.Set.of_list_exn
         [
           Awskit_s3.Tag.create_exn ~key:"project" ~value:"awskit";
           Awskit_s3.Tag.create_exn ~key:"example" ~value:"object-metadata";
         ])
    ()

let run stdenv =
  Eio.Switch.run @@ fun sw ->
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let key =
    object_key (env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/metadata.txt")
  in
  let s3 = create_s3 stdenv sw in
  ignore
    (S3.Object.put_string s3 ~bucket ~key ~options:put_options
       ~contents:"object metadata example" ()
    |> unwrap "put object");
  let head = S3.Object.head s3 ~bucket ~key () |> unwrap "head object" in
  Format.printf "s3://%a/%a@." Awskit_s3.Bucket_name.pp bucket
    Awskit_s3.Object_key.pp key;
  Format.printf "content-type: %a@."
    (Format.pp_print_option Format.pp_print_string)
    (Option.map Awskit_s3.Content_type.to_string head.content_type);
  Format.printf "metadata:@.";
  List.iter
    (fun (name, value) -> Format.printf "  %s=%s@." name value)
    (Awskit_s3.Metadata.to_list head.metadata);
  let tags =
    S3.Object.Tagging.get s3 ~bucket ~key () |> unwrap "get object tags"
  in
  Format.printf "tags:@.";
  List.iter
    (fun tag ->
      Format.printf "  %s=%s@." (Awskit_s3.Tag.key tag)
        (Awskit_s3.Tag.value tag))
    (Awskit_s3.Tag.Set.to_list tags.tags)

let main stdenv =
  try
    run stdenv;
    0
  with Example_error message ->
    Format.eprintf "error: %s@." message;
    1

let () = exit (Eio_main.run main)
