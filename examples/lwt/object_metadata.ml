open Lwt.Syntax
module S3 = Awskit_s3_lwt_unix

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
let create_s3 () = S3.create () |> unwrap "create S3 client"

let put_options =
  Awskit_s3.Object.Put.options_exn
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

let run () =
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let key =
    object_key (env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/metadata.txt")
  in
  let s3 = create_s3 () in
  let* put =
    S3.Object.put s3 ~bucket ~key ~options:put_options
      ~body:(S3.Body.of_string "object metadata example")
      ()
  in
  ignore (unwrap "put object" put);
  let* head = S3.Object.head s3 ~bucket ~key () in
  let head = unwrap "head object" head in
  Format.printf "s3://%a/%a@." Awskit_s3.Bucket_name.pp bucket
    Awskit_s3.Object_key.pp key;
  Format.printf "content-type: %a@."
    (Format.pp_print_option Format.pp_print_string)
    (Option.map Awskit_s3.Content_type.to_string head.content_type);
  Format.printf "metadata:@.";
  List.iter
    (fun (name, value) -> Format.printf "  %s=%s@." name value)
    (Awskit_s3.Metadata.to_list head.metadata);
  let* tags = S3.Object.Tagging.get s3 ~bucket ~key () in
  let tags = unwrap "get object tags" tags in
  Format.printf "tags:@.";
  List.iter
    (fun tag ->
      Format.printf "  %s=%s@." (Awskit_s3.Tag.key tag)
        (Awskit_s3.Tag.value tag))
    (Awskit_s3.Tag.Set.to_list tags.tags);
  Lwt.return_unit

let main () =
  Lwt.catch
    (fun () ->
      let* () = run () in
      Lwt.return 0)
    (function
      | Example_error message ->
          let* () = Lwt_io.eprintf "error: %s\n" message in
          Lwt.return 1
      | exn -> Lwt.fail exn)

let () = exit (Lwt_main.run (main ()))
