open Lwt.Syntax
module S3 = Awskit_s3_lwt_unix

let env name =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> failwith (Printf.sprintf "Set %s" name)

let env_default name default =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> default

let unwrap label = function
  | Ok value -> value
  | Error error ->
      failwith (Format.asprintf "%s: %a" label Awskit_s3.Error.pp error)

let bucket = env "AWSKIT_EXAMPLE_BUCKET"
let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/metadata.txt"

let put_options =
  {
    Awskit_s3.Object.Put.default_options with
    content_type = Some "text/plain";
    metadata = [ ("source", "awskit-example"); ("kind", "metadata-demo") ];
    tags =
      [
        { Awskit_s3.Tag.key = "project"; value = "awskit" };
        { Awskit_s3.Tag.key = "example"; value = "object-metadata" };
      ];
  }

let run () =
  let s3 = S3.create () |> unwrap "create S3 client" in
  let* put =
    S3.Object.put s3 ~bucket ~key ~options:put_options
      ~body:(S3.Body.of_string "object metadata example")
      ()
  in
  ignore (unwrap "put object" put);
  let* head = S3.Object.head s3 ~bucket ~key () in
  let head = unwrap "head object" head in
  Format.printf "s3://%s/%s@." bucket key;
  Format.printf "content-type: %a@."
    (Format.pp_print_option Format.pp_print_string)
    head.content_type;
  Format.printf "metadata:@.";
  List.iter
    (fun (name, value) -> Format.printf "  %s=%s@." name value)
    head.metadata;
  let* tags = S3.Object.Tagging.get s3 ~bucket ~key () in
  let tags = unwrap "get object tags" tags in
  Format.printf "tags:@.";
  List.iter
    (fun (tag : Awskit_s3.Tag.t) -> Format.printf "  %s=%s@." tag.key tag.value)
    tags.tags;
  Lwt.return_unit

let () = Lwt_main.run (run ())
