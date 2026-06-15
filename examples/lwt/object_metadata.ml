open Lwt.Syntax
module S3 = Awskit_s3_lwt_unix

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
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/metadata.txt" in
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

let main () =
  Lwt.catch
    (fun () ->
      let* () = run () in
      Lwt.return 0)
    (function
      | Awskit.Error.Awskit_error error ->
          let* () =
            Lwt_io.eprintf "error: %s\n" (Awskit.Error.to_string_hum error)
          in
          Lwt.return 1
      | exn -> Lwt.fail exn)

let () = exit (Lwt_main.run (main ()))
