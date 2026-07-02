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

let bucket_name value = value
let create_s3 () = S3.create () |> unwrap "create S3 client"

let run () =
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let prefix = env_default "AWSKIT_EXAMPLE_PREFIX" "awskit-examples/" in
  let s3 = create_s3 () in
  let options = Awskit_s3.Object.List.options_exn ~prefix ~max_keys:25 () in
  let* objects = S3.Object.List.objects s3 ~bucket ~options ~max_pages:4 () in
  let objects = unwrap "list objects" objects in
  Format.printf "s3://%s/%s@." bucket prefix;
  List.iter
    (fun (object_ : Awskit_s3.Object.List.object_summary) ->
      Format.printf "- %s" (Awskit_s3.Object_key.to_string object_.key);
      Option.iter (Format.printf " (%Ld bytes)") object_.size;
      Format.printf "@.")
    objects;
  if objects = [] then Format.printf "no objects found@.";
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
