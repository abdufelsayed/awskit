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
let prefix = env_default "AWSKIT_EXAMPLE_PREFIX" "awskit-examples/"

let run () =
  let s3 = S3.create () |> unwrap "create S3 client" in
  let options =
    {
      Awskit_s3.Object.List.default_options with
      prefix = Some prefix;
      max_keys = Some 25;
    }
  in
  let* objects =
    S3.Object.List_objects_v2.objects s3 ~bucket ~options ~max_pages:4 ()
  in
  let objects = unwrap "list objects" objects in
  Format.printf "s3://%s/%s@." bucket prefix;
  List.iter
    (fun (object_ : Awskit_s3.Object.List.object_summary) ->
      Format.printf "- %s" object_.key;
      Option.iter (Format.printf " (%Ld bytes)") object_.size;
      Format.printf "@.")
    objects;
  if objects = [] then Format.printf "no objects found@.";
  Lwt.return_unit

let () = Lwt_main.run (run ())
