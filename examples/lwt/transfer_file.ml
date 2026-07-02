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

let create_s3 () = S3.create () |> unwrap "create S3 client"

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

let run () =
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
  let s3 = create_s3 () in
  let* uploaded =
    S3.Object.Transfer.upload_file s3 ~bucket ~key ~path:upload_path
      ~on_progress:(progress "uploaded") ()
  in
  let uploaded = unwrap "upload file" uploaded in
  Format.printf "uploaded %s to s3://%a/%a with %s@." upload_path
    Awskit_s3.Bucket_name.pp bucket Awskit_s3.Object_key.pp key
    (Awskit_s3.Transfer.upload_strategy uploaded |> strategy_to_string);
  let* downloaded =
    S3.Object.Transfer.download_file s3 ~bucket ~key ~path:download_path
      ~on_progress:(progress "downloaded") ()
  in
  let downloaded = unwrap "download file" downloaded in
  Format.printf "downloaded to %s with %s@." download_path
    (Awskit_s3.Transfer.download_strategy downloaded
    |> download_strategy_to_string);
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
