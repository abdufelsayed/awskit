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
let upload_path = env_default "AWSKIT_EXAMPLE_FILE" "README.md"

let key =
  env_default "AWSKIT_EXAMPLE_KEY"
    ("awskit-examples/files/" ^ Filename.basename upload_path)

let download_path =
  env_default "AWSKIT_EXAMPLE_DOWNLOAD" (Filename.temp_file "awskit-s3-" ".bin")

let strategy_to_string = function
  | `Put -> "put-object"
  | `Multipart -> "multipart"

let download_strategy_to_string = function
  | `Get -> "get-object"
  | `Ranged -> "ranged"

let progress label bytes = Format.printf "%s %Ld bytes@." label bytes

let run () =
  let s3 = S3.create () |> unwrap "create S3 client" in
  let* uploaded =
    S3.Object.Transfer.upload_file s3 ~bucket ~key ~path:upload_path
      ~on_progress:(progress "uploaded") ()
  in
  let uploaded = unwrap "upload file" uploaded in
  Format.printf "uploaded %s to s3://%s/%s with %s@." upload_path bucket key
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

let () = Lwt_main.run (run ())
