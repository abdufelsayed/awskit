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
let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/put-get.txt"
let body = env_default "AWSKIT_EXAMPLE_BODY" "Hello from awskit live S3."

let run () =
  let s3 = S3.create () |> unwrap "create S3 client" in
  let* put = S3.Object.put s3 ~bucket ~key ~body:(S3.Body.of_string body) () in
  let put = unwrap "put object" put in
  Format.printf "uploaded s3://%s/%s@." bucket key;
  Format.printf "etag: %a@."
    (Format.pp_print_option Awskit_s3.Object.Etag.pp)
    put.etag;
  let* got =
    S3.Object.get s3 ~bucket ~key
      ~consume:(S3.Reader.to_string ~max_bytes:1_048_576L)
      ()
  in
  let _info, downloaded = unwrap "get object" got in
  Format.printf "downloaded: %S@." downloaded;
  Lwt.return_unit

let () = Lwt_main.run (run ())
