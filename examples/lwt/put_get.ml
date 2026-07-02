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
let object_key value = value
let create_s3 () = S3.create () |> unwrap "create S3 client"

let run () =
  let bucket = bucket_name (env "AWSKIT_EXAMPLE_BUCKET") in
  let key =
    object_key (env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/put-get.txt")
  in
  let body = env_default "AWSKIT_EXAMPLE_BODY" "Hello from awskit live S3." in
  let s3 = create_s3 () in
  let* put = S3.Object.put_string s3 ~bucket ~key ~contents:body () in
  let put = unwrap "put object" put in
  Format.printf "uploaded s3://%s/%s@." bucket key;
  Format.printf "etag: %a@."
    (Format.pp_print_option Awskit_s3.Object.Etag.pp)
    put.etag;
  let* got = S3.Object.get_string s3 ~bucket ~key ~max_bytes:1_048_576L () in
  let downloaded = (unwrap "get object" got).Awskit_s3.Object.Get.value in
  Format.printf "downloaded: %S@." downloaded;
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
