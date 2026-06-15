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
let expires_in = Ptime.Span.of_int_s (15 * 60)

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let print_presigned label (result : Awskit_s3.Presigned.result) =
  Format.printf "%s@." label;
  Format.printf "method: %s@." (method_to_string result.method_);
  Format.printf "url: %s@." result.url;
  (match result.signed_headers with
  | [] -> ()
  | headers ->
      Format.printf "signed headers:@.";
      List.iter
        (fun (name, value) -> Format.printf "  %s: %s@." name value)
        headers);
  Format.printf "@."

let run () =
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/presigned.txt" in
  let s3 = create_s3 () in
  let put_options =
    {
      Awskit_s3.Presigned.Put_object.default_options with
      expires_in = Some expires_in;
      content_type = Some "text/plain";
    }
  in
  let get_options =
    {
      Awskit_s3.Presigned.Get_object.default_options with
      expires_in = Some expires_in;
      response_content_type = Some "text/plain";
    }
  in
  let* put = S3.Presigned.put_object s3 ~bucket ~key ~options:put_options () in
  print_presigned "presigned PUT" (unwrap "presign put" put);
  let* get = S3.Presigned.get_object s3 ~bucket ~key ~options:get_options () in
  print_presigned "presigned GET" (unwrap "presign get" get);
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
