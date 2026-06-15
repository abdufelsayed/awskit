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
let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/presigned.txt"
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
  let s3 = S3.create () |> unwrap "create S3 client" in
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

let () = Lwt_main.run (run ())
