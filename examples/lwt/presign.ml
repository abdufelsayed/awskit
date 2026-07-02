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

let span_to_string span =
  match Ptime.Span.to_int_s span with
  | Some seconds -> Format.sprintf "%ds" seconds
  | None -> Format.asprintf "%a" Ptime.Span.pp span

let print_presigned label (result : Awskit_s3.Presigned.result) =
  Format.printf "%s@." label;
  Format.printf "method: %s@."
    (method_to_string (Awskit_s3.Presigned.method_ result));
  Format.printf "safe uri: %a@." Uri.pp (Awskit_s3.Presigned.safe_uri result);
  Format.printf "requested expiry: %s@."
    (span_to_string (Awskit_s3.Presigned.requested_expires_in result));
  Format.printf "effective expiry: %s@."
    (span_to_string (Awskit_s3.Presigned.effective_expires_in result));
  (match Awskit_s3.Presigned.expires_at result with
  | None -> ()
  | Some expires_at ->
      Format.printf "expires at: %s@." (Ptime.to_rfc3339 expires_at));
  (match Awskit_s3.Presigned.request_headers result with
  | [] -> ()
  | headers ->
      Format.printf "request headers:@.";
      List.iter (fun (name, _) -> Format.printf "  %s@." name) headers);
  Format.printf
    "bearer URL: call Awskit_s3.Presigned.reveal_url only when handing off to \
     the HTTP client@.";
  Format.printf "@."

let run () =
  let bucket = env "AWSKIT_EXAMPLE_BUCKET" in
  let key = env_default "AWSKIT_EXAMPLE_KEY" "awskit-examples/presigned.txt" in
  let s3 = create_s3 () in
  let put_options =
    {
      Awskit_s3.Presigned.Put_object.default_options with
      expires_in = Some expires_in;
      content_type = Some (Awskit_s3.Content_type.of_string_exn "text/plain");
    }
  in
  let get_options =
    {
      Awskit_s3.Presigned.Get_object.default_options with
      expires_in = Some expires_in;
      response_content_type =
        Some (Awskit_s3.Content_type.of_string_exn "text/plain");
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
