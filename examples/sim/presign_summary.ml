module S3 = Awskit_s3_sim

let unwrap label = function
  | Ok value -> value
  | Error error ->
      Format.eprintf "%s: %a@." label Awskit_s3.Error.pp error;
      exit 1

let credentials =
  Awskit.Credentials.create_exn ~source:(`Custom "docs-simulator")
    ~access_key_id:"DOCSACCESSKEY" ~secret_access_key:"docs-secret" ()

let method_to_string = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let print_presigned label result =
  Format.printf "%s@." label;
  Format.printf "method: %s@."
    (method_to_string (Awskit_s3.Presigned.method_ result));
  Format.printf "safe uri: %a@." Uri.pp (Awskit_s3.Presigned.safe_uri result);
  Format.printf "summary: %a@." Awskit_s3.Presigned.pp result;
  Format.printf "expires at: %s@."
    (Awskit_s3.Presigned.expires_at result |> Ptime.to_rfc3339);
  (match Awskit_s3.Presigned.request_headers result with
  | [] -> ()
  | headers ->
      Format.printf "request header names:@.";
      List.iter (fun (name, _) -> Format.printf "  %s@." name) headers);
  Format.printf
    "raw bearer URL intentionally omitted; reveal it only at the HTTP handoff@.";
  Format.printf "@."

let () =
  let bucket = Awskit_s3.Bucket_name.of_string_exn "docs-bucket" in
  let key = Awskit_s3.Object_key.of_string_exn "upload.txt" in
  let clock = S3.Clock.create () in
  let store = S3.create_store ~clock () in
  let s3 = S3.connect store ~credentials in
  S3.Bucket.create s3 ~bucket () |> unwrap "create bucket" |> ignore;
  let expires_in =
    Awskit_s3.Presigned.Lifetime.of_span_exn (Ptime.Span.of_int_s (15 * 60))
  in
  let put_options =
    Awskit_s3.Object.Put.options
      ~content_type:(Awskit_s3.Content_type.of_string_exn "text/plain")
      ()
  in
  S3.Presigned.put_object s3 ~bucket ~key ~expires_in ~options:put_options ()
  |> unwrap "presign PUT"
  |> print_presigned "presigned PUT";
  S3.Presigned.get_object s3 ~bucket ~key ~expires_in ()
  |> unwrap "presign GET"
  |> print_presigned "presigned GET"
