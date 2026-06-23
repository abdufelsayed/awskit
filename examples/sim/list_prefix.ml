module S3 = Awskit_s3_sim

let unwrap label = function
  | Ok value -> value
  | Error error ->
      Format.eprintf "%s: %a@." label Awskit_s3.Error.pp error;
      exit 1

let credentials =
  Awskit.Credentials.create_exn ~source:(`Custom "docs-simulator")
    ~access_key_id:"DOCSACCESSKEY" ~secret_access_key:"docs-secret" ()

let put s3 ~bucket key contents =
  let key = Awskit_s3.Object_key.of_string_exn key in
  S3.Object.put_string s3 ~bucket ~key ~contents ()
  |> unwrap ("put " ^ Awskit_s3.Object_key.to_string key)
  |> ignore

let () =
  let bucket = Awskit_s3.Bucket_name.of_string_exn "docs-bucket" in
  let clock = S3.Clock.create () in
  let store = S3.create_store ~clock () in
  let s3 = S3.connect store ~credentials in
  S3.Bucket.create s3 ~bucket () |> unwrap "create bucket" |> ignore;
  put s3 ~bucket "logs/2026-06-21.txt" "started";
  put s3 ~bucket "logs/2026-06-22.txt" "continued";
  put s3 ~bucket "images/logo.png" "not a real png";
  let options =
    Awskit_s3.Object.List.options_exn
      ~prefix:(Awskit_s3.Object_key.Prefix.of_string_exn "logs/")
      ()
  in
  let keys =
    S3.Object.List.keys s3 ~bucket ~options ~max_pages:4 ()
    |> unwrap "list keys"
  in
  List.iter (fun key -> Format.printf "%a@." Awskit_s3.Object_key.pp key) keys
