module S3 = Awskit_s3_sim

let unwrap label = function
  | Ok value -> value
  | Error error ->
      Format.eprintf "%s: %a@." label Awskit_s3.Error.pp error;
      exit 1

let credentials =
  Awskit.Credentials.create_exn ~source:(`Custom "docs-simulator")
    ~access_key_id:"DOCSACCESSKEY" ~secret_access_key:"docs-secret" ()

let () =
  let bucket = Awskit_s3.Bucket_name.of_string_exn "docs-bucket" in
  let key = Awskit_s3.Object_key.of_string_exn "hello.txt" in
  let clock = S3.Clock.create () in
  let store = S3.create_store ~clock () in
  let s3 = S3.connect store ~credentials in
  S3.Bucket.create s3 ~bucket () |> unwrap "create bucket" |> ignore;
  S3.Object.put_string s3 ~bucket ~key ~contents:"hello from awskit" ()
  |> unwrap "put object"
  |> ignore;
  let got =
    S3.Object.get_string s3 ~bucket ~key ~max_bytes:1_048_576L ()
    |> unwrap "get object"
  in
  Format.printf "%s@." got.value
