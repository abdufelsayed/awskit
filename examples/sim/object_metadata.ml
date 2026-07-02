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
  let bucket = "docs-bucket" in
  let key = "notes/today.txt" in
  let clock = S3.Clock.create () in
  let store = S3.create_store ~clock () in
  let s3 = S3.connect store ~credentials in
  S3.Bucket.create s3 ~bucket () |> unwrap "create bucket" |> ignore;
  let metadata =
    Awskit_s3.Metadata.of_list_exn [ ("origin", "simulator-example") ]
  in
  let tags =
    Awskit_s3.Tag.Set.of_list_exn
      [ Awskit_s3.Tag.create_exn ~key:"purpose" ~value:"docs" ]
  in
  let options =
    Awskit_s3.Object.Put.options_exn ~content_type:"text/plain" ~metadata ~tags
      ()
  in
  S3.Object.put_string s3 ~bucket ~key ~options ~contents:"remember the docs" ()
  |> unwrap "put object"
  |> ignore;
  let head = S3.Object.head s3 ~bucket ~key () |> unwrap "head object" in
  Format.printf "content type: %a@."
    (Format.pp_print_option Awskit_s3.Content_type.pp)
    head.content_type;
  List.iter
    (fun (name, value) -> Format.printf "metadata %s=%s@." name value)
    (Awskit_s3.Metadata.to_list head.metadata);
  let tag_result =
    S3.Object.Tagging.get s3 ~bucket ~key () |> unwrap "get tags"
  in
  List.iter
    (fun tag ->
      Format.printf "tag %s=%s@." (Awskit_s3.Tag.key tag)
        (Awskit_s3.Tag.value tag))
    (Awskit_s3.Tag.Set.to_list tag_result.tags)
