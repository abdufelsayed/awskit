open Base

let s3_uri ?key bucket =
  match key with
  | None -> Fmt.str "s3://%s" bucket
  | Some key -> Fmt.str "s3://%s/%s" bucket key

let with_s3_operation ~operation ?bucket ?key error =
  let operation = Awskit_s3.Operation.to_string operation in
  let resource = Option.map bucket ~f:(fun bucket -> s3_uri ?key bucket) in
  let already_present =
    List.exists (Awskit.Error.context error) ~f:(function
      | Awskit.Error.Operation
          { service = Some "S3"; name; resource = existing } ->
          String.equal name operation
          && Option.equal String.equal existing resource
      | _ -> false)
  in
  if already_present then error
  else
    Awskit.Error.Producer.with_operation ~service:"S3" ~name:operation ?resource
      () error
