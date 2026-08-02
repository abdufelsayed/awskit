let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Producer.validation ?field message))
    fmt

let decode fmt = Fmt.kstr Awskit.Error.Producer.decode fmt

let decode_with_context ~what message =
  Awskit.Error.Producer.decode message
  |> Awskit.Error.Producer.with_context (Fmt.str "decoding %s" what)

let s3_uri ?key bucket =
  match key with
  | None -> Fmt.str "s3://%s" bucket
  | Some key -> Fmt.str "s3://%s/%s" bucket key

let with_s3_operation ~operation ?bucket ?key error =
  let operation = Operation.to_string operation in
  let resource = Option.map (fun bucket -> s3_uri ?key bucket) bucket in
  let already_present =
    List.exists
      (function
        | Awskit.Error.Operation
            { service = Some "S3"; name; resource = existing } ->
            String.equal name operation
            && Option.equal String.equal existing resource
        | _ -> false)
      (Awskit.Error.context error)
  in
  if already_present then error
  else
    Awskit.Error.Producer.with_operation ~service:"S3" ~name:operation ?resource
      () error

let return_s3_error return_error ~operation ?bucket ?key error =
  return_error (with_s3_operation ~operation ?bucket ?key error)
