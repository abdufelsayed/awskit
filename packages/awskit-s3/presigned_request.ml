module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t

  let return_result ~operation ~bucket ~key result =
    match result with
    | Ok _ as result -> result
    | Error error ->
        Error (S3_error_context.with_s3_operation ~operation ~bucket ~key error)

  let with_credentials conn ~operation ~bucket ~key f =
    let* credentials = credentials conn in
    match credentials with
    | Error error ->
        return_error
          (S3_error_context.with_s3_operation ~operation ~bucket ~key error)
    | Ok credentials ->
        return (return_result ~operation ~bucket ~key (f credentials))

  let get_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"GetObject" ~bucket ~key
      (fun credentials ->
        Presigned.get_object_with_endpoint_config
          ~region:(Awskit.Region.to_string (region conn))
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let put_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"PutObject" ~bucket ~key
      (fun credentials ->
        Presigned.put_object_with_endpoint_config
          ~region:(Awskit.Region.to_string (region conn))
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let head_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"HeadObject" ~bucket ~key
      (fun credentials ->
        Presigned.head_object_with_endpoint_config
          ~region:(Awskit.Region.to_string (region conn))
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let delete_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"DeleteObject" ~bucket ~key
      (fun credentials ->
        Presigned.delete_object_with_endpoint_config
          ~region:(Awskit.Region.to_string (region conn))
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let upload_part conn ~upload ~part_number ?options () =
    let bucket = Multipart.Upload.bucket upload |> Bucket_name.to_string in
    let key = Multipart.Upload.key upload |> Object_key.to_string in
    with_credentials conn ~operation:"UploadPart" ~bucket ~key
      (fun credentials ->
        Presigned.upload_part_with_endpoint_config
          ~region:(Awskit.Region.to_string (region conn))
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~upload ~part_number ?options ())
end
