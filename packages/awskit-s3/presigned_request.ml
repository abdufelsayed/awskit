module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec client = connection
  type 'a io = 'a R.t

  let return_result ~operation ~bucket ~key result =
    match result with
    | Ok _ as result -> result
    | Error error ->
        Error (S3_error_context.with_s3_operation ~operation ~bucket ~key error)

  let with_credentials conn ~operation ~bucket ~key f =
    let* credentials = credentials conn in
    let bucket_context = Bucket_name.to_string bucket in
    let key_context = Object_key.to_string key in
    match credentials with
    | Error error ->
        return_error
          (S3_error_context.with_s3_operation ~operation ~bucket:bucket_context
             ~key:key_context error)
    | Ok credentials ->
        return
          (return_result ~operation ~bucket:bucket_context ~key:key_context
             (f credentials))

  let signer conn credentials =
    Presigned.Signer.create ~region:(region conn) ~credentials
      ~endpoint_config:(endpoint_config conn) ()

  let get_object conn ~bucket ~key ?expires_in ?additional_headers
      ?response_overrides ?options () =
    with_credentials conn ~operation:"GetObject" ~bucket ~key
      (fun credentials ->
        Presigned.Signer.get_object (signer conn credentials) ~now:(now conn)
          ~bucket ~key ?expires_in ?additional_headers ?response_overrides
          ?options ())

  let put_object conn ~bucket ~key ?expires_in ?additional_headers ?options () =
    with_credentials conn ~operation:"PutObject" ~bucket ~key
      (fun credentials ->
        Presigned.Signer.put_object (signer conn credentials) ~now:(now conn)
          ~bucket ~key ?expires_in ?additional_headers ?options ())

  let head_object conn ~bucket ~key ?expires_in ?additional_headers
      ?response_overrides ?options () =
    with_credentials conn ~operation:"HeadObject" ~bucket ~key
      (fun credentials ->
        Presigned.Signer.head_object (signer conn credentials) ~now:(now conn)
          ~bucket ~key ?expires_in ?additional_headers ?response_overrides
          ?options ())

  let delete_object conn ~bucket ~key ?expires_in ?additional_headers ?options
      () =
    with_credentials conn ~operation:"DeleteObject" ~bucket ~key
      (fun credentials ->
        Presigned.Signer.delete_object (signer conn credentials) ~now:(now conn)
          ~bucket ~key ?expires_in ?additional_headers ?options ())

  let upload_part conn ~upload ~part_number ?expires_in ?additional_headers
      ?options () =
    let bucket = Multipart.Upload.bucket upload in
    let key = Multipart.Upload.key upload in
    with_credentials conn ~operation:"UploadPart" ~bucket ~key
      (fun credentials ->
        Presigned.Signer.upload_part (signer conn credentials) ~now:(now conn)
          ~upload ~part_number ?expires_in ?additional_headers ?options ())
end
