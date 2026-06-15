open Common

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t

  let return_result ~operation ~bucket ~key result =
    match result with
    | Ok _ as result -> result
    | Error error -> Error (with_s3_operation ~operation ~bucket ~key error)

  let with_credentials conn ~operation ~bucket ~key f =
    let* credentials = R.credentials conn in
    match credentials with
    | Error error ->
        return_error (with_s3_operation ~operation ~bucket ~key error)
    | Ok credentials ->
        return (return_result ~operation ~bucket ~key (f credentials))

  let get_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"GetObject" ~bucket ~key
      (fun credentials ->
        Presigned.get_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let put_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"PutObject" ~bucket ~key
      (fun credentials ->
        Presigned.put_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let head_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"HeadObject" ~bucket ~key
      (fun credentials ->
        Presigned.head_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let delete_object conn ~bucket ~key ?options () =
    with_credentials conn ~operation:"DeleteObject" ~bucket ~key
      (fun credentials ->
        Presigned.delete_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let upload_part conn ~bucket ~key ~upload_id ~part_number ?options () =
    with_credentials conn ~operation:"UploadPart" ~bucket ~key
      (fun credentials ->
        Presigned.upload_part_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ~upload_id ~part_number ?options ())
end
