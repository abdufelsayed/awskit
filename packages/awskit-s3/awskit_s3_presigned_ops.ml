open Awskit_s3_core

module Make (C : Awskit_s3_operation_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t

  let with_credentials conn f =
    let* credentials = R.credentials conn in
    match credentials with
    | Error error -> return_error error
    | Ok credentials -> return (f credentials)

  let get_object conn ~bucket ~key ?options () =
    with_credentials conn (fun credentials ->
        Presigned.get_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let put_object conn ~bucket ~key ?options () =
    with_credentials conn (fun credentials ->
        Presigned.put_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let head_object conn ~bucket ~key ?options () =
    with_credentials conn (fun credentials ->
        Presigned.head_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let delete_object conn ~bucket ~key ?expires_in () =
    with_credentials conn (fun credentials ->
        Presigned.delete_object_with_endpoint_config ~region:(R.region conn)
          ~credentials ~now:(R.now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?expires_in ())
end
