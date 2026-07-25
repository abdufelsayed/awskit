module Make
    (C : Execution_request_context.S)
    (Observer :
      Awskit.Observability.For_service.Observer
        with type 'a io = 'a C.io
         and type connection = C.connection) =
struct
  open C

  let ( let* ) = bind

  module Observed_artifact = Observation.Artifact.Make (Observer)
  module Observed_signing = Observation.Artifact_signing.Make (Observer)

  type nonrec connection = connection
  type 'a io = 'a R.t

  let return_result ~operation ~bucket ~key result =
    match result with
    | Ok _ as result -> result
    | Error error ->
        Error (S3_error_context.with_s3_operation ~operation ~bucket ~key error)

  let with_artifact conn ~artifact_operation ~error_operation ~bucket ~key f =
    Observed_artifact.with_artifact conn ~operation:artifact_operation
      (fun () ->
        let* credentials = credentials conn in
        let bucket_context = Bucket_name.to_string bucket in
        let key_context = Object_key.to_string key in
        match credentials with
        | Error error ->
            return_error
              (S3_error_context.with_s3_operation ~operation:error_operation
                 ~bucket:bucket_context ~key:key_context error)
        | Ok credentials ->
            Observed_signing.with_signing conn ~operation:artifact_operation
              (fun () ->
                return
                  (return_result ~operation:error_operation
                     ~bucket:bucket_context ~key:key_context (f credentials))))

  let get_object conn ~bucket ~key ?options () =
    with_artifact conn ~artifact_operation:Artifact_operation.Presign_get_object
      ~error_operation:Operation.Get_object ~bucket ~key (fun credentials ->
        Presigned.get_object_with_endpoint_config ~region:(region conn)
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let put_object conn ~bucket ~key ?options () =
    with_artifact conn ~artifact_operation:Artifact_operation.Presign_put_object
      ~error_operation:Operation.Put_object ~bucket ~key (fun credentials ->
        Presigned.put_object_with_endpoint_config ~region:(region conn)
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let head_object conn ~bucket ~key ?options () =
    with_artifact conn
      ~artifact_operation:Artifact_operation.Presign_head_object
      ~error_operation:Operation.Head_object ~bucket ~key (fun credentials ->
        Presigned.head_object_with_endpoint_config ~region:(region conn)
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let delete_object conn ~bucket ~key ?options () =
    with_artifact conn
      ~artifact_operation:Artifact_operation.Presign_delete_object
      ~error_operation:Operation.Delete_object ~bucket ~key (fun credentials ->
        Presigned.delete_object_with_endpoint_config ~region:(region conn)
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~bucket ~key ?options ())

  let upload_part conn ~upload ~part_number ?options () =
    let bucket = Multipart.Upload.bucket upload in
    let key = Multipart.Upload.key upload in
    with_artifact conn
      ~artifact_operation:Artifact_operation.Presign_upload_part
      ~error_operation:Operation.Upload_part ~bucket ~key (fun credentials ->
        Presigned.upload_part_with_endpoint_config ~region:(region conn)
          ~credentials ~now:(now conn) ~endpoint_config:(endpoint_config conn)
          ~upload ~part_number ?options ())
end
