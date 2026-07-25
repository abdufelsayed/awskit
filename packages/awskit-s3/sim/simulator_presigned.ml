open Simulator_support
open Simulator_state
open Simulator_runtime
module Presigned_model = Awskit_s3.Presigned
module Artifact_operation = Awskit_s3.Artifact_operation
module Observability = Awskit_s3.Observability.For_simulator
module Outcome = Awskit.Observability.Outcome

module Presigned = struct
  type connection = t
  type 'a io = 'a

  let record conn completion = record_observation conn completion

  let outcome_of_result = function
    | Ok _ -> Outcome.Ok
    | Error error -> Outcome.of_error error

  let with_artifact conn ~operation f =
    (* Credential lookup is a successful, safe child even when signing later
       rejects stale credentials or invalid options. *)
    record conn
      (Observability.complete_credential_resolution
         ~credentials:(credentials conn) ());
    match f () with
    | Ok _ as result ->
        record conn
          (Observability.complete_artifact_signing ~operation
             ~outcome:Outcome.Ok ());
        record conn
          (Observability.complete_artifact ~operation ~outcome:Outcome.Ok ());
        result
    | Error _ as result ->
        let outcome = outcome_of_result result in
        record conn
          (Observability.complete_artifact_signing ~operation ~outcome ());
        record conn (Observability.complete_artifact ~operation ~outcome ());
        result
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        record conn
          (Observability.complete_artifact_signing ~operation
             ~outcome:Outcome.Exception ());
        record conn
          (Observability.complete_artifact ~operation ~outcome:Outcome.Exception
             ());
        Printexc.raise_with_backtrace exn backtrace

  let get_object conn ~bucket ~key ?options () =
    with_artifact conn ~operation:Artifact_operation.Presign_get_object
      (fun () ->
        Presigned_model.get_object_with_endpoint_config
          ~region:(Runtime.Endpoint.region conn)
          ~credentials:(credentials conn) ~now:(now conn)
          ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
          ~bucket ~key ?options ())

  let put_object conn ~bucket ~key ?options () =
    with_artifact conn ~operation:Artifact_operation.Presign_put_object
      (fun () ->
        Presigned_model.put_object_with_endpoint_config
          ~region:(Runtime.Endpoint.region conn)
          ~credentials:(credentials conn) ~now:(now conn)
          ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
          ~bucket ~key ?options ())

  let head_object conn ~bucket ~key ?options () =
    with_artifact conn ~operation:Artifact_operation.Presign_head_object
      (fun () ->
        Presigned_model.head_object_with_endpoint_config
          ~region:(Runtime.Endpoint.region conn)
          ~credentials:(credentials conn) ~now:(now conn)
          ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
          ~bucket ~key ?options ())

  let delete_object conn ~bucket ~key ?options () =
    with_artifact conn ~operation:Artifact_operation.Presign_delete_object
      (fun () ->
        Presigned_model.delete_object_with_endpoint_config
          ~region:(Runtime.Endpoint.region conn)
          ~credentials:(credentials conn) ~now:(now conn)
          ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
          ~bucket ~key ?options ())

  let upload_part conn ~upload ~part_number ?options () =
    with_artifact conn ~operation:Artifact_operation.Presign_upload_part
      (fun () ->
        Presigned_model.upload_part_with_endpoint_config
          ~region:(Runtime.Endpoint.region conn)
          ~credentials:(credentials conn) ~now:(now conn)
          ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
          ~upload ~part_number ?options ())
end
