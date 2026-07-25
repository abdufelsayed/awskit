open Simulator_support
open Simulator_headers
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
module Object = Awskit_s3.Object
module Operation = Awskit_s3.Operation

let validate_opt f = function None -> Ok () | Some value -> f value

let copy conn ~source_bucket ~source_key ~destination_bucket ~destination_key
    ?options () =
  let options = Option.value ~default:Object.Copy.default_options options in
  let source_error error =
    Error
      (with_operation Operation.Copy_object ~bucket:source_bucket
         ~key:source_key error)
  in
  let return_error error =
    Error
      (with_operation Operation.Copy_object ~bucket:destination_bucket
         ~key:destination_key error)
  in
  match validate_bucket_key source_bucket source_key with
  | Error error -> source_error error
  | Ok () -> (
      match validate_bucket_key destination_bucket destination_key with
      | Error error -> return_error error
      | Ok () -> (
          match
            require_object_version conn source_bucket source_key
              options.source_version_id
          with
          | Error error -> source_error error
          | Ok src -> (
              match
                let* () =
                  validate_opt validate_storage_class options.storage_class
                in
                let* () =
                  validate_opt validate_checksum_algorithm
                    options.checksum_algorithm
                in
                validate_opt validate_supported_algorithm
                  options.checksum_algorithm
              with
              | Error error -> return_error error
              | Ok () -> (
                  match require_bucket conn destination_bucket with
                  | Error error -> return_error error
                  | Ok destination_bucket_state -> (
                      match
                        operation_fault conn Operation.Copy_object
                          destination_bucket (Some destination_key)
                      with
                      | Some error -> return_error error
                      | None -> (
                          match
                            ensure_copy_source_preconditions src
                              options.source_preconditions
                          with
                          | Error error -> source_error error
                          | Ok () -> (
                              match
                                checksum_for_algorithm ~body:src.body
                                  options.checksum_algorithm
                              with
                              | Error error -> return_error error
                              | Ok computed_checksum ->
                                  let metadata =
                                    match options.metadata_directive with
                                    | Some (`Replace metadata) -> metadata
                                    | _ -> src.metadata
                                  in
                                  let checksum =
                                    match computed_checksum.values with
                                    | [] -> src.checksum
                                    | _ -> computed_checksum
                                  in
                                  let obj =
                                    {
                                      body = src.body;
                                      etag = src.etag;
                                      version_id = None;
                                      content_type = src.content_type;
                                      metadata;
                                      storage_class =
                                        (match options.storage_class with
                                        | Some sc -> Some sc
                                        | None -> src.storage_class);
                                      tags = src.tags;
                                      checksum;
                                      last_modified = now conn;
                                    }
                                  in
                                  let obj =
                                    store_object conn destination_bucket_state
                                      destination_key obj
                                  in
                                  Ok
                                    {
                                      Object.Copy.etag = Some obj.etag;
                                      last_modified = Some obj.last_modified;
                                      version_id = obj.version_id;
                                      copy_source_version_id = src.version_id;
                                      response =
                                        response 200
                                          ~headers:
                                            (version_headers obj.version_id
                                            @ copy_source_version_headers
                                                src.version_id);
                                    })))))))
