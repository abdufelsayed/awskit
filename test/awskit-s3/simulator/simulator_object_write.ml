open Core
open Headers
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
open Simulator_object_body

let validate_opt f = function None -> Ok () | Some value -> f value

let put conn ~bucket ~key ?options ~body () =
  let options = Option.value ~default:Put_object.default_options options in
  match validate_bucket_key bucket key with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok bucket_state -> (
          match validate_metadata options.metadata with
          | Error error -> Error error
          | Ok () -> (
              match validate_tags options.tags with
              | Error error -> Error error
              | Ok () -> (
                  match
                    validate_opt validate_checksum_value options.checksum
                  with
                  | Error error -> Error error
                  | Ok () -> (
                      match
                        operation_fault conn `Put_object bucket (Some key)
                      with
                      | Some error -> Error error
                      | None -> (
                          match
                            ensure_write_preconditions
                              (current_object bucket_state key)
                              options.preconditions
                          with
                          | Error error -> Error error
                          | Ok () -> (
                              match request_body_result body with
                              | Error error -> Error error
                              | Ok body ->
                                  let etag = etag body in
                                  let checksum =
                                    checksum_for_value options.checksum
                                  in
                                  let obj =
                                    {
                                      body;
                                      etag;
                                      version_id = None;
                                      content_type = options.content_type;
                                      metadata = options.metadata;
                                      storage_class = options.storage_class;
                                      tags = options.tags;
                                      checksum;
                                      last_modified = now conn;
                                    }
                                  in
                                  let obj =
                                    store_object conn bucket_state key obj
                                  in
                                  Ok
                                    {
                                      Put_object.etag = Some etag;
                                      version_id = obj.version_id;
                                      checksum;
                                      response =
                                        response 200
                                          ~headers:
                                            (("etag", Object.Etag.to_string etag)
                                            :: (version_headers obj.version_id
                                               @ checksum_response_headers
                                                   checksum));
                                    })))))))
