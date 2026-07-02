open Simulator_support
open Simulator_error
open Simulator_state
open Simulator_store
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key

let delete_result ?delete_marker ?version_id () =
  {
    Object.Delete.delete_marker;
    version_id;
    response =
      response 204
        ~headers:
          (version_headers version_id @ delete_marker_headers delete_marker);
  }

let delete_objects_error key ?version_id code message =
  { Object.Delete_objects.key; version_id; code; message = Some message }

let validate_delete_objects_count objects =
  let count = List.length objects in
  if count = 0 then
    invalid ~field:"objects"
      "delete objects request must contain at least one object"
  else if count > Object.Delete_objects.max_objects then
    invalid ~field:"objects"
      "delete objects request must contain at most %d objects"
      Object.Delete_objects.max_objects
  else Ok ()

let delete_objects_conditions_match object_ = function
  | Some (Stored_object obj) ->
      let etag_matches =
        match object_.Object.Delete_objects.etag with
        | None -> true
        | Some etag -> Object.Etag.equal obj.etag etag
      in
      etag_matches
  | None | Some (Stored_delete_marker _) -> Option.is_none object_.etag

let delete conn ~bucket ~key ?options () =
  let options = Option.value ~default:Object.Delete.default_options options in
  let return_error error =
    Error (with_operation `Delete_object ~bucket ~key error)
  in
  match validate_bucket_key bucket key with
  | Error error -> return_error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> return_error error
      | Ok bucket_state -> (
          match operation_fault conn `Delete_object bucket (Some key) with
          | Some error -> return_error error
          | None -> (
              match options.version_id with
              | Some version_id -> (
                  match delete_version bucket_state key version_id with
                  | None
                    when delete_preconditions_are_empty options.preconditions ->
                      Ok (delete_result ~version_id ())
                  | None -> return_error (precondition_failed ())
                  | Some (Stored_delete_marker _) ->
                      if delete_preconditions_are_empty options.preconditions
                      then Ok (delete_result ~delete_marker:true ~version_id ())
                      else return_error (precondition_failed ())
                  | Some (Stored_object obj) -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> return_error error
                      | Ok () -> Ok (delete_result ~version_id ())))
              | None when versioning_keeps_history bucket_state -> (
                  match current_object bucket_state key with
                  | None
                    when delete_preconditions_are_empty options.preconditions ->
                      let marker = store_delete_marker conn bucket_state key in
                      Ok
                        (delete_result ~delete_marker:true
                           ~version_id:marker.version_id ())
                  | None -> return_error (precondition_failed ())
                  | Some obj -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> return_error error
                      | Ok () ->
                          let marker =
                            store_delete_marker conn bucket_state key
                          in
                          Ok
                            (delete_result ~delete_marker:true
                               ~version_id:marker.version_id ())))
              | None -> (
                  match current_object bucket_state key with
                  | None
                    when delete_preconditions_are_empty options.preconditions ->
                      Hashtbl.remove bucket_state.objects key;
                      Ok (delete_result ())
                  | None -> return_error (precondition_failed ())
                  | Some obj -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> return_error error
                      | Ok () ->
                          Hashtbl.remove bucket_state.objects key;
                          Ok (delete_result ()))))))

let delete_objects conn ~bucket ~objects ?options:_ () =
  let key_string key = Object_key.to_string key in
  let return_error error =
    Error (with_operation `Delete_objects ~bucket error)
  in
  match validate_delete_objects_count objects with
  | Error error -> return_error error
  | Ok () -> (
      match validate_bucket bucket with
      | Error error -> return_error error
      | Ok () -> (
          match require_bucket conn bucket with
          | Error error -> return_error error
          | Ok bucket_state -> (
              match operation_fault conn `Delete_objects bucket None with
              | Some error -> return_error error
              | None ->
                  let deleted, errors =
                    List.fold_right
                      (fun (object_ : Object.Delete_objects.object_)
                           (deleted, errors) ->
                        let key = key_string object_.key in
                        let target =
                          match object_.version_id with
                          | Some version_id ->
                              find_version bucket_state key version_id
                          | None -> Hashtbl.find_opt bucket_state.objects key
                        in
                        if not (delete_objects_conditions_match object_ target)
                        then
                          ( deleted,
                            delete_objects_error object_.key
                              ?version_id:object_.version_id
                              "PreconditionFailed"
                              "delete preconditions did not match"
                            :: errors )
                        else
                          let delete_marker, version_id =
                            match object_.version_id with
                            | Some version_id -> (
                                match
                                  delete_version bucket_state key version_id
                                with
                                | Some (Stored_delete_marker _) ->
                                    (Some true, Some version_id)
                                | Some (Stored_object _) | None ->
                                    (None, Some version_id))
                            | None when versioning_keeps_history bucket_state ->
                                let marker =
                                  store_delete_marker conn bucket_state key
                                in
                                (Some true, Some marker.version_id)
                            | None ->
                                Hashtbl.remove bucket_state.objects key;
                                (None, None)
                          in
                          ( {
                              Object.Delete_objects.key = object_.key;
                              version_id;
                              delete_marker;
                              delete_marker_version_id =
                                (match delete_marker with
                                | Some true -> version_id
                                | Some false | None -> None);
                            }
                            :: deleted,
                            errors ))
                      objects ([], [])
                  in
                  Ok
                    {
                      Object.Delete_objects.deleted;
                      errors;
                      response = response 200;
                    })))
