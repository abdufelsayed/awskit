open Core
open Sim_error
open Sim_state
open Sim_store

let delete_result ?delete_marker ?version_id () =
  {
    Delete_object.delete_marker;
    version_id;
    response =
      response 204
        ~headers:
          (version_headers version_id @ delete_marker_headers delete_marker);
  }

let delete_objects_error key code message =
  { Delete_objects.key; code; message = Some message }

let delete_objects_conditions_match object_ = function
  | Some (Stored_object obj) ->
      let etag_matches =
        match object_.Delete_objects.etag with
        | None -> true
        | Some etag -> Object.Etag.equal obj.etag etag
      in
      etag_matches
  | None | Some (Stored_delete_marker _) -> Option.is_none object_.etag

let delete conn ~bucket ~key ?options () =
  let options = Option.value ~default:Delete_object.default_options options in
  match validate_bucket_key bucket key with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok bucket_state -> (
          match operation_fault conn `Delete_object bucket (Some key) with
          | Some error -> Error error
          | None -> (
              match options.version_id with
              | Some version_id -> (
                  match delete_version bucket_state key version_id with
                  | None
                    when delete_preconditions_are_empty options.preconditions ->
                      Ok (delete_result ~version_id ())
                  | None -> Error (precondition_failed ())
                  | Some (Stored_delete_marker _) ->
                      if delete_preconditions_are_empty options.preconditions
                      then Ok (delete_result ~delete_marker:true ~version_id ())
                      else Error (precondition_failed ())
                  | Some (Stored_object obj) -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> Error error
                      | Ok () -> Ok (delete_result ~version_id ())))
              | None when versioning_keeps_history bucket_state -> (
                  match current_object bucket_state key with
                  | None
                    when delete_preconditions_are_empty options.preconditions ->
                      let marker = store_delete_marker conn bucket_state key in
                      Ok
                        (delete_result ~delete_marker:true
                           ~version_id:marker.version_id ())
                  | None -> Error (precondition_failed ())
                  | Some obj -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> Error error
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
                  | None -> Error (precondition_failed ())
                  | Some obj -> (
                      match
                        ensure_delete_preconditions obj options.preconditions
                      with
                      | Error error -> Error error
                      | Ok () ->
                          Hashtbl.remove bucket_state.objects key;
                          Ok (delete_result ()))))))

let delete_objects conn ~bucket ~objects ?options:_ () =
  match validate_bucket bucket with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok bucket_state -> (
          match operation_fault conn `Delete_objects bucket None with
          | Some error -> Error error
          | None ->
              let deleted, errors =
                List.fold_right
                  (fun (object_ : Delete_objects.object_) (deleted, errors) ->
                    let target =
                      match object_.version_id with
                      | Some version_id ->
                          find_version bucket_state object_.key version_id
                      | None ->
                          Hashtbl.find_opt bucket_state.objects object_.key
                    in
                    if not (delete_objects_conditions_match object_ target) then
                      ( deleted,
                        delete_objects_error object_.key "PreconditionFailed"
                          "delete preconditions did not match"
                        :: errors )
                    else
                      let delete_marker, version_id =
                        match object_.version_id with
                        | Some version_id -> (
                            match
                              delete_version bucket_state object_.key version_id
                            with
                            | Some (Stored_delete_marker _) ->
                                (Some true, Some version_id)
                            | Some (Stored_object _) | None ->
                                (None, Some version_id))
                        | None when versioning_keeps_history bucket_state ->
                            let marker =
                              store_delete_marker conn bucket_state object_.key
                            in
                            (Some true, Some marker.version_id)
                        | None ->
                            Hashtbl.remove bucket_state.objects object_.key;
                            (None, None)
                      in
                      ( {
                          Delete_objects.key = object_.key;
                          version_id;
                          delete_marker;
                        }
                        :: deleted,
                        errors ))
                  objects ([], [])
              in
              Ok { Delete_objects.deleted; errors; response = response 200 }))
