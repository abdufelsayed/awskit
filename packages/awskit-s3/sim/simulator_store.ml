open Awskit_s3
open Simulator_support
open Simulator_state
open Simulator_error

let bucket_state store bucket = find_bucket store bucket

let require_bucket t bucket =
  match bucket_state (store t) bucket with
  | Some state -> Ok state
  | None -> Error (no_such_bucket ())

let require_object t bucket key =
  let* bucket = require_bucket t bucket in
  match Hashtbl.find_opt bucket.objects key with
  | Some (Stored_object object_) -> Ok object_
  | None -> Error (no_such_key ())
  | Some (Stored_delete_marker _) -> Error (no_such_key ())

let versioning_enabled (bucket : bucket_state) =
  match bucket.versioning with
  | Some Bucket.Versioning.Status.Enabled -> true
  | Some Suspended | None -> false

let versioning_suspended (bucket : bucket_state) =
  match bucket.versioning with
  | Some Bucket.Versioning.Status.Suspended -> true
  | Some Enabled | None -> false

let versioning_keeps_history bucket =
  versioning_enabled bucket || versioning_suspended bucket

let next_version_id t = allocate_version_id t
let null_version_id = Object.Version_id.of_string_exn "null"

let version_id_of_version = function
  | Stored_object obj -> obj.version_id
  | Stored_delete_marker marker -> Some marker.version_id

let prepend_version bucket key version =
  let versions =
    match Hashtbl.find_opt bucket.versions key with
    | None -> []
    | Some versions -> versions
  in
  Hashtbl.replace bucket.versions key (version :: versions)

let replace_null_version bucket key version =
  let versions =
    match Hashtbl.find_opt bucket.versions key with
    | None -> []
    | Some versions ->
        List.filter
          (fun existing ->
            match version_id_of_version existing with
            | Some version_id ->
                not (Object.Version_id.equal version_id null_version_id)
            | None -> true)
          versions
  in
  Hashtbl.replace bucket.versions key (version :: versions)

let store_object t bucket key (obj : stored_object) =
  let obj =
    if versioning_enabled bucket then
      { obj with version_id = Some (next_version_id t) }
    else if versioning_suspended bucket then
      { obj with version_id = Some null_version_id }
    else obj
  in
  let version = Stored_object obj in
  Hashtbl.replace bucket.objects key version;
  if versioning_suspended bucket then replace_null_version bucket key version
  else if Option.is_some obj.version_id then prepend_version bucket key version;
  obj

let find_version bucket key version_id =
  match Hashtbl.find_opt bucket.versions key with
  | None -> None
  | Some versions ->
      List.find_opt
        (fun version ->
          match version_id_of_version version with
          | Some candidate -> Object.Version_id.equal candidate version_id
          | None -> false)
        versions

let current_or_version bucket key version_id =
  match version_id with
  | None -> Hashtbl.find_opt bucket.objects key
  | Some version_id -> find_version bucket key version_id

let current_object bucket key =
  match Hashtbl.find_opt bucket.objects key with
  | Some (Stored_object obj) -> Some obj
  | None | Some (Stored_delete_marker _) -> None

let require_object_version t bucket key version_id =
  let* bucket = require_bucket t bucket in
  match current_or_version bucket key version_id with
  | Some (Stored_object object_) -> Ok object_
  | None -> Error (no_such_key ())
  | Some (Stored_delete_marker _) -> Error (no_such_key ())

let delete_version bucket key version_id =
  match Hashtbl.find_opt bucket.versions key with
  | None -> None
  | Some versions -> (
      let deleted = ref None in
      let remaining =
        List.filter
          (fun version ->
            match version_id_of_version version with
            | Some candidate when Object.Version_id.equal candidate version_id
              ->
                deleted := Some version;
                false
            | _ -> true)
          versions
      in
      match !deleted with
      | None -> None
      | Some deleted_version ->
          (match remaining with
          | [] ->
              Hashtbl.remove bucket.versions key;
              Hashtbl.remove bucket.objects key
          | latest :: _ ->
              Hashtbl.replace bucket.versions key remaining;
              Hashtbl.replace bucket.objects key latest);
          Some deleted_version)

let store_delete_marker t bucket key =
  let marker =
    {
      version_id =
        (if versioning_suspended bucket then null_version_id
         else next_version_id t);
      last_modified = now t;
    }
  in
  let version = Stored_delete_marker marker in
  Hashtbl.replace bucket.objects key version;
  if versioning_suspended bucket then replace_null_version bucket key version
  else prepend_version bucket key version;
  marker

let version_headers = function
  | None -> []
  | Some version_id ->
      [ ("x-amz-version-id", Object.Version_id.to_string version_id) ]

let copy_source_version_headers = function
  | None -> []
  | Some version_id ->
      [
        ("x-amz-copy-source-version-id", Object.Version_id.to_string version_id);
      ]

let delete_marker_headers = function
  | None | Some false -> []
  | Some true -> [ ("x-amz-delete-marker", "true") ]

let delete_marker_error ~current marker =
  let headers =
    version_headers (Some marker.version_id) @ delete_marker_headers (Some true)
  in
  if current then service ~headers ~status:404 ~code:"NoSuchKey" ()
  else method_not_allowed ~headers ()

let object_size (obj : stored_object) = Int64.of_int (String.length obj.body)

let etag_condition_matches (obj : stored_object) = function
  | Object.Etag_condition.Any -> true
  | Etag etag -> Object.Etag.equal obj.etag etag

let etag_condition_matches_opt obj condition =
  match obj with
  | None -> false
  | Some obj -> etag_condition_matches obj condition

let ensure_write_preconditions obj (p : Object.Preconditions.Write.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches_opt obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match (p.if_none_match, obj) with
      | Some Object.Etag_condition.Any, Some _ -> Error (precondition_failed ())
      | Some (Etag etag), Some obj when Object.Etag.equal obj.etag etag ->
          Error (precondition_failed ())
      | _ -> Ok ())

let time_leq left right = Ptime.compare left right <= 0
let time_gt left right = Ptime.compare left right > 0

let ensure_read_preconditions (obj : stored_object)
    (p : Object.Preconditions.Read.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match p.if_unmodified_since with
      | Some time when time_gt obj.last_modified time ->
          Error (precondition_failed ())
      | _ -> (
          match p.if_none_match with
          | Some condition when etag_condition_matches obj condition ->
              Error (not_modified ())
          | _ -> (
              match p.if_modified_since with
              | Some time when time_leq obj.last_modified time ->
                  Error (not_modified ())
              | _ -> Ok ())))

let ensure_delete_preconditions (obj : stored_object)
    (p : Object.Preconditions.Delete.t) =
  match p.if_match with
  | None -> Ok ()
  | Some condition ->
      if etag_condition_matches obj condition then Ok ()
      else Error (precondition_failed ())

let delete_preconditions_are_empty (p : Object.Preconditions.Delete.t) =
  Option.is_none p.if_match

let ensure_copy_source_preconditions (obj : stored_object)
    (p : Object.Preconditions.Copy_source.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match p.if_unmodified_since with
      | Some time when time_gt obj.last_modified time ->
          Error (precondition_failed ())
      | _ -> (
          match p.if_none_match with
          | Some condition when etag_condition_matches obj condition ->
              Error (precondition_failed ())
          | _ -> (
              match p.if_modified_since with
              | Some time when time_leq obj.last_modified time ->
                  Error (precondition_failed ())
              | _ -> Ok ())))

let upload_key upload_id = Multipart.Upload_id.to_string upload_id

let require_multipart_upload t ~bucket ~key ~upload_id =
  let* bucket_state = require_bucket t bucket in
  match
    Hashtbl.find_opt bucket_state.multipart_uploads (upload_key upload_id)
  with
  | Some upload
    when String.equal
           (Multipart.Upload.key upload.upload |> Object_key.to_string)
           key ->
      Ok (bucket_state, upload)
  | _ -> Error (no_such_upload ())

let next_upload_id t = allocate_upload_id t
