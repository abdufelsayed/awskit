module String_map = Map.Make (String)

type version_id_kind = No_version | Generated_version | Null_version

type object_ = {
  body : string;
  tags : S3_command.tag_model;
  metadata : S3_command.metadata_model;
  has_version_id : bool;
}

type version_entry =
  | Object_version of object_ * version_id_kind
  | Delete_marker of version_id_kind

type t = {
  objects : object_ String_map.t;
  versions : version_entry list String_map.t;
  bucket_tags : S3_command.tag_model;
  versioning : Awskit_s3.Bucket.Versioning.Status.t option;
}

type listed_version = {
  key : string;
  kind : [ `Object | `Delete_marker ];
  has_version_id : bool;
  is_latest : bool;
  size : int64 option;
}

type content_range_summary = {
  start : int64;
  finish : int64;
  complete_length : int64 option;
}

type object_read_summary = {
  read_body : string;
  read_metadata : S3_command.metadata_model;
  read_content_length : int64 option;
  read_content_range : content_range_summary option;
  read_has_version_id : bool;
}

type object_metadata_summary = {
  metadata_content_length : int64 option;
  metadata_entries : S3_command.metadata_model;
  metadata_has_version_id : bool;
}

type object_write_summary = { write_has_version_id : bool }

type object_delete_summary = {
  delete_marker : bool;
  delete_has_version_id : bool;
}

type object_copy_summary = {
  copy_source_has_version_id : bool;
  copy_destination_has_version_id : bool;
}

type operation_result =
  | Response_ok
  | Put_ok of object_write_summary
  | Get_ok of object_read_summary
  | Find_ok of object_read_summary option
  | Head_ok of object_metadata_summary
  | Exists_ok of bool
  | Delete_ok of object_delete_summary
  | List_keys_ok of string list
  | List_versions_ok of listed_version list
  | Copy_ok of object_copy_summary
  | Object_tags_ok of S3_command.tag_model
  | Bucket_tags_ok of S3_command.tag_model
  | Versioning_ok of Awskit_s3.Bucket.Versioning.Status.t option
  | Not_found
  | Invalid_range

let operation_result_kind = function
  | Response_ok -> "response-ok"
  | Put_ok _ -> "put-ok"
  | Get_ok _ -> "get-ok"
  | Find_ok _ -> "find-ok"
  | Head_ok _ -> "head-ok"
  | Exists_ok _ -> "exists-ok"
  | Delete_ok _ -> "delete-ok"
  | List_keys_ok _ -> "list-keys-ok"
  | List_versions_ok _ -> "list-versions-ok"
  | Copy_ok _ -> "copy-ok"
  | Object_tags_ok _ -> "object-tags-ok"
  | Bucket_tags_ok _ -> "bucket-tags-ok"
  | Versioning_ok _ -> "versioning-ok"
  | Not_found -> "not-found"
  | Invalid_range -> "invalid-range"

let empty =
  {
    objects = String_map.empty;
    versions = String_map.empty;
    bucket_tags = [];
    versioning = None;
  }

let versioning_keeps_history model =
  match model.versioning with
  | Some Awskit_s3.Bucket.Versioning.Status.Enabled | Some Suspended -> true
  | Some (Unknown _) | None -> false

let status_keeps_history = function
  | Awskit_s3.Bucket.Versioning.Status.Enabled | Suspended -> true
  | Unknown _ -> false

let versioning_enabled model =
  match model.versioning with
  | Some Awskit_s3.Bucket.Versioning.Status.Enabled -> true
  | Some Suspended | Some (Unknown _) | None -> false

let versioning_suspended model =
  match model.versioning with
  | Some Awskit_s3.Bucket.Versioning.Status.Suspended -> true
  | Some Enabled | Some (Unknown _) | None -> false

let version_id_kind_for_write model =
  if versioning_enabled model then Generated_version
  else if versioning_suspended model then Null_version
  else No_version

let has_version_id = function
  | No_version -> false
  | Generated_version | Null_version -> true

let find key model = String_map.find_opt key model.objects
let content_length body = Int64.of_int (String.length body)

let object_read_summary ?content_range ?body (object_ : object_) =
  let body = Option.value ~default:object_.body body in
  {
    read_body = body;
    read_metadata = object_.metadata;
    read_content_length = Some (content_length body);
    read_content_range = content_range;
    read_has_version_id = object_.has_version_id;
  }

let object_metadata_summary (object_ : object_) =
  {
    metadata_content_length = Some (content_length object_.body);
    metadata_entries = object_.metadata;
    metadata_has_version_id = object_.has_version_id;
  }

let int64_succ value = Int64.add value 1L

let substring_by_int64_bounds body ~start ~finish =
  let start_int = Int64.to_int start in
  let length = Int64.to_int (int64_succ (Int64.sub finish start)) in
  String.sub body start_int length

let ranged_read_summary (object_ : object_) range =
  let length = String.length object_.body in
  let length64 = Int64.of_int length in
  let bounds =
    match Awskit_s3.Range.view range with
    | Awskit_s3.Range.Bytes (start, finish) ->
        if Int64.compare start length64 >= 0 then None
        else Some (start, Int64.min finish (Int64.sub length64 1L))
    | Awskit_s3.Range.From start ->
        if Int64.compare start length64 >= 0 then None
        else Some (start, Int64.sub length64 1L)
    | Awskit_s3.Range.Suffix suffix ->
        if length = 0 then None
        else
          let start =
            if Int64.compare suffix length64 >= 0 then 0L
            else Int64.sub length64 suffix
          in
          Some (start, Int64.sub length64 1L)
  in
  match bounds with
  | None -> Invalid_range
  | Some (start, finish) ->
      let body = substring_by_int64_bounds object_.body ~start ~finish in
      Get_ok
        (object_read_summary object_ ~body
           ~content_range:{ start; finish; complete_length = Some length64 })

let replace_null_version key entry versions =
  let entries =
    match String_map.find_opt key versions with
    | None -> []
    | Some entries ->
        List.filter
          (function
            | Object_version (_, Null_version) | Delete_marker Null_version ->
                false
            | Object_version (_, (No_version | Generated_version))
            | Delete_marker (No_version | Generated_version) ->
                true)
          entries
  in
  String_map.add key (entry :: entries) versions

let prepend_version key entry versions =
  let entries = Option.value ~default:[] (String_map.find_opt key versions) in
  String_map.add key (entry :: entries) versions

let store_version key entry model =
  match entry with
  | Object_version (_, No_version) | Delete_marker No_version -> model.versions
  | Object_version (_, Null_version) | Delete_marker Null_version ->
      replace_null_version key entry model.versions
  | Object_version (_, Generated_version) | Delete_marker Generated_version ->
      prepend_version key entry model.versions

let put_with_metadata key body tags metadata model =
  let version_id_kind = version_id_kind_for_write model in
  let object_ =
    { body; tags; metadata; has_version_id = has_version_id version_id_kind }
  in
  {
    model with
    objects = String_map.add key object_ model.objects;
    versions =
      store_version key (Object_version (object_, version_id_kind)) model;
  }

let put key body tags model = put_with_metadata key body tags [] model

let delete key model =
  if versioning_keeps_history model then
    let version_id_kind = version_id_kind_for_write model in
    {
      model with
      objects = String_map.remove key model.objects;
      versions = store_version key (Delete_marker version_id_kind) model;
    }
  else { model with objects = String_map.remove key model.objects }

let copy ?replace_metadata ~source_key ~destination_key model =
  match find source_key model with
  | None -> model
  | Some source ->
      let version_id_kind = version_id_kind_for_write model in
      let object_ =
        {
          source with
          metadata =
            (match replace_metadata with
            | None -> source.metadata
            | Some metadata -> metadata);
          has_version_id = has_version_id version_id_kind;
        }
      in
      {
        model with
        objects = String_map.add destination_key object_ model.objects;
        versions =
          store_version destination_key
            (Object_version (object_, version_id_kind))
            model;
      }

let put_object_tags key tags model =
  match find key model with
  | None -> model
  | Some object_ ->
      {
        model with
        objects = String_map.add key { object_ with tags } model.objects;
      }

let delete_object_tags key model = put_object_tags key [] model
let put_bucket_tags tags model = { model with bucket_tags = tags }
let delete_bucket_tags model = { model with bucket_tags = [] }

let promote_current_objects_to_null_versions model =
  let objects, versions =
    String_map.fold
      (fun key (object_ : object_) (objects, versions) ->
        let object_ = { object_ with has_version_id = true } in
        let versions =
          replace_null_version key
            (Object_version (object_, Null_version))
            versions
        in
        (String_map.add key object_ objects, versions))
      model.objects
      (String_map.empty, model.versions)
  in
  { model with objects; versions }

let put_versioning status model =
  let was_unversioned = not (versioning_keeps_history model) in
  let model = { model with versioning = Some status } in
  if was_unversioned && status_keeps_history status then
    promote_current_objects_to_null_versions model
  else model

let objects_as_strings model =
  model.objects
  |> String_map.bindings
  |> List.map (fun (key, (object_ : object_)) -> (key, object_.body))

let keys model = model |> objects_as_strings |> List.map fst

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal prefix (String.sub value 0 prefix_len)

let keys_with_prefix prefix model =
  model |> keys |> List.filter (is_prefix ~prefix)

let keys_for_page ?prefix model =
  match prefix with
  | None -> keys model
  | Some prefix -> keys_with_prefix prefix model

let take n values =
  let rec loop n values acc =
    match (n, values) with
    | n, _ when n <= 0 -> List.rev acc
    | _, [] -> List.rev acc
    | n, value :: rest -> loop (n - 1) rest (value :: acc)
  in
  loop n values []

let list_keys_page ?prefix ~max_keys model =
  let keys = keys_for_page ?prefix model in
  take max_keys keys

let list_keys_page_is_truncated ?prefix ~max_keys model =
  List.length (keys_for_page ?prefix model) > max_keys

let version_id_kind_has_id = function
  | No_version -> false
  | Generated_version | Null_version -> true

let listed_entry ~key ~is_latest = function
  | Object_version (object_, version_id_kind) ->
      {
        key;
        kind = `Object;
        has_version_id = version_id_kind_has_id version_id_kind;
        is_latest;
        size = Some (Int64.of_int (String.length object_.body));
      }
  | Delete_marker version_id_kind ->
      {
        key;
        kind = `Delete_marker;
        has_version_id = version_id_kind_has_id version_id_kind;
        is_latest;
        size = None;
      }

let listed_versions model =
  let version_keys = model.versions |> String_map.bindings |> List.map fst in
  let object_keys = model.objects |> String_map.bindings |> List.map fst in
  version_keys @ object_keys
  |> List.sort_uniq String.compare
  |> List.concat_map (fun key ->
      match String_map.find_opt key model.versions with
      | Some entries ->
          List.mapi
            (fun index entry ->
              listed_entry ~key ~is_latest:(Int.equal index 0) entry)
            entries
      | None -> (
          match String_map.find_opt key model.objects with
          | None -> []
          | Some object_ ->
              [
                listed_entry ~key ~is_latest:true
                  (Object_version (object_, No_version));
              ]))

let list_versions_page ~max_keys model = take max_keys (listed_versions model)

let list_versions_page_is_truncated ~max_keys model =
  List.length (listed_versions model) > max_keys

let object_versions model =
  listed_versions model |> List.filter (fun version -> version.kind = `Object)

let delete_markers model =
  listed_versions model
  |> List.filter (fun version -> version.kind = `Delete_marker)

let expected_put_result model =
  Put_ok { write_has_version_id = versioning_keeps_history model }

let expected_delete_result model =
  Delete_ok
    {
      delete_marker = versioning_keeps_history model;
      delete_has_version_id = versioning_keeps_history model;
    }

let expected_copy_result model source =
  match source with
  | None -> Not_found
  | Some (object_ : object_) ->
      Copy_ok
        {
          copy_source_has_version_id = object_.has_version_id;
          copy_destination_has_version_id = versioning_keeps_history model;
        }

let expected_get_result model key =
  match find key model with
  | None -> Not_found
  | Some object_ -> Get_ok (object_read_summary object_)

let expected_find_result model key =
  Find_ok (Option.map object_read_summary (find key model))

let expected_range_get_result model key range =
  match find key model with
  | None -> Not_found
  | Some object_ -> ranged_read_summary object_ range

let expected_head_result model key =
  match find key model with
  | None -> Not_found
  | Some object_ -> Head_ok (object_metadata_summary object_)

let expected_object_tags_result model key =
  match find key model with
  | None -> Not_found
  | Some object_ -> Object_tags_ok object_.tags

let expected_object_tags_mutation_result model key =
  match find key model with None -> Not_found | Some _ -> Response_ok

let expected_result command model =
  match command with
  | S3_command.Put_string _ | Put_string_metadata _ -> expected_put_result model
  | Get_string key -> expected_get_result model key
  | Get_range (key, range) -> expected_range_get_result model key range
  | Find_string key -> expected_find_result model key
  | Head_object key -> expected_head_result model key
  | Exists_object key -> Exists_ok (Option.is_some (find key model))
  | Delete_object _ -> expected_delete_result model
  | List_keys -> List_keys_ok (keys model)
  | List_prefix prefix -> List_keys_ok (keys_with_prefix prefix model)
  | List_keys_page { prefix; max_keys } ->
      List_keys_ok (list_keys_page ?prefix ~max_keys model)
  | List_versions_page { max_keys } ->
      List_versions_ok (list_versions_page ~max_keys model)
  | Copy_object (source_key, _) ->
      expected_copy_result model (find source_key model)
  | Copy_object_metadata (source_key, _, _) ->
      expected_copy_result model (find source_key model)
  | Put_object_tags (key, _) | Delete_object_tags key ->
      expected_object_tags_mutation_result model key
  | Get_object_tags key -> expected_object_tags_result model key
  | Put_bucket_tags _ | Delete_bucket_tags | Put_versioning _ -> Response_ok
  | Get_bucket_tags -> Bucket_tags_ok model.bucket_tags
  | Get_versioning -> Versioning_ok model.versioning

let apply command model =
  match command with
  | S3_command.Put_string (key, body, tags) -> put key body tags model
  | Put_string_metadata (key, body, tags, metadata) ->
      put_with_metadata key body tags metadata model
  | Get_string _ | Get_range _ | Find_string _ | Head_object _ | Exists_object _
  | List_keys | List_prefix _ | List_keys_page _ | List_versions_page _
  | Get_object_tags _ | Get_bucket_tags | Get_versioning ->
      model
  | Delete_object key -> delete key model
  | Copy_object (source_key, destination_key) ->
      copy ~source_key ~destination_key model
  | Copy_object_metadata (source_key, destination_key, metadata) ->
      let replace_metadata =
        match metadata with
        | S3_command.Copy_source_metadata -> None
        | Replace_metadata metadata -> Some metadata
      in
      copy ?replace_metadata ~source_key ~destination_key model
  | Put_object_tags (key, tags) -> put_object_tags key tags model
  | Delete_object_tags key -> delete_object_tags key model
  | Put_bucket_tags tags -> put_bucket_tags tags model
  | Delete_bucket_tags -> delete_bucket_tags model
  | Put_versioning status -> put_versioning status model
