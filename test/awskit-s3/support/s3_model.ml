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
  |> List.map (fun (key, object_) -> (key, object_.body))

let keys model = model |> objects_as_strings |> List.map fst

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal prefix (String.sub value 0 prefix_len)

let keys_with_prefix prefix model =
  model |> keys |> List.filter (is_prefix ~prefix)

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

let object_versions model =
  listed_versions model |> List.filter (fun version -> version.kind = `Object)

let delete_markers model =
  listed_versions model
  |> List.filter (fun version -> version.kind = `Delete_marker)

let apply command model =
  match command with
  | S3_command.Put_string (key, body, tags) -> put key body tags model
  | Put_string_metadata (key, body, tags, metadata) ->
      put_with_metadata key body tags metadata model
  | Get_string _ | Find_string _ | Head_object _ | Exists_object _ | List_keys
  | List_prefix _ | Get_object_tags _ | Get_bucket_tags | Get_versioning ->
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
