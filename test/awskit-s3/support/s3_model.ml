module String_map = Map.Make (String)

type object_ = {
  body : string;
  tags : S3_command.tag_model;
  has_version_id : bool;
}

type t = {
  objects : object_ String_map.t;
  bucket_tags : S3_command.tag_model;
  versioning : Awskit_s3.Bucket.Versioning.Status.t option;
}

let empty = { objects = String_map.empty; bucket_tags = []; versioning = None }

let versioning_keeps_history model =
  match model.versioning with
  | Some Awskit_s3.Bucket.Versioning.Status.Enabled | Some Suspended -> true
  | Some (Unknown _) | None -> false

let find key model = String_map.find_opt key model.objects

let put key body tags model =
  {
    model with
    objects =
      String_map.add key
        { body; tags; has_version_id = versioning_keeps_history model }
        model.objects;
  }

let delete key model =
  { model with objects = String_map.remove key model.objects }

let copy ~source_key ~destination_key model =
  match find source_key model with
  | None -> model
  | Some source ->
      {
        model with
        objects =
          String_map.add destination_key
            { source with has_version_id = versioning_keeps_history model }
            model.objects;
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
let put_versioning status model = { model with versioning = Some status }

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

let apply command model =
  match command with
  | S3_command.Put_string (key, body, tags) -> put key body tags model
  | Get_string _ | Find_string _ | Head_object _ | Exists_object _ | List_keys
  | List_prefix _ | Get_object_tags _ | Get_bucket_tags | Get_versioning ->
      model
  | Delete_object key -> delete key model
  | Copy_object (source_key, destination_key) ->
      copy ~source_key ~destination_key model
  | Put_object_tags (key, tags) -> put_object_tags key tags model
  | Delete_object_tags key -> delete_object_tags key model
  | Put_bucket_tags tags -> put_bucket_tags tags model
  | Delete_bucket_tags -> delete_bucket_tags model
  | Put_versioning status -> put_versioning status model
