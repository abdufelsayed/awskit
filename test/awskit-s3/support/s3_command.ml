type tag_model = (string * string) list
type metadata_model = (string * string) list
type copy_metadata = Copy_source_metadata | Replace_metadata of metadata_model

type t =
  | Put_string of string * string * tag_model
  | Put_string_metadata of string * string * tag_model * metadata_model
  | Get_string of string
  | Find_string of string
  | Head_object of string
  | Exists_object of string
  | Delete_object of string
  | List_keys
  | List_prefix of string
  | Copy_object of string * string
  | Copy_object_metadata of string * string * copy_metadata
  | Put_object_tags of string * tag_model
  | Get_object_tags of string
  | Delete_object_tags of string
  | Put_bucket_tags of tag_model
  | Get_bucket_tags
  | Delete_bucket_tags
  | Put_versioning of Awskit_s3.Bucket.Versioning.Status.t
  | Get_versioning

let key_domain =
  [ "a.txt"; "b.txt"; "logs/a.txt"; "logs/b.txt"; "photos/2026.jpg" ]

let prefix_domain = [ "logs/"; "photos/"; "missing/"; "a" ]

let tag_sets_domain =
  [
    [];
    [ ("env", "dev") ];
    [ ("env", "prod") ];
    [ ("owner", "sdk") ];
    [ ("team", "storage") ];
    [ ("env", "dev"); ("owner", "sdk") ];
    [ ("team", "storage"); ("mode", "pbt") ];
    [ ("path/key", "x@y") ];
  ]

let metadata_sets_domain =
  [
    [];
    [ ("author", "awskit") ];
    [ ("trace-id", "sim-1") ];
    [ ("purpose", "stateful-pbt"); ("owner", "sdk") ];
    [ ("cache-key", "logs/a"); ("review", "broad") ];
  ]

let versioning_status_domain =
  [
    Awskit_s3.Bucket.Versioning.Status.Enabled;
    Awskit_s3.Bucket.Versioning.Status.Suspended;
  ]

let tags_to_string tags =
  tags
  |> List.map (fun (key, value) -> Printf.sprintf "%S=%S" key value)
  |> String.concat ";"
  |> Printf.sprintf "[%s]"

let metadata_to_string metadata =
  metadata
  |> List.map (fun (key, value) -> Printf.sprintf "%S=%S" key value)
  |> String.concat ";"
  |> Printf.sprintf "[%s]"

let copy_metadata_to_string = function
  | Copy_source_metadata -> "copy-source-metadata"
  | Replace_metadata metadata ->
      Printf.sprintf "replace-metadata metadata=%s"
        (metadata_to_string metadata)

let versioning_status_to_string = Awskit_s3.Bucket.Versioning.Status.to_string

let to_string = function
  | Put_string (key, body, tags) ->
      Printf.sprintf "put-string key=%S body=%S tags=%s" key body
        (tags_to_string tags)
  | Put_string_metadata (key, body, tags, metadata) ->
      Printf.sprintf "put-string-metadata key=%S body=%S tags=%s metadata=%s"
        key body (tags_to_string tags)
        (metadata_to_string metadata)
  | Get_string key -> Printf.sprintf "get-string key=%S" key
  | Find_string key -> Printf.sprintf "find-string key=%S" key
  | Head_object key -> Printf.sprintf "head-object key=%S" key
  | Exists_object key -> Printf.sprintf "exists-object key=%S" key
  | Delete_object key -> Printf.sprintf "delete-object key=%S" key
  | List_keys -> "list-keys"
  | List_prefix prefix -> Printf.sprintf "list-prefix prefix=%S" prefix
  | Copy_object (source_key, destination_key) ->
      Printf.sprintf "copy-object source=%S destination=%S" source_key
        destination_key
  | Copy_object_metadata (source_key, destination_key, metadata) ->
      Printf.sprintf "copy-object-metadata source=%S destination=%S %s"
        source_key destination_key
        (copy_metadata_to_string metadata)
  | Put_object_tags (key, tags) ->
      Printf.sprintf "put-object-tags key=%S tags=%s" key (tags_to_string tags)
  | Get_object_tags key -> Printf.sprintf "get-object-tags key=%S" key
  | Delete_object_tags key -> Printf.sprintf "delete-object-tags key=%S" key
  | Put_bucket_tags tags ->
      Printf.sprintf "put-bucket-tags tags=%s" (tags_to_string tags)
  | Get_bucket_tags -> "get-bucket-tags"
  | Delete_bucket_tags -> "delete-bucket-tags"
  | Put_versioning status ->
      Printf.sprintf "put-versioning status=%s"
        (versioning_status_to_string status)
  | Get_versioning -> "get-versioning"

let transcript commands =
  commands
  |> List.mapi (fun index command ->
      Printf.sprintf "%02d. %s" (index + 1) (to_string command))
  |> String.concat "\n"

let gen_key = QCheck.Gen.oneof_list key_domain
let gen_prefix = QCheck.Gen.oneof_list prefix_domain
let gen_tags = QCheck.Gen.oneof_list tag_sets_domain
let gen_metadata = QCheck.Gen.oneof_list metadata_sets_domain
let gen_versioning_status = QCheck.Gen.oneof_list versioning_status_domain

let gen_copy_metadata =
  QCheck.Gen.(
    oneof
      [
        return Copy_source_metadata;
        map (fun metadata -> Replace_metadata metadata) gen_metadata;
      ])

let gen_body =
  QCheck.Gen.(
    string_size
      ~gen:
        (oneof_weighted
           [ (8, char_range 'a' 'z'); (1, return ' '); (1, numeral) ])
      (int_range 0 12))

let generator =
  let open QCheck.Gen in
  oneof_weighted
    [
      ( 4,
        map3
          (fun key body tags -> Put_string (key, body, tags))
          gen_key gen_body gen_tags );
      ( 3,
        map4
          (fun key body tags metadata ->
            Put_string_metadata (key, body, tags, metadata))
          gen_key gen_body gen_tags gen_metadata );
      (2, map (fun key -> Get_string key) gen_key);
      (2, map (fun key -> Find_string key) gen_key);
      (2, map (fun key -> Head_object key) gen_key);
      (2, map (fun key -> Exists_object key) gen_key);
      (2, map (fun key -> Delete_object key) gen_key);
      (1, return List_keys);
      (1, map (fun prefix -> List_prefix prefix) gen_prefix);
      (2, map2 (fun source dest -> Copy_object (source, dest)) gen_key gen_key);
      ( 2,
        map3
          (fun source dest metadata ->
            Copy_object_metadata (source, dest, metadata))
          gen_key gen_key gen_copy_metadata );
      (2, map2 (fun key tags -> Put_object_tags (key, tags)) gen_key gen_tags);
      (2, map (fun key -> Get_object_tags key) gen_key);
      (2, map (fun key -> Delete_object_tags key) gen_key);
      (1, map (fun tags -> Put_bucket_tags tags) gen_tags);
      (1, return Get_bucket_tags);
      (1, return Delete_bucket_tags);
      (1, map (fun status -> Put_versioning status) gen_versioning_status);
      (1, return Get_versioning);
    ]

let shrink_to_domain_first domain value =
  match domain with
  | first :: _ when not (String.equal value first) -> QCheck.Iter.return first
  | _ -> QCheck.Iter.empty

let shrink_key = shrink_to_domain_first key_domain
let shrink_prefix = shrink_to_domain_first prefix_domain

let shrink_body body =
  QCheck.Shrink.string ~shrink:QCheck.Shrink.char_printable body

let shrink_tags = function
  | [] -> QCheck.Iter.empty
  | _ :: _ -> QCheck.Iter.return []

let shrink_metadata = function
  | [] -> QCheck.Iter.empty
  | _ :: _ -> QCheck.Iter.return []

let shrink_copy_metadata = function
  | Copy_source_metadata -> QCheck.Iter.empty
  | Replace_metadata metadata ->
      QCheck.Iter.append
        (QCheck.Iter.return Copy_source_metadata)
        (QCheck.Iter.map
           (fun metadata -> Replace_metadata metadata)
           (shrink_metadata metadata))

let shrink_versioning_status = function
  | Awskit_s3.Bucket.Versioning.Status.Suspended ->
      QCheck.Iter.return Awskit_s3.Bucket.Versioning.Status.Enabled
  | Enabled | Unknown _ -> QCheck.Iter.empty

let shrinker = function
  | Put_string (key, body, tags) ->
      QCheck.Iter.of_list
        [
          QCheck.Iter.map
            (fun key -> Put_string (key, body, tags))
            (shrink_key key);
          QCheck.Iter.map
            (fun body -> Put_string (key, body, tags))
            (shrink_body body);
          QCheck.Iter.map
            (fun tags -> Put_string (key, body, tags))
            (shrink_tags tags);
        ]
      |> QCheck.Iter.flatten
  | Put_string_metadata (key, body, tags, metadata) ->
      QCheck.Iter.of_list
        [
          QCheck.Iter.map
            (fun key -> Put_string_metadata (key, body, tags, metadata))
            (shrink_key key);
          QCheck.Iter.map
            (fun body -> Put_string_metadata (key, body, tags, metadata))
            (shrink_body body);
          QCheck.Iter.map
            (fun tags -> Put_string_metadata (key, body, tags, metadata))
            (shrink_tags tags);
          QCheck.Iter.map
            (fun metadata -> Put_string_metadata (key, body, tags, metadata))
            (shrink_metadata metadata);
        ]
      |> QCheck.Iter.flatten
  | Get_string key ->
      QCheck.Iter.map (fun key -> Get_string key) (shrink_key key)
  | Find_string key ->
      QCheck.Iter.map (fun key -> Find_string key) (shrink_key key)
  | Head_object key ->
      QCheck.Iter.map (fun key -> Head_object key) (shrink_key key)
  | Exists_object key ->
      QCheck.Iter.map (fun key -> Exists_object key) (shrink_key key)
  | Delete_object key ->
      QCheck.Iter.map (fun key -> Delete_object key) (shrink_key key)
  | List_keys -> QCheck.Iter.empty
  | List_prefix prefix ->
      QCheck.Iter.map (fun prefix -> List_prefix prefix) (shrink_prefix prefix)
  | Copy_object (source_key, destination_key) ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun source_key -> Copy_object (source_key, destination_key))
           (shrink_key source_key))
        (QCheck.Iter.map
           (fun destination_key -> Copy_object (source_key, destination_key))
           (shrink_key destination_key))
  | Copy_object_metadata (source_key, destination_key, metadata) ->
      QCheck.Iter.of_list
        [
          QCheck.Iter.map
            (fun source_key ->
              Copy_object_metadata (source_key, destination_key, metadata))
            (shrink_key source_key);
          QCheck.Iter.map
            (fun destination_key ->
              Copy_object_metadata (source_key, destination_key, metadata))
            (shrink_key destination_key);
          QCheck.Iter.map
            (fun metadata ->
              Copy_object_metadata (source_key, destination_key, metadata))
            (shrink_copy_metadata metadata);
        ]
      |> QCheck.Iter.flatten
  | Put_object_tags (key, tags) ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun key -> Put_object_tags (key, tags))
           (shrink_key key))
        (QCheck.Iter.map
           (fun tags -> Put_object_tags (key, tags))
           (shrink_tags tags))
  | Get_object_tags key ->
      QCheck.Iter.map (fun key -> Get_object_tags key) (shrink_key key)
  | Delete_object_tags key ->
      QCheck.Iter.map (fun key -> Delete_object_tags key) (shrink_key key)
  | Put_bucket_tags tags ->
      QCheck.Iter.map (fun tags -> Put_bucket_tags tags) (shrink_tags tags)
  | Get_bucket_tags | Delete_bucket_tags -> QCheck.Iter.empty
  | Put_versioning status ->
      QCheck.Iter.map
        (fun status -> Put_versioning status)
        (shrink_versioning_status status)
  | Get_versioning -> QCheck.Iter.empty

let shrink_list = QCheck.Shrink.list ~shrink:shrinker

let arbitrary =
  QCheck.make ~print:transcript ~shrink:shrink_list
    QCheck.Gen.(list_size (int_range 1 40) generator)
