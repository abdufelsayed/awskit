type tag_model = (string * string) list
type metadata_model = (string * string) list
type copy_metadata = Copy_source_metadata | Replace_metadata of metadata_model
type value_profile = Small | Broad

type t =
  | Put_string of string * string * tag_model
  | Put_string_metadata of string * string * tag_model * metadata_model
  | Get_string of string
  | Get_range of string * Awskit_s3.Range.t
  | Find_string of string
  | Head_object of string
  | Exists_object of string
  | Delete_object of string
  | List_keys
  | List_prefix of string
  | List_keys_page of { prefix : string option; max_keys : int }
  | List_versions_page of { max_keys : int }
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

let ascii_key_domain =
  [ "a.txt"; "b.txt"; "logs/a.txt"; "logs/b.txt"; "photos/2026.jpg" ]

let broad_key_domain =
  ascii_key_domain
  @ [
      "space key.txt";
      "unicode-\206\180.txt";
      "prefix//double-slash";
      "prefix/trailing/";
      "copy/source-object";
      "copy/destination-object";
    ]

let keys_for_profile = function
  | Small -> ascii_key_domain
  | Broad -> broad_key_domain

let key_domain = ascii_key_domain
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

let range_to_string range = Awskit_s3.Range.to_header range

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
  | Get_range (key, range) ->
      Printf.sprintf "get-range key=%S range=%S" key (range_to_string range)
  | Find_string key -> Printf.sprintf "find-string key=%S" key
  | Head_object key -> Printf.sprintf "head-object key=%S" key
  | Exists_object key -> Printf.sprintf "exists-object key=%S" key
  | Delete_object key -> Printf.sprintf "delete-object key=%S" key
  | List_keys -> "list-keys"
  | List_prefix prefix -> Printf.sprintf "list-prefix prefix=%S" prefix
  | List_keys_page { prefix; max_keys } ->
      Printf.sprintf "list-keys-page prefix=%S max=%d"
        (Option.value ~default:"" prefix)
        max_keys
  | List_versions_page { max_keys } ->
      Printf.sprintf "list-versions-page max=%d" max_keys
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

let command_bin = function
  | Put_string _ -> "s3.command.put"
  | Put_string_metadata _ -> "s3.command.put-metadata"
  | Get_string _ -> "s3.command.get"
  | Get_range _ -> "s3.command.get-range"
  | Find_string _ -> "s3.command.find"
  | Head_object _ -> "s3.command.head"
  | Exists_object _ -> "s3.command.exists"
  | Delete_object _ -> "s3.command.delete"
  | List_keys -> "s3.command.list"
  | List_prefix _ -> "s3.command.list-prefix"
  | List_keys_page _ -> "s3.command.list-keys-page"
  | List_versions_page _ -> "s3.command.list-versions-page"
  | Copy_object _ -> "s3.command.copy"
  | Copy_object_metadata _ -> "s3.command.copy-metadata"
  | Put_object_tags _ -> "s3.command.put-object-tags"
  | Get_object_tags _ -> "s3.command.get-object-tags"
  | Delete_object_tags _ -> "s3.command.delete-object-tags"
  | Put_bucket_tags _ -> "s3.command.put-bucket-tags"
  | Get_bucket_tags -> "s3.command.get-bucket-tags"
  | Delete_bucket_tags -> "s3.command.delete-bucket-tags"
  | Put_versioning Enabled -> "s3.command.versioning.enabled"
  | Put_versioning Suspended -> "s3.command.versioning.suspended"
  | Put_versioning (Unknown _) -> "s3.command.versioning.unknown"
  | Get_versioning -> "s3.command.get-versioning"

let put_key = function
  | Put_string (key, _, _) | Put_string_metadata (key, _, _, _) -> Some key
  | _ -> None

let read_key = function
  | Get_string key
  | Get_range (key, _)
  | Find_string key
  | Head_object key
  | Exists_object key ->
      Some key
  | _ -> None

let range_read_key = function Get_range (key, _) -> Some key | _ -> None
let delete_key = function Delete_object key -> Some key | _ -> None

let object_tag_mutation_key = function
  | Put_object_tags (key, _) | Delete_object_tags key -> Some key
  | _ -> None

let object_tag_read_key = function Get_object_tags key -> Some key | _ -> None

let copy_destination_key = function
  | Copy_object (_, destination_key)
  | Copy_object_metadata (_, destination_key, _) ->
      Some destination_key
  | _ -> None

let same_key left right =
  match (left, right) with
  | Some left, Some right -> String.equal left right
  | None, _ | _, None -> false

let adjacent_transition_bins_for_pair left right =
  let add_if condition bin acc = if condition then bin :: acc else acc in
  []
  |> add_if (same_key (put_key left) (read_key right)) "s3.history.put-read"
  |> add_if
       (same_key (put_key left) (range_read_key right))
       "s3.history.put-range-read"
  |> add_if (same_key (put_key left) (delete_key right)) "s3.history.put-delete"
  |> add_if
       (match (left, right) with
       | ( Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled,
           Delete_object _ ) ->
           true
       | _ -> false)
       "s3.history.versioning-enabled-delete"
  |> add_if
       (same_key (object_tag_mutation_key left) (object_tag_read_key right))
       "s3.history.object-tag-mutation-read"
  |> add_if
       (same_key (copy_destination_key left) (read_key right))
       "s3.history.copy-destination-read"

let adjacent_transition_bins commands =
  let rec loop acc = function
    | left :: right :: rest ->
        loop
          (List.rev_append (adjacent_transition_bins_for_pair left right) acc)
          (right :: rest)
    | [] | [ _ ] -> List.rev acc
  in
  loop [] commands

let history_bins commands =
  let command_bins = List.map command_bin commands in
  let saw_put = ref false in
  let saw_enabled_after_put = ref false in
  let saw_delete_after_versioning = ref false in
  let versioning = ref false in
  List.iter
    (function
      | Put_string _ | Put_string_metadata _ -> saw_put := true
      | Put_versioning Enabled when !saw_put ->
          versioning := true;
          saw_enabled_after_put := true
      | Put_versioning Enabled | Put_versioning Suspended -> versioning := true
      | Delete_object _ when !versioning -> saw_delete_after_versioning := true
      | _ -> ())
    commands;
  let enabled_after_put_bins =
    if !saw_enabled_after_put then [ "s3.history.versioning-after-put" ] else []
  in
  let delete_after_versioning_bins =
    if !saw_delete_after_versioning then
      [ "s3.history.delete-after-versioning" ]
    else []
  in
  command_bins
  @ adjacent_transition_bins commands
  @ enabled_after_put_bins
  @ delete_after_versioning_bins

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

let range_domain =
  [
    Awskit_s3.Range.bytes_exn ~start:0L ~finish:0L;
    Awskit_s3.Range.bytes_exn ~start:0L ~finish:3L;
    Awskit_s3.Range.bytes_exn ~start:2L ~finish:5L;
    Awskit_s3.Range.bytes_exn ~start:64L ~finish:128L;
    Awskit_s3.Range.from_exn 0L;
    Awskit_s3.Range.from_exn 3L;
    Awskit_s3.Range.from_exn 64L;
    Awskit_s3.Range.suffix_exn 1L;
    Awskit_s3.Range.suffix_exn 4L;
    Awskit_s3.Range.suffix_exn 64L;
  ]

let gen_range =
  QCheck.Gen.(
    oneof_weighted
      [
        (4, oneof_list range_domain);
        ( 3,
          map2
            (fun start span ->
              let start = Int64.of_int start in
              let finish = Int64.add start (Int64.of_int span) in
              Awskit_s3.Range.bytes_exn ~start ~finish)
            (int_range 0 128) (int_range 0 128) );
        ( 2,
          map
            (fun start -> Awskit_s3.Range.from_exn (Int64.of_int start))
            (int_range 0 128) );
        ( 2,
          map
            (fun length -> Awskit_s3.Range.suffix_exn (Int64.of_int length))
            (int_range 1 128) );
      ])

let gen_copy_metadata =
  QCheck.Gen.(
    oneof
      [
        return Copy_source_metadata;
        map (fun metadata -> Replace_metadata metadata) gen_metadata;
      ])

let body_gen_for_profile = function
  | Small ->
      QCheck.Gen.(
        string_size
          ~gen:
            (oneof_weighted
               [ (8, char_range 'a' 'z'); (1, return ' '); (1, numeral) ])
          (int_range 0 12))
  | Broad ->
      QCheck.Gen.(
        oneof_weighted
          [
            (5, string_size ~gen:(char_range 'a' 'z') (int_range 0 128));
            (2, return "");
            (1, return "\000\255binary-ish");
            (1, string_size ~gen:(char_range ' ' '~') (int_range 129 4096));
          ])

let gen_body = body_gen_for_profile Small

let generator_with ~gen_key ~gen_body =
  QCheck.Gen.(
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
        (2, map2 (fun key range -> Get_range (key, range)) gen_key gen_range);
        (2, map (fun key -> Find_string key) gen_key);
        (2, map (fun key -> Head_object key) gen_key);
        (2, map (fun key -> Exists_object key) gen_key);
        (2, map (fun key -> Delete_object key) gen_key);
        (1, return List_keys);
        (1, map (fun prefix -> List_prefix prefix) gen_prefix);
        ( 2,
          map2
            (fun prefix max_keys -> List_keys_page { prefix; max_keys })
            (oneof [ return None; map Option.some gen_prefix ])
            (int_range 1 3) );
        ( 1,
          map (fun max_keys -> List_versions_page { max_keys }) (int_range 1 3)
        );
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
      ])

let generator_for_profile profile =
  let gen_key = QCheck.Gen.oneof_list (keys_for_profile profile) in
  let gen_body = body_gen_for_profile profile in
  generator_with ~gen_key ~gen_body

let generator = generator_for_profile Small

let shrink_to_domain_first domain value =
  match domain with
  | first :: _ when not (String.equal value first) -> QCheck.Iter.return first
  | _ -> QCheck.Iter.empty

let shrink_key = shrink_to_domain_first key_domain
let shrink_prefix = shrink_to_domain_first prefix_domain

let shrink_optional_prefix = function
  | None -> QCheck.Iter.empty
  | Some prefix ->
      QCheck.Iter.append (QCheck.Iter.return None)
        (QCheck.Iter.map (fun prefix -> Some prefix) (shrink_prefix prefix))

let shrink_max_keys max_keys =
  if max_keys > 1 then QCheck.Iter.return 1 else QCheck.Iter.empty

let shrink_body body =
  QCheck.Shrink.string ~shrink:QCheck.Shrink.char_printable body

let shrink_range range =
  match Awskit_s3.Range.view range with
  | Awskit_s3.Range.Bytes (0L, 0L) -> QCheck.Iter.empty
  | Awskit_s3.Range.Bytes _ ->
      QCheck.Iter.of_list
        [
          Awskit_s3.Range.bytes_exn ~start:0L ~finish:0L;
          Awskit_s3.Range.from_exn 0L;
          Awskit_s3.Range.suffix_exn 1L;
        ]
  | Awskit_s3.Range.From 0L -> QCheck.Iter.empty
  | Awskit_s3.Range.From _ ->
      QCheck.Iter.of_list
        [
          Awskit_s3.Range.from_exn 0L;
          Awskit_s3.Range.bytes_exn ~start:0L ~finish:0L;
        ]
  | Awskit_s3.Range.Suffix 1L -> QCheck.Iter.empty
  | Awskit_s3.Range.Suffix _ ->
      QCheck.Iter.of_list
        [ Awskit_s3.Range.suffix_exn 1L; Awskit_s3.Range.from_exn 0L ]

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
  | Get_range (key, range) ->
      QCheck.Iter.append
        (QCheck.Iter.map (fun key -> Get_range (key, range)) (shrink_key key))
        (QCheck.Iter.map
           (fun range -> Get_range (key, range))
           (shrink_range range))
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
  | List_keys_page { prefix; max_keys } ->
      QCheck.Iter.append
        (QCheck.Iter.map
           (fun prefix -> List_keys_page { prefix; max_keys })
           (shrink_optional_prefix prefix))
        (QCheck.Iter.map
           (fun max_keys -> List_keys_page { prefix; max_keys })
           (shrink_max_keys max_keys))
  | List_versions_page { max_keys } ->
      QCheck.Iter.map
        (fun max_keys -> List_versions_page { max_keys })
        (shrink_max_keys max_keys)
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
