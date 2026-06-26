type target_profile = Strict | Minio

type config = {
  target_profile : target_profile;
  value_profile : S3_command.value_profile;
  min_length : int;
  max_length : int;
}

let strict_default =
  {
    target_profile = Strict;
    value_profile = Broad;
    min_length = 1;
    max_length = 60;
  }

let minio_default =
  {
    target_profile = Minio;
    value_profile = Small;
    min_length = 1;
    max_length = 25;
  }

let supports_command target_profile command =
  match (target_profile, command) with
  | Strict, _ -> true
  | Minio, S3_command.Put_string_metadata _ -> false
  | Minio, List_versions_page _ -> false
  | Minio, Copy_object_metadata _ -> false
  | Minio, Put_versioning status -> (
      match status with
      | Awskit_s3.Bucket.Versioning.Status.Enabled -> true
      | Suspended | Unknown _ -> false)
  | Minio, Copy_object (source_key, destination_key) ->
      not (String.equal source_key destination_key)
  | Minio, _ -> true

let command_supported config command =
  supports_command config.target_profile command

let existing_keys model = S3_model.keys model

let minio_can_enable_versioning model =
  (* MinIO does not report null version ids for current objects that predate
     enabling versioning, so that transition stays outside this profile. *)
  S3_model.versioning_keeps_history model || existing_keys model = []

let command_supported_in_state config model command =
  command_supported config command
  &&
  match (config.target_profile, command) with
  | Minio, S3_command.Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled
    ->
      minio_can_enable_versioning model
  | _ -> true

let history_supported config commands =
  let rec loop model = function
    | [] -> true
    | command :: rest ->
        command_supported_in_state config model command
        && loop (S3_model.apply command model) rest
  in
  loop S3_model.empty commands

let list_page_after_multiple_visible_keys model = function
  | S3_command.List_keys_page { prefix; max_keys = _ } ->
      List.length (S3_model.keys_for_page ?prefix model) > 1
  | _ -> false

let stateful_transition_bins model command =
  let versioning_after_existing_object =
    match command with
    | S3_command.Put_versioning Awskit_s3.Bucket.Versioning.Status.Enabled
      when S3_model.keys model <> [] ->
        [ "s3.history.versioning-enabled-after-existing-object" ]
    | _ -> []
  in
  let list_page_bins =
    if list_page_after_multiple_visible_keys model command then
      [ "s3.history.list-page-after-multiple-visible-keys" ]
    else []
  in
  versioning_after_existing_object @ list_page_bins

let state_bins (model : S3_model.t) =
  let object_count = List.length (S3_model.keys model) in
  let object_count_bin =
    if object_count = 0 then "s3.state.empty"
    else if object_count = 1 then "s3.state.one-object"
    else "s3.state.multiple-objects"
  in
  let versioning_bins =
    match model.versioning with
    | Some Awskit_s3.Bucket.Versioning.Status.Enabled ->
        [ "s3.state.versioning-enabled" ]
    | Some Suspended -> [ "s3.state.versioning-suspended" ]
    | Some (Unknown _) | None -> []
  in
  let delete_marker_bins =
    if List.is_empty (S3_model.delete_markers model) then []
    else [ "s3.state.delete-marker-present" ]
  in
  let bucket_tag_bins =
    if List.is_empty model.bucket_tags then []
    else [ "s3.state.bucket-tags-present" ]
  in
  let object_tag_bins =
    if
      S3_model.String_map.exists
        (fun _key (object_ : S3_model.object_) ->
          not (List.is_empty object_.tags))
        model.objects
    then [ "s3.state.object-tags-present" ]
    else []
  in
  (object_count_bin :: versioning_bins)
  @ delete_marker_bins
  @ bucket_tag_bins
  @ object_tag_bins

let coverage_bins commands =
  let rec loop model state_acc transition_bins = function
    | [] -> state_acc @ transition_bins
    | command :: rest ->
        let transition_bins =
          stateful_transition_bins model command @ transition_bins
        in
        let next_model = S3_model.apply command model in
        loop next_model (state_bins next_model @ state_acc) transition_bins rest
  in
  S3_command.history_bins commands
  @ loop S3_model.empty (state_bins S3_model.empty) [] commands

let absent_keys config model =
  S3_command.keys_for_profile config.value_profile
  |> List.filter (fun key -> not (List.mem key (existing_keys model)))

let oneof_existing_key model fallback =
  match existing_keys model with
  | [] -> QCheck.Gen.return fallback
  | keys -> QCheck.Gen.oneof_list keys

let oneof_absent_key config model fallback =
  match absent_keys config model with
  | [] -> QCheck.Gen.return fallback
  | keys -> QCheck.Gen.oneof_list keys

let fallback_destination source =
  if String.equal source "a.txt" then "b.txt" else "a.txt"

let destination_keys config source =
  let keys = S3_command.keys_for_profile config.value_profile in
  match config.target_profile with
  | Strict -> keys
  | Minio -> List.filter (fun key -> not (String.equal key source)) keys

let oneof_destination_key config source =
  match destination_keys config source with
  | [] -> QCheck.Gen.return (fallback_destination source)
  | keys -> QCheck.Gen.oneof_list keys

let copy_object_gen config source_gen =
  let open QCheck.Gen in
  source_gen >>= fun source ->
  map
    (fun destination -> S3_command.Copy_object (source, destination))
    (oneof_destination_key config source)

let copy_object_metadata_gen config source_gen =
  let open QCheck.Gen in
  source_gen >>= fun source ->
  map2
    (fun destination metadata ->
      S3_command.Copy_object_metadata (source, destination, metadata))
    (oneof_destination_key config source)
    S3_command.gen_copy_metadata

type command_candidate = {
  weight : int;
  example : S3_command.t;
  gen : S3_command.t QCheck.Gen.t;
}

let candidate weight example gen = { weight; example; gen }

let supported_candidates config model candidates =
  List.filter
    (fun candidate -> command_supported_in_state config model candidate.example)
    candidates

let versioning_command_candidate config model =
  let statuses =
    List.filter
      (fun status ->
        command_supported_in_state config model
          (S3_command.Put_versioning status))
      S3_command.versioning_status_domain
  in
  match statuses with
  | [] -> None
  | example_status :: _ ->
      Some
        (candidate 1 (S3_command.Put_versioning example_status)
           QCheck.Gen.(
             map
               (fun status -> S3_command.Put_versioning status)
               (oneof_list statuses)))

let command_gen config model =
  let open QCheck.Gen in
  let key_gen = oneof_list (S3_command.keys_for_profile config.value_profile) in
  let body_gen = S3_command.body_gen_for_profile config.value_profile in
  let existing = oneof_existing_key model "a.txt" in
  let absent = oneof_absent_key config model "missing/generated.txt" in
  let candidates =
    ([
       candidate 5
         (S3_command.Put_string ("a.txt", "", []))
         (map3
            (fun key body tags -> S3_command.Put_string (key, body, tags))
            key_gen body_gen S3_command.gen_tags);
       candidate 3
         (S3_command.Put_string_metadata ("a.txt", "", [], []))
         (map4
            (fun key body tags metadata ->
              S3_command.Put_string_metadata (key, body, tags, metadata))
            key_gen body_gen S3_command.gen_tags S3_command.gen_metadata);
       candidate 3 (S3_command.Get_string "a.txt")
         (map (fun key -> S3_command.Get_string key) existing);
       candidate 2 (S3_command.Get_string "missing/generated.txt")
         (map (fun key -> S3_command.Get_string key) absent);
       candidate 2 (S3_command.Find_string "a.txt")
         (map (fun key -> S3_command.Find_string key) existing);
       candidate 1 (S3_command.Find_string "missing/generated.txt")
         (map (fun key -> S3_command.Find_string key) absent);
       candidate 3 (S3_command.Head_object "a.txt")
         (map (fun key -> S3_command.Head_object key) existing);
       candidate 1 (S3_command.Head_object "missing/generated.txt")
         (map (fun key -> S3_command.Head_object key) absent);
       candidate 2 (S3_command.Exists_object "a.txt")
         (map (fun key -> S3_command.Exists_object key) existing);
       candidate 1 (S3_command.Exists_object "missing/generated.txt")
         (map (fun key -> S3_command.Exists_object key) absent);
       candidate 2 (S3_command.Delete_object "a.txt")
         (map (fun key -> S3_command.Delete_object key) existing);
       candidate 1 (S3_command.Delete_object "missing/generated.txt")
         (map (fun key -> S3_command.Delete_object key) absent);
       candidate 2
         (S3_command.Copy_object ("a.txt", fallback_destination "a.txt"))
         (copy_object_gen config existing);
       candidate 1
         (S3_command.Copy_object
            ( "missing/generated.txt",
              fallback_destination "missing/generated.txt" ))
         (copy_object_gen config absent);
       candidate 2
         (S3_command.Copy_object_metadata
            ("a.txt", "b.txt", Copy_source_metadata))
         (copy_object_metadata_gen config existing);
       candidate 1
         (S3_command.Copy_object_metadata
            ("missing.txt", "b.txt", Copy_source_metadata))
         (copy_object_metadata_gen config absent);
       candidate 2 S3_command.List_keys (return S3_command.List_keys);
       candidate 2 (S3_command.List_prefix "logs/")
         (map
            (fun prefix -> S3_command.List_prefix prefix)
            S3_command.gen_prefix);
       candidate 2
         (S3_command.List_keys_page { prefix = None; max_keys = 1 })
         (map2
            (fun prefix max_keys ->
              S3_command.List_keys_page { prefix; max_keys })
            (oneof [ return None; map Option.some S3_command.gen_prefix ])
            (int_range 1 3));
       candidate 1
         (S3_command.List_versions_page { max_keys = 1 })
         (map
            (fun max_keys -> S3_command.List_versions_page { max_keys })
            (int_range 1 3));
       candidate 2
         (S3_command.Put_object_tags ("a.txt", []))
         (map2
            (fun key tags -> S3_command.Put_object_tags (key, tags))
            existing S3_command.gen_tags);
       candidate 1
         (S3_command.Put_object_tags ("missing/generated.txt", []))
         (map2
            (fun key tags -> S3_command.Put_object_tags (key, tags))
            absent S3_command.gen_tags);
       candidate 1 (S3_command.Get_object_tags "a.txt")
         (map (fun key -> S3_command.Get_object_tags key) existing);
       candidate 1 (S3_command.Get_object_tags "missing/generated.txt")
         (map (fun key -> S3_command.Get_object_tags key) absent);
       candidate 1 (S3_command.Delete_object_tags "a.txt")
         (map (fun key -> S3_command.Delete_object_tags key) existing);
       candidate 1 (S3_command.Delete_object_tags "missing/generated.txt")
         (map (fun key -> S3_command.Delete_object_tags key) absent);
       candidate 1 (S3_command.Put_bucket_tags [])
         (map (fun tags -> S3_command.Put_bucket_tags tags) S3_command.gen_tags);
       candidate 1 S3_command.Get_bucket_tags
         (return S3_command.Get_bucket_tags);
       candidate 1 S3_command.Delete_bucket_tags
         (return S3_command.Delete_bucket_tags);
       candidate 1 S3_command.Get_versioning (return S3_command.Get_versioning);
     ]
    @
    match versioning_command_candidate config model with
    | None -> []
    | Some candidate -> [ candidate ])
    |> supported_candidates config model
  in
  match candidates with
  | [] -> return S3_command.List_keys
  | candidates ->
      oneof_weighted
        (List.map
           (fun candidate -> (candidate.weight, candidate.gen))
           candidates)

let generator config =
  let open QCheck.Gen in
  int_range config.min_length config.max_length >>= fun length ->
  let rec loop model remaining acc =
    if remaining = 0 then return (List.rev acc)
    else
      command_gen config model >>= fun command ->
      let next_model = S3_model.apply command model in
      loop next_model (remaining - 1) (command :: acc)
  in
  loop S3_model.empty length []

let shrink config commands =
  let shrink = S3_command.shrink_list commands in
  match config.target_profile with
  | Strict -> shrink
  | Minio -> QCheck.Iter.filter (history_supported config) shrink

let arbitrary config =
  QCheck.make ~print:S3_command.transcript ~shrink:(shrink config)
    (generator config)
