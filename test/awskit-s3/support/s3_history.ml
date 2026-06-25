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

let command_supported config command =
  match (config.target_profile, command) with
  | Strict, _ -> true
  | Minio, S3_command.Put_string_metadata _ -> false
  | Minio, Copy_object_metadata _ -> false
  | Minio, Put_versioning status -> (
      match status with
      | Awskit_s3.Bucket.Versioning.Status.Suspended -> false
      | Enabled | Unknown _ -> true)
  | Minio, Copy_object (source_key, destination_key) ->
      not (String.equal source_key destination_key)
  | Minio, _ -> true

let history_supported config =
  List.for_all (fun command -> command_supported config command)

let existing_keys model = S3_model.keys model

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

let versioning_command_generator config =
  let statuses =
    List.filter
      (fun status ->
        command_supported config (S3_command.Put_versioning status))
      S3_command.versioning_status_domain
  in
  match statuses with
  | [] -> None
  | statuses ->
      Some
        QCheck.Gen.(
          map
            (fun status -> S3_command.Put_versioning status)
            (oneof_list statuses))

let maybe_supported config sample weight gen =
  if command_supported config sample then [ (weight, gen) ] else []

let maybe_weighted weight = function
  | None -> []
  | Some gen -> [ (weight, gen) ]

let command_gen config model =
  let open QCheck.Gen in
  let key_gen = oneof_list (S3_command.keys_for_profile config.value_profile) in
  let body_gen = S3_command.body_gen_for_profile config.value_profile in
  let existing = oneof_existing_key model "a.txt" in
  let absent = oneof_absent_key config model "missing/generated.txt" in
  oneof_weighted
    ([
       ( 5,
         map3
           (fun key body tags -> S3_command.Put_string (key, body, tags))
           key_gen body_gen S3_command.gen_tags );
     ]
    @ maybe_supported config
        (S3_command.Put_string_metadata ("a.txt", "", [], []))
        3
        (map4
           (fun key body tags metadata ->
             S3_command.Put_string_metadata (key, body, tags, metadata))
           key_gen body_gen S3_command.gen_tags S3_command.gen_metadata)
    @ [
        (3, map (fun key -> S3_command.Get_string key) existing);
        (2, map (fun key -> S3_command.Get_string key) absent);
        (2, map (fun key -> S3_command.Find_string key) existing);
        (1, map (fun key -> S3_command.Find_string key) absent);
        (3, map (fun key -> S3_command.Head_object key) existing);
        (1, map (fun key -> S3_command.Head_object key) absent);
        (2, map (fun key -> S3_command.Exists_object key) existing);
        (1, map (fun key -> S3_command.Exists_object key) absent);
        (2, map (fun key -> S3_command.Delete_object key) existing);
        (1, map (fun key -> S3_command.Delete_object key) absent);
        (2, copy_object_gen config existing);
        (1, copy_object_gen config absent);
      ]
    @ maybe_supported config
        (S3_command.Copy_object_metadata ("a.txt", "b.txt", Copy_source_metadata))
        2
        (copy_object_metadata_gen config existing)
    @ maybe_supported config
        (S3_command.Copy_object_metadata
           ("missing.txt", "b.txt", Copy_source_metadata))
        1
        (copy_object_metadata_gen config absent)
    @ [
        (2, return S3_command.List_keys);
        ( 2,
          map
            (fun prefix -> S3_command.List_prefix prefix)
            S3_command.gen_prefix );
        ( 2,
          map2
            (fun key tags -> S3_command.Put_object_tags (key, tags))
            existing S3_command.gen_tags );
        ( 1,
          map2
            (fun key tags -> S3_command.Put_object_tags (key, tags))
            absent S3_command.gen_tags );
        (1, map (fun key -> S3_command.Get_object_tags key) existing);
        (1, map (fun key -> S3_command.Get_object_tags key) absent);
        (1, map (fun key -> S3_command.Delete_object_tags key) existing);
        (1, map (fun key -> S3_command.Delete_object_tags key) absent);
        ( 1,
          map (fun tags -> S3_command.Put_bucket_tags tags) S3_command.gen_tags
        );
        (1, return S3_command.Get_bucket_tags);
        (1, return S3_command.Delete_bucket_tags);
        (1, return S3_command.Get_versioning);
      ]
    @ maybe_weighted 1 (versioning_command_generator config))

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
