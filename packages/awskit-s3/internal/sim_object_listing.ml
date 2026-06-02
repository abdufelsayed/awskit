open Core
open Sim_state
open Sim_checksum

let visible_objects bucket_state (options : List_objects_v2.options) =
  Hashtbl.to_seq bucket_state.objects
  |> Seq.filter_map (function
    | key, Stored_object obj -> (
        match options.prefix with
        | None -> Some (key, obj)
        | Some prefix -> if is_prefix ~prefix key then Some (key, obj) else None
        )
    | _, Stored_delete_marker _ -> None)
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let after_start_marker objects (options : List_objects_v2.options) =
  match options.continuation_token with
  | Some token ->
      List.filter (fun (key, _) -> String.compare key token > 0) objects
  | None -> (
      match options.start_after with
      | None -> objects
      | Some start_after ->
          List.filter
            (fun (key, _) -> String.compare key start_after > 0)
            objects)

let page ~default_max_keys ~bucket bucket_state
    (options : List_objects_v2.options) ~response =
  let all = after_start_marker (visible_objects bucket_state options) options in
  let max_keys = Option.value ~default:default_max_keys options.max_keys in
  let selected = all |> List.to_seq |> Seq.take max_keys |> List.of_seq in
  let is_truncated = List.length all > List.length selected in
  let next_continuation_token =
    if not is_truncated then None
    else match List.rev selected with [] -> None | (key, _) :: _ -> Some key
  in
  let objects =
    List.map
      (fun (key, (obj : stored_object)) ->
        {
          List_objects_v2.key;
          size = Some (Int64.of_int (String.length obj.body));
          etag = Some obj.etag;
          last_modified = Some obj.last_modified;
          storage_class = obj.storage_class;
          checksum = checksum_summary obj.checksum;
        })
      selected
  in
  {
    List_objects_v2.bucket = Some bucket;
    prefix = options.prefix;
    delimiter = options.delimiter;
    objects;
    common_prefixes = [];
    key_count = Some (List.length objects);
    is_truncated;
    continuation_token = options.continuation_token;
    next_continuation_token;
    response;
  }
