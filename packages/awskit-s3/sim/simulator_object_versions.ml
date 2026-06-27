open Simulator_support
open Simulator_state
open Simulator_store
open Simulator_checksum
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key

type version_entry =
  | Object_version of Object.Versions.object_version
  | Delete_marker of Object.Versions.delete_marker

type listing_entry =
  | Version_entry of version_entry
  | Common_prefix of Object_key.Prefix.t

let version_entry_key = function
  | Object_version version -> version.key
  | Delete_marker marker -> marker.key

let version_entry_id = function
  | Object_version version -> version.version_id
  | Delete_marker marker -> marker.version_id

let listing_entry_marker = function
  | Version_entry entry -> Object_key.to_string (version_entry_key entry)
  | Common_prefix prefix -> Object_key.Prefix.to_string prefix

let listing_entry_key_marker = function
  | Version_entry entry -> version_entry_key entry
  | Common_prefix prefix ->
      Object_key.of_string_exn (Object_key.Prefix.to_string prefix)

let listing_entry_id = function
  | Version_entry entry -> version_entry_id entry
  | Common_prefix _ -> None

let version_entries_after_marker key_marker version_id_marker entries =
  match key_marker with
  | None -> entries
  | Some key_marker ->
      let key_marker = Object_key.to_string key_marker in
      let rec drop = function
        | [] -> []
        | entry :: rest -> (
            let key = Object_key.to_string (version_entry_key entry) in
            match String.compare key key_marker with
            | value when value > 0 -> entry :: rest
            | value when value < 0 -> drop rest
            | _ -> (
                match version_id_marker with
                | None -> entry :: rest
                | Some marker -> (
                    match version_entry_id entry with
                    | Some version_id
                      when Object.Version_id.equal version_id marker ->
                        entry :: rest
                    | _ -> drop rest)))
      in
      drop entries

let listing_entries_after_marker key_marker version_id_marker entries =
  match key_marker with
  | None -> entries
  | Some key_marker ->
      let key_marker = Object_key.to_string key_marker in
      let rec drop = function
        | [] -> []
        | entry :: rest -> (
            match String.compare (listing_entry_marker entry) key_marker with
            | value when value > 0 -> entry :: rest
            | value when value < 0 -> drop rest
            | _ -> (
                match (entry, version_id_marker) with
                | _, None -> entry :: rest
                | Version_entry version_entry, Some marker -> (
                    match version_entry_id version_entry with
                    | Some version_id
                      when Object.Version_id.equal version_id marker ->
                        entry :: rest
                    | _ -> drop rest)
                | _ -> drop rest))
      in
      drop entries

let version_entry_is_current bucket key version =
  match (Hashtbl.find_opt bucket.objects key, version) with
  | Some (Stored_object current), Stored_object obj -> current == obj
  | Some (Stored_delete_marker current), Stored_delete_marker marker ->
      Object.Version_id.equal current.version_id marker.version_id
  | _ -> false

let sorted_version_entries bucket (options : Object.Versions.options) =
  let prefix =
    Option.map Object_key.Prefix.to_string options.Object.Versions.prefix
  in
  let from_version key version =
    let is_latest = Some (version_entry_is_current bucket key version) in
    match version with
    | Stored_object obj ->
        Object_version
          {
            Object.Versions.key = Object_key.of_string_exn key;
            version_id = obj.version_id;
            is_latest;
            last_modified = Some obj.last_modified;
            etag = Some obj.etag;
            size = Some (Int64.of_int (String.length obj.body));
            storage_class = obj.storage_class;
            owner = None;
            checksum = checksum_summary obj.checksum;
          }
    | Stored_delete_marker marker ->
        Delete_marker
          {
            Object.Versions.key = Object_key.of_string_exn key;
            version_id = Some marker.version_id;
            is_latest;
            last_modified = Some marker.last_modified;
            owner = None;
          }
  in
  let versioned =
    Simulator_state.versions bucket
    |> List.concat_map (fun (key, versions) ->
        List.map (fun version -> (key, version)) versions)
  in
  let unversioned =
    Simulator_state.objects bucket
    |> List.filter_map (fun (key, version) ->
        if Hashtbl.mem bucket.versions key then None else Some (key, version))
  in
  versioned @ unversioned
  |> List.filter (fun (key, _) ->
      match prefix with None -> true | Some prefix -> is_prefix ~prefix key)
  |> List.sort (fun (left_key, left_version) (right_key, right_version) ->
      match String.compare left_key right_key with
      | 0 ->
          let left_current =
            version_entry_is_current bucket left_key left_version
          in
          let right_current =
            version_entry_is_current bucket right_key right_version
          in
          Bool.compare right_current left_current
      | value -> value)
  |> List.map (fun (key, version) -> from_version key version)

let version_entries bucket (options : Object.Versions.options) =
  sorted_version_entries bucket options
  |> version_entries_after_marker options.key_marker options.version_id_marker

let find_sub ~sub value =
  let sub_len = String.length sub in
  let value_len = String.length value in
  let rec loop index =
    if sub_len = 0 || index + sub_len > value_len then None
    else if String.equal (String.sub value index sub_len) sub then Some index
    else loop (index + 1)
  in
  loop 0

let common_prefix_for_key (options : Object.Versions.options) key =
  match options.delimiter with
  | None -> None
  | Some delimiter -> (
      let key = Object_key.to_string key in
      let prefix =
        Option.value ~default:""
          (Option.map Object_key.Prefix.to_string options.prefix)
      in
      let delimiter = Object.Versions.Delimiter.to_string delimiter in
      let rest =
        String.sub key (String.length prefix)
          (String.length key - String.length prefix)
      in
      match find_sub ~sub:delimiter rest with
      | None -> None
      | Some index ->
          let prefix_len = index + String.length delimiter in
          Some
            (Object_key.Prefix.of_string_exn
               (prefix ^ String.sub rest 0 prefix_len)))

let listing_entry_of_version options entry =
  match common_prefix_for_key options (version_entry_key entry) with
  | None -> Version_entry entry
  | Some prefix -> Common_prefix prefix

let dedupe_listing_entries entries =
  let rec dedupe seen acc = function
    | [] ->
        List.stable_sort
          (fun left right ->
            String.compare
              (listing_entry_marker left)
              (listing_entry_marker right))
          (List.rev acc)
    | Common_prefix prefix :: rest ->
        let prefix_text = Object_key.Prefix.to_string prefix in
        if List.exists (String.equal prefix_text) seen then dedupe seen acc rest
        else dedupe (prefix_text :: seen) (Common_prefix prefix :: acc) rest
    | (Version_entry _ as entry) :: rest -> dedupe seen (entry :: acc) rest
  in
  dedupe [] [] entries

let listing_entries bucket (options : Object.Versions.options) =
  sorted_version_entries bucket options
  |> List.map (listing_entry_of_version options)
  |> dedupe_listing_entries
  |> listing_entries_after_marker options.key_marker options.version_id_marker
