open Core
open Sim_state
open Sim_store
open Sim_checksum

type version_entry =
  | Object_version of List_object_versions.object_version
  | Delete_marker of List_object_versions.delete_marker

let version_entry_key = function
  | Object_version version -> version.key
  | Delete_marker marker -> marker.key

let version_entry_id = function
  | Object_version version -> version.version_id
  | Delete_marker marker -> marker.version_id

let version_entries_after_marker key_marker version_id_marker entries =
  match key_marker with
  | None -> entries
  | Some key_marker ->
      let rec drop = function
        | [] -> []
        | entry :: rest -> (
            let key = version_entry_key entry in
            match String.compare key key_marker with
            | value when value > 0 -> entry :: rest
            | value when value < 0 -> drop rest
            | _ -> (
                match version_id_marker with
                | None -> drop rest
                | Some marker -> (
                    match version_entry_id entry with
                    | Some version_id
                      when Object.Version_id.equal version_id marker ->
                        rest
                    | _ -> drop rest)))
      in
      drop entries

let version_entry_is_current bucket key version =
  match (Hashtbl.find_opt bucket.objects key, version) with
  | Some (Stored_object current), Stored_object obj -> current == obj
  | Some (Stored_delete_marker current), Stored_delete_marker marker ->
      Object.Version_id.equal current.version_id marker.version_id
  | _ -> false

let version_entries bucket (options : List_object_versions.options) =
  let from_version key version =
    let is_latest = Some (version_entry_is_current bucket key version) in
    match version with
    | Stored_object obj ->
        Object_version
          {
            List_object_versions.key;
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
            List_object_versions.key;
            version_id = Some marker.version_id;
            is_latest;
            last_modified = Some marker.last_modified;
            owner = None;
          }
  in
  let versioned =
    Hashtbl.to_seq bucket.versions
    |> Seq.flat_map (fun (key, versions) ->
        versions |> List.to_seq |> Seq.map (fun version -> (key, version)))
    |> List.of_seq
  in
  let unversioned =
    Hashtbl.to_seq bucket.objects
    |> Seq.filter_map (fun (key, version) ->
        if Hashtbl.mem bucket.versions key then None else Some (key, version))
    |> List.of_seq
  in
  versioned @ unversioned
  |> List.filter (fun (key, _) ->
      match options.List_object_versions.prefix with
      | None -> true
      | Some prefix -> is_prefix ~prefix key)
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
  |> version_entries_after_marker options.key_marker options.version_id_marker
