open Core
open Sim_state
open Sim_store
open Sim_checksum

type object_metadata = {
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

let object_metadata store ~bucket ~key =
  match bucket_state store bucket with
  | None -> None
  | Some bucket -> (
      match Hashtbl.find_opt bucket.objects key with
      | None | Some (Stored_delete_marker _) -> None
      | Some (Stored_object obj) ->
          Some
            {
              etag = Some obj.etag;
              size = Some (Int64.of_int (String.length obj.body));
              last_modified = Some obj.last_modified;
            })

let keys store ~bucket =
  match bucket_state store bucket with
  | None -> []
  | Some bucket ->
      Hashtbl.to_seq_keys bucket.objects
      |> Seq.filter (fun key ->
          match Hashtbl.find_opt bucket.objects key with
          | Some (Stored_object _) -> true
          | None | Some (Stored_delete_marker _) -> false)
      |> List.of_seq
      |> List.sort String.compare

let history = Sim_state.history
let clear_history = Sim_state.clear_history

let objects_as_strings store ~bucket =
  match bucket_state store bucket with
  | None -> []
  | Some (bucket : bucket_state) ->
      Hashtbl.to_seq bucket.objects
      |> Seq.filter_map (function
        | key, Stored_object obj -> Some (key, obj.body)
        | _, Stored_delete_marker _ -> None)
      |> List.of_seq
      |> List.sort compare

let inject_fault t fault = append_faults t [ fault ]
let inject_faults = Sim_state.append_faults
let clear_faults = Sim_state.clear_faults
let enable_random_faults = Sim_state.enable_random_faults
let disable_random_faults = Sim_state.disable_random_faults

let info_of_object ?content_length response (obj : stored_object) =
  {
    Get_object.etag = Some obj.etag;
    content_type = obj.content_type;
    content_length =
      Some
        (Option.value ~default:(String.length obj.body) content_length
        |> Int64.of_int);
    last_modified = Some obj.last_modified;
    metadata = obj.metadata;
    storage_class = obj.storage_class;
    version_id = obj.version_id;
    checksum = obj.checksum;
    server_side_encryption = None;
    response;
  }
