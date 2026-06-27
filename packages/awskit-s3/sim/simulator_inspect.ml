open Simulator_support
open Simulator_state
open Simulator_store
open Simulator_checksum
module Object = Awskit_s3.Object
module Range = Awskit_s3.Range

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
      objects bucket
      |> List.filter_map (function
        | key, Stored_object _ -> Some key
        | _, Stored_delete_marker _ -> None)

let history = Simulator_state.history
let clear_history = Simulator_state.clear_history

let compare_object_body_pair (left_key, left_body) (right_key, right_body) =
  match String.compare left_key right_key with
  | 0 -> String.compare left_body right_body
  | value -> value

let objects_as_strings store ~bucket =
  match bucket_state store bucket with
  | None -> []
  | Some (bucket : bucket_state) ->
      objects bucket
      |> List.filter_map (function
        | key, Stored_object obj -> Some (key, obj.body)
        | _, Stored_delete_marker _ -> None)
      |> List.sort compare_object_body_pair

let inject_fault t fault = append_faults t [ fault ]
let inject_faults = Simulator_state.append_faults
let clear_faults = Simulator_state.clear_faults
let enable_random_faults = Simulator_state.enable_random_faults
let disable_random_faults = Simulator_state.disable_random_faults

let content_range response =
  match Awskit.Response.header response "content-range" with
  | None -> None
  | Some value ->
      Some
        (Awskit.Error.Producer.get_ok_exn (Range.Content_range.of_header value))

let info_of_object ?content_length response (obj : stored_object) :
    Object.Get.info =
  {
    Object.Get.etag = Some obj.etag;
    content_type = obj.content_type;
    content_length =
      Some
        (Option.value ~default:(String.length obj.body) content_length
        |> Int64.of_int);
    content_range = content_range response;
    last_modified = Some obj.last_modified;
    metadata = obj.metadata;
    storage_class = obj.storage_class;
    version_id = obj.version_id;
    checksum = obj.checksum;
    encryption = None;
    response;
  }
