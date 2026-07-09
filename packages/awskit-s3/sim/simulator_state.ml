open Simulator_support
module Bucket = Awskit_s3.Bucket
module Content_type = Awskit_s3.Content_type
module Metadata = Awskit_s3.Metadata
module Multipart = Awskit_s3.Multipart
module Object = Awskit_s3.Object
module Policy = Awskit_s3.Policy
module Storage_class = Awskit_s3.Storage_class
module Tag = Awskit_s3.Tag

module Clock = struct
  type t = { mutable now : Ptime.t }

  let create ?(now = Ptime.epoch) () = { now }
  let now t = t.now

  let advance t span =
    t.now <- Option.value ~default:t.now (Ptime.add_span t.now span)

  let advance_ms t ms = advance t (Ptime.Span.of_int_s (ms / 1000))
end

type config = { max_list_keys : int }

let default_max_list_keys = 1000

let create_config ?(max_list_keys = default_max_list_keys) () =
  if max_list_keys < 1 || max_list_keys > 1000 then
    invalid ~field:"max_list_keys" "max_list_keys must be between 1 and 1000"
  else Ok { max_list_keys }

let create_config_exn ?max_list_keys () =
  Awskit.Error.Producer.get_ok_exn (create_config ?max_list_keys ())

let default_config = create_config_exn ()

type stored_object = {
  mutable body : string;
  mutable etag : Object.Etag.t;
  mutable version_id : Object.Version_id.t option;
  mutable content_type : Content_type.t option;
  mutable metadata : Metadata.t;
  mutable storage_class : Storage_class.t option;
  mutable tags : Tag.Set.t;
  mutable checksum : Object.Checksum.response;
  mutable last_modified : Ptime.t;
}

type stored_delete_marker = {
  version_id : Object.Version_id.t;
  last_modified : Ptime.t;
}

type stored_version =
  | Stored_object of stored_object
  | Stored_delete_marker of stored_delete_marker

type stored_part = {
  part_number : int;
  body : string;
  etag : Object.Etag.t;
  checksum : Object.Checksum.response;
  last_modified : Ptime.t;
}

type multipart_upload = {
  upload : Multipart.Upload.created Multipart.Upload.t;
  content_type : Content_type.t option;
  metadata : Metadata.t;
  storage_class : Storage_class.t option;
  tags : Tag.Set.t;
  checksum : Multipart.Create.Checksum.t option;
  parts : (int, stored_part) Hashtbl.t;
  created_at : Ptime.t;
}

type bucket_state = {
  created_at : Ptime.t;
  objects : (string, stored_version) Hashtbl.t;
  versions : (string, stored_version list) Hashtbl.t;
  multipart_uploads : (string, multipart_upload) Hashtbl.t;
  mutable policy : Policy.t option;
  mutable bucket_tags : Tag.Set.t;
  mutable versioning : Bucket.Versioning.Status.t option;
  mutable encryption : Bucket.Encryption.Observed.t option;
  mutable cors : Bucket.Cors.Observed.t option;
  mutable public_access_block : Bucket.Public_access_block.config option;
  mutable ownership_controls : Bucket.Ownership_controls.config option;
}

type operation =
  [ `Put_object
  | `Get_object
  | `Head_object
  | `Delete_object
  | `List_objects_v2
  | `List_object_versions
  | `Copy_object
  | `Delete_objects
  | `Create_multipart_upload
  | `Upload_part
  | `Complete_multipart_upload
  | `Abort_multipart_upload
  | `List_parts ]

type operation_record = {
  op : operation;
  bucket : string;
  key : string option;
  timestamp : Ptime.t;
  faulted : bool;
}

type store = {
  config : config;
  clock : Clock.t;
  buckets : (string, bucket_state) Hashtbl.t;
  mutable history : operation_record list;
  mutable next_upload_id : int;
  mutable next_version_id : int;
}

let create_store ?(config = default_config) ~clock () =
  {
    config;
    clock;
    buckets = Hashtbl.create 17;
    history = [];
    next_upload_id = 1;
    next_version_id = 1;
  }

let config t = t.config
let clock t = t.clock
let find_bucket t bucket = Hashtbl.find_opt t.buckets bucket
let bucket_exists t bucket = Hashtbl.mem t.buckets bucket
let add_bucket t bucket state = Hashtbl.add t.buckets bucket state
let remove_bucket t bucket = Hashtbl.remove t.buckets bucket

let sorted_bindings compare_key table =
  Hashtbl.to_seq table
  |> List.of_seq
  |> List.sort (fun (left, _) (right, _) -> compare_key left right)

let buckets store = sorted_bindings String.compare store.buckets
let buckets_seq store = List.to_seq (buckets store)
let objects bucket = sorted_bindings String.compare bucket.objects
let versions bucket = sorted_bindings String.compare bucket.versions
let parts upload = sorted_bindings Int.compare upload.parts |> List.map snd
let history t = List.rev t.history
let clear_history t = t.history <- []

type fault = Slow_down | Internal_error | Connection_reset | Response_lost
type random_faults = { random : Random.State.t; prob : float }

type t = {
  store : store;
  credentials : Awskit.Credentials.t;
  faults : fault Queue.t;
  mutable random_faults : random_faults option;
}

let connect store ~credentials =
  { store; credentials; faults = Queue.create (); random_faults = None }

let store t = t.store
let credentials t = t.credentials
let now t = Clock.now t.store.clock

let record_operation ?(faulted = false) t op bucket key =
  t.store.history <-
    { op; bucket; key; timestamp = now t; faulted } :: t.store.history

let allocate_upload_id t =
  let id = t.store.next_upload_id in
  t.store.next_upload_id <- id + 1;
  Multipart.Upload_id.of_string_exn (Fmt.str "simulator-upload-%d" id)

let allocate_version_id t =
  let id = t.store.next_version_id in
  t.store.next_version_id <- id + 1;
  Object.Version_id.of_string_exn (Fmt.str "simulator-version-%d" id)

let append_faults t faults =
  List.iter (fun fault -> Queue.add fault t.faults) faults

let clear_faults t = Queue.clear t.faults

let enable_random_faults t ~seed ~prob =
  t.random_faults <- Some { random = Random.State.make [| seed |]; prob }

let disable_random_faults t = t.random_faults <- None

let take_fault t =
  if not (Queue.is_empty t.faults) then Some (Queue.take t.faults)
  else
    match t.random_faults with
    | None -> None
    | Some random_faults ->
        if Random.State.float random_faults.random 1.0 < random_faults.prob then
          Some Internal_error
        else None
