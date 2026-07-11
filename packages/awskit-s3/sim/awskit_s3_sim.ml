module Clock = Simulator_state.Clock

type config = Simulator_state.config = private { max_list_keys : int }

let config = Simulator_state.create_config
let config_exn = Simulator_state.create_config_exn
let default_config = Simulator_state.default_config

type store = Simulator_state.store

let create_store = Simulator_state.create_store

module Runtime = Simulator_runtime.Runtime
module S3 = Awskit_s3.Make (Runtime)

type t = S3.t
type +'a io = 'a

let connect store ~credentials =
  Simulator_state.connect store ~credentials |> S3.create

let runtime_connection = S3.runtime_connection
let store client = Simulator_state.store (runtime_connection client)

module Body = S3.Body
module Reader = S3.Reader

type fault = Simulator_state.fault =
  | Slow_down
  | Internal_error
  | Connection_reset
  | Response_lost

let inject_fault client fault =
  Simulator_inspect.inject_fault (runtime_connection client) fault

let inject_faults client faults =
  Simulator_inspect.inject_faults (runtime_connection client) faults

let clear_faults client =
  Simulator_inspect.clear_faults (runtime_connection client)

let enable_random_faults client ~seed ~prob =
  Simulator_inspect.enable_random_faults (runtime_connection client) ~seed ~prob

let disable_random_faults client =
  Simulator_inspect.disable_random_faults (runtime_connection client)

type operation_record = Simulator_state.operation_record = {
  op : Simulator_state.operation;
  bucket : string;
  key : string option;
  timestamp : Ptime.t;
  faulted : bool;
}

type object_metadata = Simulator_inspect.object_metadata = {
  etag : Awskit_s3.Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

let history = Simulator_inspect.history
let clear_history = Simulator_inspect.clear_history
let bucket_to_string = Awskit_s3.Bucket_name.to_string
let key_to_string = Awskit_s3.Object_key.to_string

let object_metadata store ~bucket ~key =
  Simulator_inspect.object_metadata store ~bucket:(bucket_to_string bucket)
    ~key:(key_to_string key)

let keys store ~bucket =
  Simulator_inspect.keys store ~bucket:(bucket_to_string bucket)

let objects_as_strings store ~bucket =
  Simulator_inspect.objects_as_strings store ~bucket:(bucket_to_string bucket)

module Object = struct
  type client = t
  type 'a io = 'a
  type request_body = Body.t
  type response_body_reader = Reader.t

  module Raw = Simulator_object.Object

  let raw client = runtime_connection client

  let put conn ~bucket ~key ?options ~body () =
    Raw.put (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ~body ()

  let put_string conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_string contents) ()

  let put_bytes conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_bytes contents) ()

  let get conn ~bucket ~key ?options ~consume () =
    Raw.get (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ~consume ()

  let validate_max_bytes max_bytes =
    if Int64.compare max_bytes 0L < 0 then
      Error
        (Awskit.Error.Producer.validation ~field:"max_bytes"
           (Fmt.str "max_bytes must be non-negative, got %Ld" max_bytes))
    else Ok ()

  let get_string conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error _ as error -> error
    | Ok () ->
        get conn ~bucket ~key ?options ~consume:(Reader.to_string ~max_bytes) ()

  let get_bytes conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error _ as error -> error
    | Ok () ->
        get conn ~bucket ~key ?options ~consume:(Reader.to_bytes ~max_bytes) ()

  let find conn ~bucket ~key ?options ~consume () =
    Raw.find (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ~consume ()

  let find_string conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error _ as error -> error
    | Ok () ->
        find conn ~bucket ~key ?options
          ~consume:(Reader.to_string ~max_bytes)
          ()

  let find_bytes conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error _ as error -> error
    | Ok () ->
        find conn ~bucket ~key ?options ~consume:(Reader.to_bytes ~max_bytes) ()

  let head conn ~bucket ~key ?options () =
    Raw.head (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ()

  let find_metadata conn ~bucket ~key ?options () =
    Raw.find_metadata (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ()

  let exists conn ~bucket ~key ?options () =
    Raw.exists (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ()

  let delete conn ~bucket ~key ?options () =
    Raw.delete (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ()

  let delete_objects conn ~bucket ~objects ?expected_bucket_owner () =
    Raw.delete_objects (raw conn) ~bucket:(bucket_to_string bucket) ~objects
      ?expected_bucket_owner ()

  let copy conn ~source_bucket ~source_key ~destination_bucket ~destination_key
      ?options () =
    Raw.copy (raw conn)
      ~source_bucket:(bucket_to_string source_bucket)
      ~source_key:(key_to_string source_key)
      ~destination_bucket:(bucket_to_string destination_bucket)
      ~destination_key:(key_to_string destination_key)
      ?options ()

  let list_versions conn ~bucket ?options () =
    Raw.list_versions (raw conn) ~bucket:(bucket_to_string bucket) ?options ()

  let list conn ~bucket ?options () =
    Raw.list (raw conn) ~bucket:(bucket_to_string bucket) ?options ()

  module List = struct
    type 'acc fold_step = 'acc Raw.List.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages (raw conn) ~bucket:(bucket_to_string bucket) ?options
        ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages_until (raw conn) ~bucket:(bucket_to_string bucket)
        ?options ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.List.pages (raw conn) ~bucket:(bucket_to_string bucket) ?options
        ~max_pages ()

    let objects conn ~bucket ?options ~max_pages () =
      Raw.List.objects (raw conn) ~bucket:(bucket_to_string bucket) ?options
        ~max_pages ()

    let keys conn ~bucket ?options ~max_pages () =
      Raw.List.keys (raw conn) ~bucket:(bucket_to_string bucket) ?options
        ~max_pages ()
  end

  module Versions = struct
    type 'acc fold_step = 'acc Raw.Versions.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages (raw conn) ~bucket:(bucket_to_string bucket)
        ?options ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages_until (raw conn) ~bucket:(bucket_to_string bucket)
        ?options ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.Versions.pages (raw conn) ~bucket:(bucket_to_string bucket) ?options
        ~max_pages ()

    let object_versions conn ~bucket ?options ~max_pages () =
      Raw.Versions.object_versions (raw conn) ~bucket:(bucket_to_string bucket)
        ?options ~max_pages ()

    let delete_markers conn ~bucket ?options ~max_pages () =
      Raw.Versions.delete_markers (raw conn) ~bucket:(bucket_to_string bucket)
        ?options ~max_pages ()
  end

  module Tagging = struct
    let get conn ~bucket ~key ?expected_bucket_owner () =
      Raw.Tagging.get (raw conn) ~bucket:(bucket_to_string bucket)
        ~key:(key_to_string key) ?expected_bucket_owner ()

    let put conn ~bucket ~key ?expected_bucket_owner ~tags () =
      Raw.Tagging.put (raw conn) ~bucket:(bucket_to_string bucket)
        ~key:(key_to_string key) ?expected_bucket_owner ~tags ()

    let delete conn ~bucket ~key ?expected_bucket_owner () =
      Raw.Tagging.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ~key:(key_to_string key) ?expected_bucket_owner ()
  end
end

module Bucket = struct
  type client = t
  type 'a io = 'a

  module Raw = Simulator_bucket.Bucket

  let raw client = runtime_connection client

  let create conn ~bucket ?region () =
    Raw.create (raw conn) ~bucket:(bucket_to_string bucket) ?region ()

  let delete conn ~bucket ?expected_bucket_owner () =
    Raw.delete (raw conn) ~bucket:(bucket_to_string bucket)
      ?expected_bucket_owner ()

  let head conn ~bucket ?expected_bucket_owner () =
    Raw.head (raw conn) ~bucket:(bucket_to_string bucket) ?expected_bucket_owner
      ()

  let exists conn ~bucket ?expected_bucket_owner () =
    Raw.exists (raw conn) ~bucket:(bucket_to_string bucket)
      ?expected_bucket_owner ()

  let list conn = Raw.list (raw conn)

  let get_location conn ~bucket ?expected_bucket_owner () =
    Raw.get_location (raw conn) ~bucket:(bucket_to_string bucket)
      ?expected_bucket_owner ()

  module Policy = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Policy.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~policy () =
      Raw.Policy.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~policy ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Policy.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()
  end

  module Versioning = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Versioning.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~status () =
      Raw.Versioning.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~status ()
  end

  module Tagging = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Tagging.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~tags () =
      Raw.Tagging.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~tags ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Tagging.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()
  end

  module Encryption = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Encryption.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~config () =
      Raw.Encryption.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~config ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Encryption.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()
  end

  module Cors = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Cors.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~config () =
      Raw.Cors.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~config ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Cors.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()
  end

  module Public_access_block = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Public_access_block.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~config () =
      Raw.Public_access_block.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~config ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Public_access_block.delete (raw conn)
        ~bucket:(bucket_to_string bucket) ?expected_bucket_owner ()
  end

  module Ownership_controls = struct
    let get conn ~bucket ?expected_bucket_owner () =
      Raw.Ownership_controls.get (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()

    let put conn ~bucket ?expected_bucket_owner ~config () =
      Raw.Ownership_controls.put (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ~config ()

    let delete conn ~bucket ?expected_bucket_owner () =
      Raw.Ownership_controls.delete (raw conn) ~bucket:(bucket_to_string bucket)
        ?expected_bucket_owner ()
  end
end

module Multipart = struct
  type client = t
  type 'a io = 'a
  type request_body = Body.t

  module Raw = Simulator_multipart.Multipart

  let raw client = runtime_connection client

  let create_upload conn ~bucket ~key ?options () =
    Raw.create_upload (raw conn) ~bucket:(bucket_to_string bucket)
      ~key:(key_to_string key) ?options ()

  let upload_part conn ~upload ~part_number ~body ?options () =
    Raw.upload_part (raw conn) ~upload ~part_number ~body ?options ()

  let complete_upload conn ~upload ?options ~parts () =
    Raw.complete_upload (raw conn) ~upload ?options ~parts ()

  let abort_upload conn ~upload ?expected_bucket_owner () =
    Raw.abort_upload (raw conn) ~upload ?expected_bucket_owner ()

  let list_parts conn ~upload ?options () =
    Raw.list_parts (raw conn) ~upload ?options ()

  module List_parts = struct
    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      Raw.List_parts.fold_pages (raw conn) ~upload ?options ?max_pages ~init ~f
        ()

    let pages conn ~upload ?options ?max_pages () =
      Raw.List_parts.pages (raw conn) ~upload ?options ?max_pages ()

    let parts conn ~upload ?options ?max_pages () =
      Raw.List_parts.parts (raw conn) ~upload ?options ?max_pages ()
  end
end

module Presigned = S3.Presigned
