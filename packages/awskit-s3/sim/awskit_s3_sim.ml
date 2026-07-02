module Clock = Simulator_state.Clock

type config = Simulator_state.config = private { max_list_keys : int }

let config = Simulator_state.create_config
let config_exn = Simulator_state.create_config_exn
let default_config = Simulator_state.default_config

type store = Simulator_state.store

let create_store = Simulator_state.create_store

type t = Simulator_state.t

let connect = Simulator_state.connect
let store = Simulator_state.store

module Runtime = Simulator_runtime.Runtime
module S3 = Awskit_s3.Make (Runtime)
module Body = S3.Body
module Reader = S3.Reader

type fault = Simulator_state.fault =
  | Slow_down
  | Internal_error
  | Connection_reset
  | Response_lost

let inject_fault = Simulator_inspect.inject_fault
let inject_faults = Simulator_inspect.inject_faults
let clear_faults = Simulator_inspect.clear_faults
let enable_random_faults = Simulator_inspect.enable_random_faults
let disable_random_faults = Simulator_inspect.disable_random_faults

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

let object_metadata store ~bucket ~key =
  Simulator_inspect.object_metadata store ~bucket ~key

let keys store ~bucket = Simulator_inspect.keys store ~bucket

let objects_as_strings store ~bucket =
  Simulator_inspect.objects_as_strings store ~bucket

module Object = struct
  type connection = t
  type 'a io = 'a
  type request_body = Body.t
  type response_body_reader = Reader.t

  module Raw = Simulator_object.Object

  let put conn ~bucket ~key ?options ~body () =
    Raw.put conn ~bucket ~key ?options ~body ()

  let put_string conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_string contents) ()

  let put_bytes conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_bytes contents) ()

  let get conn ~bucket ~key ?options ~consume () =
    Raw.get conn ~bucket ~key ?options ~consume ()

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
    Raw.find conn ~bucket ~key ?options ~consume ()

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
    Raw.head conn ~bucket ~key ?options ()

  let find_metadata conn ~bucket ~key ?options () =
    Raw.find_metadata conn ~bucket ~key ?options ()

  let exists conn ~bucket ~key ?options () =
    Raw.exists conn ~bucket ~key ?options ()

  let delete conn ~bucket ~key ?options () =
    Raw.delete conn ~bucket ~key ?options ()

  let delete_objects conn ~bucket ~objects ?options () =
    Raw.delete_objects conn ~bucket ~objects ?options ()

  let copy conn ~source_bucket ~source_key ~destination_bucket ~destination_key
      ?options () =
    Raw.copy conn ~source_bucket ~source_key ~destination_bucket
      ~destination_key ?options ()

  let list_versions conn ~bucket ?options () =
    Raw.list_versions conn ~bucket ?options ()

  let list conn ~bucket ?options () = Raw.list conn ~bucket ?options ()

  module List = struct
    type 'acc fold_step = 'acc Raw.List.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages conn ~bucket ?options ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages_until conn ~bucket ?options ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.List.pages conn ~bucket ?options ~max_pages ()

    let objects conn ~bucket ?options ~max_pages () =
      Raw.List.objects conn ~bucket ?options ~max_pages ()

    let keys conn ~bucket ?options ~max_pages () =
      Raw.List.keys conn ~bucket ?options ~max_pages ()
  end

  module Versions = struct
    type 'acc fold_step = 'acc Raw.Versions.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages conn ~bucket ?options ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages_until conn ~bucket ?options ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.Versions.pages conn ~bucket ?options ~max_pages ()

    let object_versions conn ~bucket ?options ~max_pages () =
      Raw.Versions.object_versions conn ~bucket ?options ~max_pages ()

    let delete_markers conn ~bucket ?options ~max_pages () =
      Raw.Versions.delete_markers conn ~bucket ?options ~max_pages ()
  end

  module Tagging = struct
    let get conn ~bucket ~key ?options () =
      Raw.Tagging.get conn ~bucket ~key ?options ()

    let put conn ~bucket ~key ?options ~tags () =
      Raw.Tagging.put conn ~bucket ~key ?options ~tags ()

    let delete conn ~bucket ~key ?options () =
      Raw.Tagging.delete conn ~bucket ~key ?options ()
  end
end

module Bucket = struct
  type connection = t
  type 'a io = 'a

  module Raw = Simulator_bucket.Bucket

  let create conn ~bucket ?options () = Raw.create conn ~bucket ?options ()
  let delete conn ~bucket ?options () = Raw.delete conn ~bucket ?options ()
  let head conn ~bucket ?options () = Raw.head conn ~bucket ?options ()
  let exists conn ~bucket ?options () = Raw.exists conn ~bucket ?options ()
  let list = Raw.list

  let get_location conn ~bucket ?options () =
    Raw.get_location conn ~bucket ?options ()

  module Policy = struct
    let get conn ~bucket ?options () = Raw.Policy.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~policy () =
      Raw.Policy.put conn ~bucket ?options ~policy ()

    let delete conn ~bucket ?options () =
      Raw.Policy.delete conn ~bucket ?options ()
  end

  module Versioning = struct
    let get conn ~bucket ?options () =
      Raw.Versioning.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~status () =
      Raw.Versioning.put conn ~bucket ?options ~status ()
  end

  module Tagging = struct
    let get conn ~bucket ?options () = Raw.Tagging.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~tags () =
      Raw.Tagging.put conn ~bucket ?options ~tags ()

    let delete conn ~bucket ?options () =
      Raw.Tagging.delete conn ~bucket ?options ()
  end

  module Encryption = struct
    let get conn ~bucket ?options () =
      Raw.Encryption.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~config () =
      Raw.Encryption.put conn ~bucket ?options ~config ()

    let delete conn ~bucket ?options () =
      Raw.Encryption.delete conn ~bucket ?options ()
  end

  module Cors = struct
    let get conn ~bucket ?options () = Raw.Cors.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~config () =
      Raw.Cors.put conn ~bucket ?options ~config ()

    let delete conn ~bucket ?options () =
      Raw.Cors.delete conn ~bucket ?options ()
  end

  module Public_access_block = struct
    let get conn ~bucket ?options () =
      Raw.Public_access_block.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~config () =
      Raw.Public_access_block.put conn ~bucket ?options ~config ()

    let delete conn ~bucket ?options () =
      Raw.Public_access_block.delete conn ~bucket ?options ()
  end

  module Ownership_controls = struct
    let get conn ~bucket ?options () =
      Raw.Ownership_controls.get conn ~bucket ?options ()

    let put conn ~bucket ?options ~config () =
      Raw.Ownership_controls.put conn ~bucket ?options ~config ()

    let delete conn ~bucket ?options () =
      Raw.Ownership_controls.delete conn ~bucket ?options ()
  end
end

module Multipart = struct
  type connection = t
  type 'a io = 'a
  type request_body = Body.t

  module Raw = Simulator_multipart.Multipart

  let create_upload conn ~bucket ~key ?options () =
    Raw.create_upload conn ~bucket ~key ?options ()

  let upload_part conn ~upload ~part_number ~body ?options () =
    match Awskit_s3.Multipart.Part_number.of_int part_number with
    | Error _ as error -> error
    | Ok part_number ->
        Raw.upload_part conn ~upload ~part_number ~body ?options ()

  let complete_upload conn ~upload ?options ~parts () =
    Raw.complete_upload conn ~upload ?options ~parts ()

  let abort_upload conn ~upload ?options () =
    Raw.abort_upload conn ~upload ?options ()

  let list_parts conn ~upload ?options () =
    Raw.list_parts conn ~upload ?options ()

  module List_parts = struct
    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      Raw.List_parts.fold_pages conn ~upload ?options ?max_pages ~init ~f ()

    let pages conn ~upload ?options ?max_pages () =
      Raw.List_parts.pages conn ~upload ?options ?max_pages ()

    let parts conn ~upload ?options ?max_pages () =
      Raw.List_parts.parts conn ~upload ?options ?max_pages ()
  end
end

module Presigned = Simulator_presigned.Presigned
