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
  op : Awskit_s3.Operation.t;
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
let observations = Simulator_state.observations
let clear_observations = Simulator_state.clear_observations
let bucket_to_string = Awskit_s3.Bucket_name.to_string
let key_to_string = Awskit_s3.Object_key.to_string

module Operation = Awskit_s3.Operation

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let observe conn ~operation ?logical_request_bytes
    ?logical_response_bytes_on_success thunk =
  let record ?retry_class ?logical_response_bytes outcome =
    let completion =
      Awskit_s3.Observability.For_simulator.complete ~operation ~outcome
        ?retry_class ?logical_request_bytes ?logical_response_bytes ()
    in
    Simulator_state.record_observation conn completion
  in
  match thunk () with
  | Ok _ as result ->
      let logical_response_bytes =
        Base.Option.bind logical_response_bytes_on_success ~f:(fun bytes ->
            bytes ())
      in
      record ?logical_response_bytes Awskit.Observability.Outcome.Ok;
      result
  | Error error as result ->
      record
        ~retry_class:(Awskit.Error.retry_class error)
        (Awskit.Observability.Outcome.of_error error);
      result
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      record Awskit.Observability.Outcome.Exception;
      Printexc.raise_with_backtrace exn backtrace

let measured_consumer consume =
  let logical_response_bytes = ref None in
  let consume reader =
    let result = consume reader in
    logical_response_bytes :=
      Some
        (Simulator_runtime.Runtime.response_body_reader_consumed_bytes reader);
    result
  in
  (consume, fun () -> !logical_response_bytes)

let object_metadata store ~bucket ~key =
  Simulator_inspect.object_metadata store ~bucket:(bucket_to_string bucket)
    ~key:(key_to_string key)

let keys store ~bucket =
  Simulator_inspect.keys store ~bucket:(bucket_to_string bucket)

let objects_as_strings store ~bucket =
  Simulator_inspect.objects_as_strings store ~bucket:(bucket_to_string bucket)

module Object = struct
  type connection = t
  type 'a io = 'a
  type request_body = Body.t
  type response_body_reader = Reader.t

  module Raw = Simulator_object.Object

  let put conn ~bucket ~key ?options ~body () =
    observe conn ~operation:Operation.Put_object
      ?logical_request_bytes:(Body.content_length body) (fun () ->
        Raw.put conn ~bucket:(bucket_to_string bucket) ~key:(key_to_string key)
          ?options ~body ())

  let put_string conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_string contents) ()

  let put_bytes conn ~bucket ~key ?options ~contents () =
    put conn ~bucket ~key ?options ~body:(Body.of_bytes contents) ()

  let get conn ~bucket ~key ?options ~consume () =
    let consume, logical_response_bytes_on_success =
      measured_consumer consume
    in
    observe conn ~operation:Operation.Get_object
      ~logical_response_bytes_on_success (fun () ->
        Raw.get conn ~bucket:(bucket_to_string bucket) ~key:(key_to_string key)
          ?options ~consume ())

  let validate_max_bytes max_bytes =
    if Int64.compare max_bytes 0L < 0 then
      Error
        (Awskit.Error.Producer.validation ~field:"max_bytes"
           (Fmt.str "max_bytes must be non-negative, got %Ld" max_bytes))
    else Ok ()

  let get_string conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error error ->
        observe conn ~operation:Operation.Get_object (fun () -> Error error)
    | Ok () ->
        get conn ~bucket ~key ?options ~consume:(Reader.to_string ~max_bytes) ()

  let get_bytes conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error error ->
        observe conn ~operation:Operation.Get_object (fun () -> Error error)
    | Ok () ->
        get conn ~bucket ~key ?options ~consume:(Reader.to_bytes ~max_bytes) ()

  let find conn ~bucket ~key ?options ~consume () =
    let consume, logical_response_bytes_on_success =
      measured_consumer consume
    in
    observe conn ~operation:Operation.Get_object
      ~logical_response_bytes_on_success (fun () ->
        Raw.find conn ~bucket:(bucket_to_string bucket) ~key:(key_to_string key)
          ?options ~consume ())

  let find_string conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error error ->
        observe conn ~operation:Operation.Get_object (fun () -> Error error)
    | Ok () ->
        find conn ~bucket ~key ?options
          ~consume:(Reader.to_string ~max_bytes)
          ()

  let find_bytes conn ~bucket ~key ?options ~max_bytes () =
    match validate_max_bytes max_bytes with
    | Error error ->
        observe conn ~operation:Operation.Get_object (fun () -> Error error)
    | Ok () ->
        find conn ~bucket ~key ?options ~consume:(Reader.to_bytes ~max_bytes) ()

  let head conn ~bucket ~key ?options () =
    observe conn ~operation:Operation.Head_object (fun () ->
        Raw.head conn ~bucket:(bucket_to_string bucket) ~key:(key_to_string key)
          ?options ())

  let find_metadata conn ~bucket ~key ?options () =
    match head conn ~bucket ~key ?options () with
    | Ok value -> Ok (Some value)
    | Error error when Awskit_s3.Error.is_no_such_key error -> Ok None
    | Error error -> Error error

  let exists conn ~bucket ~key ?options () =
    match head conn ~bucket ~key ?options () with
    | Ok _ -> Ok true
    | Error error when Awskit_s3.Error.is_no_such_key error -> Ok false
    | Error error -> Error error

  let delete conn ~bucket ~key ?options () =
    observe conn ~operation:Operation.Delete_object (fun () ->
        Raw.delete conn ~bucket:(bucket_to_string bucket)
          ~key:(key_to_string key) ?options ())

  let delete_objects conn ~bucket ~objects ?options () =
    observe conn ~operation:Operation.Delete_objects (fun () ->
        Raw.delete_objects conn ~bucket:(bucket_to_string bucket) ~objects
          ?options ())

  let copy conn ~source_bucket ~source_key ~destination_bucket ~destination_key
      ?options () =
    observe conn ~operation:Operation.Copy_object (fun () ->
        Raw.copy conn
          ~source_bucket:(bucket_to_string source_bucket)
          ~source_key:(key_to_string source_key)
          ~destination_bucket:(bucket_to_string destination_bucket)
          ~destination_key:(key_to_string destination_key)
          ?options ())

  let list_versions conn ~bucket ?options () =
    observe conn ~operation:Operation.List_object_versions (fun () ->
        Raw.list_versions conn ~bucket:(bucket_to_string bucket) ?options ())

  let list conn ~bucket ?options () =
    observe conn ~operation:Operation.List_objects_v2 (fun () ->
        Raw.list conn ~bucket:(bucket_to_string bucket) ?options ())

  module List = struct
    type 'acc fold_step = 'acc Raw.List.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fetch_page conn ~bucket ~options () = list conn ~bucket ~options ()

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages_with ~fetch:fetch_page conn ~bucket ?options
        ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.List.fold_pages_until_with ~fetch:fetch_page conn ~bucket ?options
        ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.List.pages_with ~fetch:fetch_page conn ~bucket ?options ~max_pages ()

    let objects conn ~bucket ?options ~max_pages () =
      Raw.List.objects_with ~fetch:fetch_page conn ~bucket ?options ~max_pages
        ()

    let keys conn ~bucket ?options ~max_pages () =
      Raw.List.keys_with ~fetch:fetch_page conn ~bucket ?options ~max_pages ()
  end

  module Versions = struct
    type 'acc fold_step = 'acc Raw.Versions.fold_step =
      | Continue of 'acc
      | Stop of 'acc

    let fetch_page conn ~bucket ~options () =
      list_versions conn ~bucket ~options ()

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages_with ~fetch:fetch_page conn ~bucket ?options
        ?max_pages ~init ~f ()

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      Raw.Versions.fold_pages_until_with ~fetch:fetch_page conn ~bucket ?options
        ?max_pages ~init ~f ()

    let pages conn ~bucket ?options ~max_pages () =
      Raw.Versions.pages_with ~fetch:fetch_page conn ~bucket ?options ~max_pages
        ()

    let object_versions conn ~bucket ?options ~max_pages () =
      Raw.Versions.object_versions_with ~fetch:fetch_page conn ~bucket ?options
        ~max_pages ()

    let delete_markers conn ~bucket ?options ~max_pages () =
      Raw.Versions.delete_markers_with ~fetch:fetch_page conn ~bucket ?options
        ~max_pages ()
  end

  module Tagging = struct
    let get conn ~bucket ~key ?options () =
      observe conn ~operation:Operation.Get_object_tagging (fun () ->
          Raw.Tagging.get conn ~bucket:(bucket_to_string bucket)
            ~key:(key_to_string key) ?options ())

    let put conn ~bucket ~key ?options ~tags () =
      observe conn ~operation:Operation.Put_object_tagging (fun () ->
          Raw.Tagging.put conn ~bucket:(bucket_to_string bucket)
            ~key:(key_to_string key) ?options ~tags ())

    let delete conn ~bucket ~key ?options () =
      observe conn ~operation:Operation.Delete_object_tagging (fun () ->
          Raw.Tagging.delete conn ~bucket:(bucket_to_string bucket)
            ~key:(key_to_string key) ?options ())
  end
end

module Bucket = struct
  type connection = t
  type 'a io = 'a

  module Raw = Simulator_bucket.Bucket

  let create conn ~bucket ?options () =
    observe conn ~operation:Operation.Create_bucket (fun () ->
        Raw.create conn ~bucket:(bucket_to_string bucket) ?options ())

  let delete conn ~bucket ?options () =
    observe conn ~operation:Operation.Delete_bucket (fun () ->
        Raw.delete conn ~bucket:(bucket_to_string bucket) ?options ())

  let head conn ~bucket ?options () =
    observe conn ~operation:Operation.Head_bucket (fun () ->
        Raw.head conn ~bucket:(bucket_to_string bucket) ?options ())

  let exists conn ~bucket ?options () =
    match head conn ~bucket ?options () with
    | Ok _ -> Ok true
    | Error error when Awskit_s3.Error.is_not_found error -> Ok false
    | Error error -> Error error

  let list conn =
    observe conn ~operation:Operation.List_buckets (fun () -> Raw.list conn)

  let get_location conn ~bucket ?options () =
    observe conn ~operation:Operation.Get_bucket_location (fun () ->
        Raw.get_location conn ~bucket:(bucket_to_string bucket) ?options ())

  module Policy = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_policy (fun () ->
          Raw.Policy.get conn ~bucket:(bucket_to_string bucket) ?options ())

    let put conn ~bucket ?options ~policy () =
      observe conn ~operation:Operation.Put_bucket_policy (fun () ->
          Raw.Policy.put conn ~bucket:(bucket_to_string bucket) ?options ~policy
            ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_bucket_policy (fun () ->
          Raw.Policy.delete conn ~bucket:(bucket_to_string bucket) ?options ())
  end

  module Versioning = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_versioning (fun () ->
          Raw.Versioning.get conn ~bucket:(bucket_to_string bucket) ?options ())

    let put conn ~bucket ?options ~status () =
      observe conn ~operation:Operation.Put_bucket_versioning (fun () ->
          Raw.Versioning.put conn ~bucket:(bucket_to_string bucket) ?options
            ~status ())
  end

  module Tagging = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_tagging (fun () ->
          Raw.Tagging.get conn ~bucket:(bucket_to_string bucket) ?options ())

    let put conn ~bucket ?options ~tags () =
      observe conn ~operation:Operation.Put_bucket_tagging (fun () ->
          Raw.Tagging.put conn ~bucket:(bucket_to_string bucket) ?options ~tags
            ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_bucket_tagging (fun () ->
          Raw.Tagging.delete conn ~bucket:(bucket_to_string bucket) ?options ())
  end

  module Encryption = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_encryption (fun () ->
          Raw.Encryption.get conn ~bucket:(bucket_to_string bucket) ?options ())

    let put conn ~bucket ?options ~config () =
      observe conn ~operation:Operation.Put_bucket_encryption (fun () ->
          Raw.Encryption.put conn ~bucket:(bucket_to_string bucket) ?options
            ~config ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_bucket_encryption (fun () ->
          Raw.Encryption.delete conn ~bucket:(bucket_to_string bucket) ?options
            ())
  end

  module Cors = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_cors (fun () ->
          Raw.Cors.get conn ~bucket:(bucket_to_string bucket) ?options ())

    let put conn ~bucket ?options ~config () =
      observe conn ~operation:Operation.Put_bucket_cors (fun () ->
          Raw.Cors.put conn ~bucket:(bucket_to_string bucket) ?options ~config
            ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_bucket_cors (fun () ->
          Raw.Cors.delete conn ~bucket:(bucket_to_string bucket) ?options ())
  end

  module Public_access_block = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_public_access_block (fun () ->
          Raw.Public_access_block.get conn ~bucket:(bucket_to_string bucket)
            ?options ())

    let put conn ~bucket ?options ~config () =
      observe conn ~operation:Operation.Put_public_access_block (fun () ->
          Raw.Public_access_block.put conn ~bucket:(bucket_to_string bucket)
            ?options ~config ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_public_access_block (fun () ->
          Raw.Public_access_block.delete conn ~bucket:(bucket_to_string bucket)
            ?options ())
  end

  module Ownership_controls = struct
    let get conn ~bucket ?options () =
      observe conn ~operation:Operation.Get_bucket_ownership_controls (fun () ->
          Raw.Ownership_controls.get conn ~bucket:(bucket_to_string bucket)
            ?options ())

    let put conn ~bucket ?options ~config () =
      observe conn ~operation:Operation.Put_bucket_ownership_controls (fun () ->
          Raw.Ownership_controls.put conn ~bucket:(bucket_to_string bucket)
            ?options ~config ())

    let delete conn ~bucket ?options () =
      observe conn ~operation:Operation.Delete_bucket_ownership_controls
        (fun () ->
          Raw.Ownership_controls.delete conn ~bucket:(bucket_to_string bucket)
            ?options ())
  end
end

module Multipart = struct
  type connection = t
  type 'a io = 'a
  type request_body = Body.t

  module Raw = Simulator_multipart.Multipart

  let create_upload conn ~bucket ~key ?options () =
    observe conn ~operation:Operation.Create_multipart_upload (fun () ->
        Raw.create_upload conn ~bucket:(bucket_to_string bucket)
          ~key:(key_to_string key) ?options ())

  let upload_part conn ~upload ~part_number ~body ?options () =
    observe conn ~operation:Operation.Upload_part
      ?logical_request_bytes:(Body.content_length body) (fun () ->
        Raw.upload_part conn ~upload ~part_number ~body ?options ())

  let complete_upload conn ~upload ?options ~parts () =
    observe conn ~operation:Operation.Complete_multipart_upload (fun () ->
        Raw.complete_upload conn ~upload ?options ~parts ())

  let abort_upload conn ~upload ?options () =
    observe conn ~operation:Operation.Abort_multipart_upload (fun () ->
        Raw.abort_upload conn ~upload ?options ())

  let list_parts conn ~upload ?options () =
    observe conn ~operation:Operation.List_parts (fun () ->
        Raw.list_parts conn ~upload ?options ())

  module List_parts = struct
    let fetch_page conn ~upload ~options () =
      list_parts conn ~upload ~options ()

    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      Raw.List_parts.fold_pages_with ~fetch:fetch_page conn ~upload ?options
        ?max_pages ~init ~f ()

    let pages conn ~upload ?options ?max_pages () =
      Raw.List_parts.pages_with ~fetch:fetch_page conn ~upload ?options
        ?max_pages ()

    let parts conn ~upload ?options ?max_pages () =
      Raw.List_parts.parts_with ~fetch:fetch_page conn ~upload ?options
        ?max_pages ()
  end
end

module Presigned = Simulator_presigned.Presigned
