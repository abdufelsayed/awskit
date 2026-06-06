open Awskit_s3
module Clock = Simulator_state.Clock

type config = Simulator_state.config = { max_list_keys : int }

let default_config = Simulator_state.default_config

type store = Simulator_state.store

let create_store = Simulator_state.create_store

type t = Simulator_state.t

let connect = Simulator_state.connect
let store = Simulator_state.store

module Runtime = Simulator_runtime.Runtime

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
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

let object_metadata = Simulator_inspect.object_metadata
let keys = Simulator_inspect.keys
let history = Simulator_inspect.history
let clear_history = Simulator_inspect.clear_history
let objects_as_strings = Simulator_inspect.objects_as_strings

module Multipart = Simulator_multipart.Multipart

module Object = struct
  include Simulator_object.Object
end

module Bucket = Simulator_bucket.Bucket
module Presigned = Simulator_presigned.Presigned
