module Public = struct
  module Clock = Sim_state.Clock

  type config = Sim_state.config = { max_list_keys : int }

  let default_config = Sim_state.default_config

  type store = Sim_state.store

  let create_store = Sim_state.create_store

  type t = Sim_state.t

  let connect = Sim_state.connect
  let store = Sim_state.store

  module Runtime = Sim_runtime.Runtime

  type fault = Sim_state.fault =
    | Slow_down
    | Internal_error
    | Connection_reset
    | Response_lost

  let inject_fault = Sim_inspect.inject_fault
  let inject_faults = Sim_inspect.inject_faults
  let clear_faults = Sim_inspect.clear_faults
  let enable_random_faults = Sim_inspect.enable_random_faults
  let disable_random_faults = Sim_inspect.disable_random_faults

  type operation_record = Sim_state.operation_record = {
    op : Sim_state.operation;
    bucket : string;
    key : string option;
    timestamp : Ptime.t;
    faulted : bool;
  }

  type object_metadata = Sim_inspect.object_metadata = {
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
  }

  let object_metadata = Sim_inspect.object_metadata
  let keys = Sim_inspect.keys
  let history = Sim_inspect.history
  let clear_history = Sim_inspect.clear_history
  let objects_as_strings = Sim_inspect.objects_as_strings

  module Multipart = Sim_multipart.Multipart

  module Object = struct
    include Sim_object.Object
  end

  module Bucket = Sim_bucket.Bucket
  module Presigned = Sim_presigned.Presigned
end
