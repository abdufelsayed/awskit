open Base
module O = Awskit.Observability

module Trace_sink = struct
  type activation = {
    within : 'a. (unit -> 'a) -> 'a;
    correlation : O.Diagnostic.Public.t list;
    finish : O.For_projection.Operation.Completion.t -> unit;
  }

  type t = {
    name : string;
    needs_clock : bool;
    enabled : O.For_projection.Operation.Info.t -> bool;
    start : O.For_projection.Operation.Start.t -> activation;
    event_enabled : O.For_projection.Event.Info.t -> bool;
    event : O.For_projection.Event.t -> unit;
  }

  let create ~name ~needs_clock ~enabled ~start ~event_enabled ~event =
    if String.is_empty name then
      invalid_arg "Awskit_eio observability trace sink name must not be empty";
    { name; needs_clock; enabled; start; event_enabled; event }

  let name t = t.name
  let needs_clock t = t.needs_clock
  let enabled t = t.enabled
  let start t = t.start
  let correlation activation = activation.correlation
  let finish activation = activation.finish
  let event_enabled t = t.event_enabled
  let event t = t.event
end

module Context = struct
  type +'a io = 'a
  type 'a key = 'a Eio.Fiber.key

  let create () = Eio.Fiber.create_key ()
  let get key = Eio.Fiber.get key
  let with_binding = Eio.Fiber.with_binding
  let bind value callback = callback value
  let return value = value
  let fail exn = raise exn
  let capture callback = try Ok (callback ()) with exn -> Error exn

  let finalize callback hook =
    try
      let value = callback () in
      (try hook (Ok value) with _ -> ());
      value
    with exn ->
      (try hook (Error exn) with _ -> ());
      raise exn

  let raised_outcome = function
    | Eio.Cancel.Cancelled _ -> O.Outcome.Cancelled
    | _ -> O.Outcome.Exception
end

module Runtime_trace_sink = struct
  type +'a io = 'a Context.io
  type t = Trace_sink.t
  type activation = Trace_sink.activation

  let name = Trace_sink.name
  let needs_clock = Trace_sink.needs_clock
  let enabled = Trace_sink.enabled
  let start = Trace_sink.start
  let correlation = Trace_sink.correlation
  let within activation callback = activation.Trace_sink.within callback
  let finish activation completion = activation.Trace_sink.finish completion
  let event_enabled = Trace_sink.event_enabled
  let event = Trace_sink.event
end

module Runtime = O.For_runtime.Make (Context) (Runtime_trace_sink)

type t = Runtime.t
type lease = Runtime.lease

let default = Runtime.default
let none = Runtime.none
let create = Runtime.create
let health = Runtime.health
let snapshot = Runtime.snapshot
let instrument_snapshot = Runtime.snapshot
let with_operation = Runtime.with_operation
let emit_event = Runtime.emit_event
let acquire = Runtime.acquire
let add = Runtime.add
let release = Runtime.release
let with_instrument = Runtime.with_instrument
