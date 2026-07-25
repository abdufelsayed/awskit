module Fields = Observability_core.Fields
module Metric = Observability_core.Metric
module Value = Observability_core.Value
module Definition = Observability_core.Definition
module Engine = Observability_core.Engine
open Base
module Outcome = Fields.Outcome
module Span_kind = Fields.Span_kind

module Sources = struct
  let http = Observability_core.Http.source
  let credentials = Observability_core.Credentials_observation.source
end

module Diagnostic = struct
  module Public = struct
    type value = Fields.Diagnostic.value =
      | String of string
      | Bool of bool
      | Int of int
      | Int64 of int64
      | Float of float

    type t = Fields.Diagnostic.Public.t

    let name = Fields.Diagnostic.Public.name
    let value = Fields.Diagnostic.Public.value
    let pp = Fields.Diagnostic.Public.pp
  end
end

module Correlation = Fields.Correlation
module Health = Observability_core.Health

module For_projection = struct
  module Dimension = Fields.Dimension
  module Measurement = Fields.Measurement

  module Operation = struct
    module Info = Value.Info
    module Start = Value.Operation.Start
    module Completion = Value.Operation.Completion
  end

  module Event = Value.Event

  module Metric = struct
    module Label = Metric.Label

    module Family = struct
      type t = Metric.Family.packed

      type aggregation = Metric.Family.aggregation =
        | Counter
        | Histogram
        | Gauge

      type number = Metric.Family.number = Int | Int64 | Float

      let id = Metric.Family.id
      let name = Metric.Family.name
      let doc = Metric.Family.doc
      let unit_ = Metric.Family.unit_
      let aggregation = Metric.Family.aggregation
      let number = Metric.Family.number

      let labels family =
        List.map (Metric.Family.labels family) ~f:(fun (name, allowed_values) ->
            Metric.Label.create ~name ~allowed_values)

      let equal = Metric.Family.equal
    end

    module Value = Metric.Value
    module Observation = Metric.Observation
  end
end

module Metric_sink = Metric.Sink

module Logs_tags = struct
  let operation_completion =
    Observability_core.Logs_projection.operation_completion_tag

  let event = Observability_core.Logs_projection.event_tag
end

module For_service = struct
  type ('start, 'finish) operation_completion =
    ('start, 'finish) Definition.operation_completion

  module Dimension = Fields.Dimension
  module Measurement = Fields.Measurement
  module Diagnostic = Fields.Diagnostic
  module Fields = Fields.Fields
  module Terminal = Definition.Terminal

  module Metric = struct
    type counter = Metric.counter
    type histogram = Metric.histogram
    type gauge = Metric.gauge

    module Number = Metric.Number
    module Labels = Metric.Labels
    module Family = Metric.Family
    module Projection = Metric.Projection
  end

  module Log = Definition.Log
  module Operation = Definition.Operation
  module Event = Definition.Event
  module Instrument = Definition.Instrument
  module Credential_resolution = Observability_core.Credentials_observation

  module type Observer = sig
    type +'a io
    type connection
    type lease

    val with_operation :
      connection ->
      operation:(unit -> ('start, 'result, 'finish) Operation.t) ->
      start:(unit -> 'start) ->
      (unit -> 'result io) ->
      'result io

    val emit_event :
      connection -> 'payload Event.t -> data:(unit -> 'payload) -> unit

    val acquire :
      connection ->
      'labels Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      lease

    val add : lease -> int64 -> unit
    val release : lease -> unit

    val with_instrument :
      connection ->
      'labels Instrument.t ->
      labels:(unit -> 'labels) ->
      int64 ->
      (unit -> 'a io) ->
      'a io
  end
end

module For_runtime = struct
  module type Context = sig
    type +'a io
    type 'a key

    val create : unit -> 'a key
    val get : 'a key -> 'a option
    val with_binding : 'a key -> 'a -> (unit -> 'b io) -> 'b io
    val bind : 'a io -> ('a -> 'b io) -> 'b io
    val return : 'a -> 'a io
    val fail : exn -> 'a io
    val capture : (unit -> 'a io) -> ('a, exn) Result.t io
    val finalize : (unit -> 'a io) -> (('a, exn) Result.t -> unit) -> 'a io
    val raised_outcome : exn -> Outcome.t
  end

  module type Trace_sink = sig
    type +'a io
    type t
    type activation

    val name : t -> string
    val needs_clock : t -> bool
    val enabled : t -> For_projection.Operation.Info.t -> bool
    val start : t -> For_projection.Operation.Start.t -> activation
    val correlation : activation -> Diagnostic.Public.t list
    val within : activation -> (unit -> 'a io) -> 'a io
    val finish : activation -> For_projection.Operation.Completion.t -> unit
    val event_enabled : t -> For_projection.Event.Info.t -> bool
    val event : t -> For_projection.Event.t -> unit
  end

  module Make
      (Context : Context)
      (Trace_sink : Trace_sink with type 'a io = 'a Context.io) =
  struct
    module Engine_trace_sink = struct
      type t = Trace_sink.t
      type activation = Trace_sink.activation

      let name = Trace_sink.name
      let needs_clock = Trace_sink.needs_clock
      let enabled = Trace_sink.enabled
      let start = Trace_sink.start
      let correlation = Trace_sink.correlation
      let finish = Trace_sink.finish
      let event_enabled = Trace_sink.event_enabled
      let event = Trace_sink.event
    end

    module Engine = Observability_core.Engine.Make (Engine_trace_sink)

    type lease = Engine.lease

    type t = {
      engine : Engine.t;
      scope_key : Engine.scope Context.key option;
      hard_off : bool;
    }

    let none = { engine = Engine.none; scope_key = None; hard_off = true }

    let create ?(logs = true) ?clock ?(metric_sinks = []) ?(trace_sinks = []) ()
        =
      let engine = Engine.create ~logs ?clock ~metric_sinks ~trace_sinks () in
      { engine; scope_key = Some (Context.create ()); hard_off = false }

    let config = create
    let default () = create ()
    let health t = Engine.health t.engine
    let snapshot t = Engine.instrument_snapshot t.engine

    let current_scope t =
      match t.scope_key with
      | None -> Engine.root_scope
      | Some key -> Option.value (Context.get key) ~default:Engine.root_scope

    let result_to_io = function
      | Ok value -> Context.return value
      | Error exn -> Context.fail exn

    let physically_equal left right = phys_equal left right

    let trace_within t operation activation next =
      let called = ref false in
      let repeated = ref false in
      let callback_result = ref None in
      let callback () =
        if !called then repeated := true else called := true;
        match !callback_result with
        | Some result -> Context.bind result result_to_io
        | None ->
            let result = Context.capture next in
            callback_result := Some result;
            Context.bind result result_to_io
      in
      let wrapped_result =
        Context.capture (fun () ->
            Trace_sink.within (Engine.trace_activation activation) callback)
      in
      let finish_result wrapped =
        match !callback_result with
        | None ->
            Engine.context_failure operation activation;
            let result = callback () in
            Context.bind (Context.capture (fun () -> result)) result_to_io
        | Some real_result ->
            let check_violation real wrapped =
              let mismatch =
                match (real, wrapped) with
                | Ok real, Ok wrapped -> not (physically_equal real wrapped)
                | Ok _, Error _ | Error _, Ok _ -> true
                | Error real, Error wrapped ->
                    not (physically_equal real wrapped)
              in
              if !repeated || mismatch then
                Engine.context_failure operation activation
            in
            Context.bind real_result (fun real ->
                check_violation real wrapped;
                result_to_io real)
      in
      Context.bind wrapped_result finish_result

    let with_operation t ~operation:make_definition ~start callback =
      if t.hard_off then callback ()
      else
        let parent = current_scope t in
        match
          Engine.prepare t.engine ~parent ~operation:make_definition ~start
        with
        | None -> callback ()
        | Some operation ->
            let run () =
              let run_callback () =
                List.fold_right (Engine.trace_activations operation)
                  ~init:callback ~f:(fun activation next ->
                    fun () -> trace_within t operation activation next)
                |> fun run -> run ()
              in
              match t.scope_key with
              | None -> run_callback ()
              | Some key ->
                  Context.with_binding key (Engine.scope operation) run_callback
            in
            Context.finalize run (fun result ->
                Engine.finish operation ~raised_outcome:Context.raised_outcome
                  result)

    let emit_event t definition ~data =
      if not t.hard_off then
        Engine.emit_event t.engine ~current:(current_scope t) definition ~data

    let acquire t instrument ~labels initial =
      if t.hard_off then (Engine.Disabled : lease)
      else Engine.acquire t.engine instrument ~labels initial

    let add = Engine.add
    let release = Engine.release

    let with_instrument t instrument ~labels initial callback =
      if t.hard_off then callback ()
      else
        let lease = acquire t instrument ~labels initial in
        Context.finalize callback (fun _ -> release lease)
  end

  module Http = struct
    type replayability = Observability_core.Http.replayability =
      | Replayable
      | Non_replayable

    type request_start = Observability_core.Http.request_start
    type request_finish = Observability_core.Http.request_finish
    type response = Observability_core.Http.response
    type request_stats = Observability_core.Http.request_stats
    type phase_start = Observability_core.Http.phase_start
    type phase_finish = Observability_core.Http.phase_finish
    type request_state = Observability_core.Http.method_labels

    type streaming_direction = Observability_core.Http.streaming_direction =
      | Request
      | Response

    type streaming_state = Observability_core.Http.streaming_labels

    let request_start = Observability_core.Http.request_start
    let response = Observability_core.Http.response
    let request_stats = Observability_core.Http.request_stats
    let request = Observability_core.Http.request
    let phase_start = Observability_core.Http.phase_start

    let request_body_production =
      Observability_core.Http.request_body_production

    let response_headers_wait = Observability_core.Http.response_headers_wait

    let response_body_consumption =
      Observability_core.Http.response_body_consumption

    let response_body_drain = Observability_core.Http.response_body_drain
    let attempts_in_flight = Observability_core.Http.attempts_in_flight
    let request_state ~method_ = Observability_core.Http.state_labels method_

    let streaming_bytes_in_flight =
      Observability_core.Http.streaming_bytes_in_flight

    let streaming_state direction =
      Observability_core.Http.streaming_state_labels direction
  end
end
