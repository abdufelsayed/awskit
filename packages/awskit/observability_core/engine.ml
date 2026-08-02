module Logging = Logs_projection
open Base

module type Trace_sink = sig
  type t
  type activation

  val name : t -> string
  val needs_clock : t -> bool
  val enabled : t -> Value.Info.t -> bool
  val start : t -> Value.Operation.Start.t -> activation
  val correlation : activation -> Fields.Diagnostic.Public.t list
  val finish : activation -> Value.Operation.Completion.t -> unit
  val event_enabled : t -> Value.Event.Info.t -> bool
  val event : t -> Value.Event.t -> unit
end

module Make (Trace_sink : Trace_sink) = struct
  type scope = Value.Scope.t

  type metric_slot = {
    sink : Metric.Sink.t;
    health_projection : Health.Projection.t;
  }

  type trace_slot = {
    sink : Trace_sink.t;
    health_projection : Health.Projection.t;
  }

  type gauge_key = { family_id : int; labels : string list }

  type gauge_entry = {
    key : gauge_key;
    value : int64 Atomic.t;
    observation : int64 -> Metric.Observation.t;
  }

  type t = {
    hard_off : bool;
    logs : bool;
    clock : (unit -> int64) option;
    metric_slots : metric_slot list;
    trace_slots : trace_slot list;
    logs_projection : Health.Projection.t option;
    engine_projection : Health.Projection.t option;
    health_state : Health.t;
    gauge_entries : gauge_entry list Atomic.t;
  }

  type trace_activation = {
    slot : trace_slot;
    activation : Trace_sink.activation;
  }

  type ('start, 'finish) metric_activation = {
    projection :
      ('start, 'finish) Definition.Operation.Completion.t Metric.Projection.t;
    slots : metric_slot list;
  }

  type ('result, 'finish) operation =
    | Operation : {
        observer : t;
        parent : Value.Scope.t;
        definition : ('start, 'result, 'finish) Definition.Operation.t;
        start_payload : 'start;
        start_fields : Fields.Fields.t;
        started_ns : int64 option;
        logs_enabled : bool;
        metric_activations : ('start, 'finish) metric_activation list;
        trace_activations : trace_activation list;
        correlation : Fields.Diagnostic.Public.t list;
        scope : Value.Scope.t;
        finished : bool Atomic.t;
      }
        -> ('result, 'finish) operation

  type lease_state = Active of int64 | Released
  type lease_data = { entry : gauge_entry; state : lease_state Atomic.t }
  type lease = Disabled | Lease of lease_data

  let projections ~(logs : Health.Projection.t option)
      (metric_slots : metric_slot list) (trace_slots : trace_slot list)
      (engine_projection : Health.Projection.t option) =
    Option.to_list logs
    @ List.map metric_slots ~f:(fun slot -> slot.health_projection)
    @ List.map trace_slots ~f:(fun slot -> slot.health_projection)
    @ Option.to_list engine_projection

  let create ?(logs = true) ?clock ?(metric_sinks = []) ?(trace_sinks = []) () =
    let sink_requires_clock =
      List.exists metric_sinks ~f:Metric.Sink.needs_clock
      || List.exists trace_sinks ~f:Trace_sink.needs_clock
    in
    if sink_requires_clock && Option.is_none clock then
      invalid_arg
        "Awskit observability sink requires an explicit monotonic clock";
    let next_projection_id = ref 0 in
    let projection ~kind ~name =
      let id = !next_projection_id in
      Int.incr next_projection_id;
      Health.Projection.create ~id ~kind ~name
    in
    let logs_projection =
      Option.some_if logs (projection ~kind:Logs ~name:"logs")
    in
    let metric_slots : metric_slot list =
      List.map metric_sinks ~f:(fun (sink : Metric.Sink.t) ->
          ({
             sink;
             health_projection =
               projection ~kind:Metric ~name:(Metric.Sink.name sink);
           }
            : metric_slot))
    in
    let trace_slots : trace_slot list =
      List.map trace_sinks ~f:(fun (sink : Trace_sink.t) ->
          ({
             sink;
             health_projection =
               projection ~kind:Trace ~name:(Trace_sink.name sink);
           }
            : trace_slot))
    in
    let engine_projection = Some (projection ~kind:Engine ~name:"engine") in
    let health_state =
      Health.create
        (projections ~logs:logs_projection metric_slots trace_slots
           engine_projection)
    in
    {
      hard_off = false;
      logs;
      clock;
      metric_slots;
      trace_slots;
      logs_projection;
      engine_projection;
      health_state;
      gauge_entries = Atomic.make [];
    }

  let default () = create ()

  let none =
    {
      hard_off = true;
      logs = false;
      clock = None;
      metric_slots = [];
      trace_slots = [];
      logs_projection = None;
      engine_projection = None;
      health_state = Health.empty;
      gauge_entries = Atomic.make [];
    }

  let health t = Health.snapshot t.health_state
  let root_scope = Value.Scope.root
  let bump t projection phase = Health.bump t.health_state projection phase

  let bump_engine t phase =
    Option.iter t.engine_projection ~f:(fun projection ->
        bump t projection phase)

  let protect t projection phase ~default callback =
    try callback ()
    with _ ->
      bump t projection phase;
      default

  let protect_engine t phase ~default callback =
    match t.engine_projection with
    | None -> default
    | Some projection -> protect t projection phase ~default callback

  let logs_operation_enabled t definition =
    if not t.logs then false
    else
      match t.logs_projection with
      | None -> false
      | Some projection ->
          protect t projection Enablement ~default:false (fun () ->
              Logging.operation_enabled definition)

  let logs_event_enabled t definition =
    if not t.logs then false
    else
      match t.logs_projection with
      | None -> false
      | Some projection ->
          protect t projection Enablement ~default:false (fun () ->
              Logging.event_enabled definition)

  let enabled_metric_slots t family =
    List.filter t.metric_slots ~f:(fun slot ->
        protect t slot.health_projection Enablement ~default:false (fun () ->
            Metric.Sink.enabled slot.sink family))

  let metric_activations t projections =
    List.filter_map projections ~f:(fun projection ->
        let slots =
          enabled_metric_slots t (Metric.Projection.family projection)
        in
        Option.some_if (not (List.is_empty slots)) { projection; slots })

  let enabled_trace_slots t info =
    List.filter t.trace_slots ~f:(fun slot ->
        protect t slot.health_projection Enablement ~default:false (fun () ->
            Trace_sink.enabled slot.sink info))

  let start_clock t =
    match t.clock with
    | None -> None
    | Some clock ->
        protect_engine t Start ~default:None (fun () -> Some (clock ()))

  let needs_clock t ~logs_enabled metric_activations trace_slots =
    (Option.is_some t.clock && logs_enabled)
    || List.exists metric_activations ~f:(fun activation ->
        Metric.Projection.needs_duration activation.projection
        || List.exists activation.slots ~f:(fun slot ->
            Metric.Sink.needs_clock slot.sink))
    || List.exists trace_slots ~f:(fun slot -> Trace_sink.needs_clock slot.sink)

  let start_trace t start_view slot =
    protect t slot.health_projection Start ~default:None (fun () ->
        Some { slot; activation = Trace_sink.start slot.sink start_view })

  let trace_correlation t trace_activation =
    protect t trace_activation.slot.health_projection Context ~default:[]
      (fun () -> Trace_sink.correlation trace_activation.activation)

  let prepare t ~parent ~operation:make_definition ~start =
    if t.hard_off then None
    else
      match
        protect_engine t Enablement ~default:None (fun () ->
            Some (make_definition ()))
      with
      | None -> None
      | Some definition -> (
          let logs_enabled = logs_operation_enabled t definition in
          let metric_activations =
            metric_activations t (Definition.Operation.metrics definition)
          in
          let info = Definition.Operation.info definition in
          let trace_slots = enabled_trace_slots t info in
          if
            (not logs_enabled)
            && List.is_empty metric_activations
            && List.is_empty trace_slots
          then None
          else
            match
              protect_engine t Start ~default:None (fun () -> Some (start ()))
            with
            | None -> None
            | Some start_payload -> (
                match
                  protect_engine t Start ~default:None (fun () ->
                      Some
                        (Definition.Operation.encode_start definition
                           start_payload))
                with
                | None -> None
                | Some start_fields ->
                    let started_ns =
                      if
                        needs_clock t ~logs_enabled metric_activations
                          trace_slots
                      then start_clock t
                      else None
                    in
                    let start_view =
                      Value.Operation.Start.create ~info ~fields:start_fields
                    in
                    let trace_activations =
                      List.filter_map trace_slots ~f:(start_trace t start_view)
                    in
                    let correlation =
                      List.concat_map trace_activations ~f:(trace_correlation t)
                    in
                    let scope =
                      Value.Scope.of_start ~parent start_view ~correlation
                    in
                    Some
                      (Operation
                         {
                           observer = t;
                           parent;
                           definition;
                           start_payload;
                           start_fields;
                           started_ns;
                           logs_enabled;
                           metric_activations;
                           trace_activations;
                           correlation;
                           scope;
                           finished = Atomic.make false;
                         })))

  let scope (Operation operation) = operation.scope
  let trace_activations (Operation operation) = operation.trace_activations
  let trace_activation activation = activation.activation

  let context_failure (Operation operation) activation =
    bump operation.observer activation.slot.health_projection Context

  let duration t started_ns =
    match (t.clock, started_ns) with
    | Some clock, Some started ->
        let stopped =
          protect_engine t Finish ~default:started (fun () -> clock ())
        in
        Some
          (if Int64.(stopped < started) then 0L else Int64.(stopped - started))
    | None, _ | _, None -> None

  let classify definition terminal =
    Definition.Operation.classify definition terminal

  let finish (Operation operation) ~raised_outcome result =
    let t = operation.observer in
    if Atomic.compare_and_set operation.finished false true then (
      let default_outcome =
        match result with
        | Ok _ -> Fields.Outcome.Ok
        | Error exn ->
            protect_engine t Finish ~default:Fields.Outcome.Exception (fun () ->
                raised_outcome exn)
      in
      let terminal = Definition.Terminal.create ~result ~default_outcome in
      let classified =
        protect_engine t Finish ~default:None (fun () ->
            Some (classify operation.definition terminal))
      in
      let outcome, finish_payload =
        match classified with
        | None -> (default_outcome, None)
        | Some (outcome, finish_payload) -> (outcome, Some finish_payload)
      in
      let duration_ns = duration t operation.started_ns in
      let typed_completion =
        Definition.Operation.completion ~start:operation.start_payload
          ~finish:finish_payload ~outcome ~duration_ns
      in
      let log_decision =
        if operation.logs_enabled then
          Option.value_map t.logs_projection ~default:Definition.Log.Skip
            ~f:(fun projection ->
              protect t projection Finish ~default:Definition.Log.Skip
                (fun () ->
                  Logging.decide_operation operation.definition typed_completion))
        else Definition.Log.Skip
      in
      let needs_public_completion =
        (not (List.is_empty operation.trace_activations))
        ||
        match log_decision with
        | Definition.Log.Skip -> false
        | Definition.Log.Emit _ -> true
      in
      let public_completion =
        if not needs_public_completion then None
        else
          let finish_fields =
            Option.value_map finish_payload ~default:Fields.Fields.empty
              ~f:(fun finish_payload ->
                protect_engine t Finish ~default:Fields.Fields.empty (fun () ->
                    Definition.Operation.encode_finish operation.definition
                      finish_payload))
          in
          Some
            (Value.Operation.Completion.create
               ~info:(Definition.Operation.info operation.definition)
               ~outcome ~duration_ns ~start:operation.start_fields
               ~finish:finish_fields
               ~inherited:(Value.Scope.fields operation.parent)
               ~correlation:operation.correlation)
      in
      List.iter operation.metric_activations ~f:(fun activation ->
          match
            protect_engine t Finish ~default:None (fun () ->
                Metric.Projection.observe activation.projection typed_completion)
          with
          | None -> ()
          | Some observation ->
              List.iter activation.slots ~f:(fun slot ->
                  protect t slot.health_projection Finish ~default:() (fun () ->
                      Metric.Sink.observe slot.sink observation)));
      Option.iter public_completion ~f:(fun completion ->
          List.iter operation.trace_activations ~f:(fun activation ->
              protect t activation.slot.health_projection Finish ~default:()
                (fun () -> Trace_sink.finish activation.activation completion));
          match log_decision with
          | Definition.Log.Skip -> ()
          | Definition.Log.Emit _ ->
              Option.iter t.logs_projection ~f:(fun projection ->
                  protect t projection Finish ~default:() (fun () ->
                      Logging.emit_operation log_decision completion))))
    else bump_engine t Finish

  let emit_event t ~current definition ~data =
    if not t.hard_off then
      let logs_enabled = logs_event_enabled t definition in
      let metric_activations =
        List.filter_map (Definition.Event.metrics definition)
          ~f:(fun projection ->
            let slots =
              enabled_metric_slots t (Metric.Projection.family projection)
            in
            Option.some_if (not (List.is_empty slots)) (projection, slots))
      in
      let info = Definition.Event.info definition in
      let trace_slots =
        List.filter t.trace_slots ~f:(fun slot ->
            protect t slot.health_projection Enablement ~default:false
              (fun () -> Trace_sink.event_enabled slot.sink info))
      in
      if
        logs_enabled
        || (not (List.is_empty metric_activations))
        || not (List.is_empty trace_slots)
      then
        match
          protect_engine t Event ~default:None (fun () -> Some (data ()))
        with
        | None -> ()
        | Some payload ->
            let log_decision =
              if logs_enabled then
                Option.value_map t.logs_projection ~default:Definition.Log.Skip
                  ~f:(fun projection ->
                    protect t projection Event ~default:Definition.Log.Skip
                      (fun () -> Logging.decide_event definition payload))
              else Definition.Log.Skip
            in
            let needs_public_event =
              (not (List.is_empty trace_slots))
              ||
              match log_decision with
              | Definition.Log.Skip -> false
              | Definition.Log.Emit _ -> true
            in
            let public_event =
              if not needs_public_event then None
              else
                match
                  protect_engine t Event ~default:None (fun () ->
                      Some (Definition.Event.fields definition payload))
                with
                | None -> None
                | Some fields ->
                    Some
                      (Value.Event.create ~info ~fields
                         ~inherited:(Value.Scope.fields current)
                         ~correlation:[])
            in
            List.iter metric_activations ~f:(fun (projection, slots) ->
                match
                  protect_engine t Event ~default:None (fun () ->
                      Metric.Projection.observe projection payload)
                with
                | None -> ()
                | Some observation ->
                    List.iter slots ~f:(fun slot ->
                        protect t slot.health_projection Event ~default:()
                          (fun () -> Metric.Sink.observe slot.sink observation)));
            Option.iter public_event ~f:(fun event ->
                List.iter trace_slots ~f:(fun slot ->
                    protect t slot.health_projection Event ~default:()
                      (fun () -> Trace_sink.event slot.sink event));
                match log_decision with
                | Definition.Log.Skip -> ()
                | Definition.Log.Emit _ ->
                    Option.iter t.logs_projection ~f:(fun projection ->
                        protect t projection Event ~default:() (fun () ->
                            Logging.emit_event log_decision event)))

  let equal_gauge_key left right =
    Int.equal left.family_id right.family_id
    && List.equal String.equal left.labels right.labels

  let rec find_or_add_gauge_entry t key ~observation =
    let entries = Atomic.get t.gauge_entries in
    match List.find entries ~f:(fun entry -> equal_gauge_key entry.key key) with
    | Some entry -> entry
    | None ->
        let entry = { key; value = Atomic.make 0L; observation } in
        if Atomic.compare_and_set t.gauge_entries entries (entry :: entries)
        then entry
        else find_or_add_gauge_entry t key ~observation

  let rec add_atomic_int64 value delta =
    let current = Atomic.get value in
    let updated = Int64.(current + delta) in
    if Atomic.compare_and_set value current updated then updated
    else add_atomic_int64 value delta

  let acquire t instrument ~labels initial =
    if t.hard_off then Disabled
    else
      let family = Definition.Instrument.family instrument in
      let packed_family = Metric.Family.pack family in
      let slots = enabled_metric_slots t packed_family in
      if List.is_empty slots then Disabled
      else
        match
          protect_engine t Instrument ~default:None (fun () -> Some (labels ()))
        with
        | None -> Disabled
        | Some labels -> (
            match
              protect_engine t Instrument ~default:None (fun () ->
                  Some (Metric.Observation.create family labels 0L))
            with
            | None -> Disabled
            | Some zero ->
                let key =
                  {
                    family_id = Metric.Family.id packed_family;
                    labels =
                      List.map
                        (Metric.Observation.labels zero)
                        ~f:Metric.Label.encoded;
                  }
                in
                let observation value =
                  Metric.Observation.create family labels value
                in
                let entry = find_or_add_gauge_entry t key ~observation in
                let lease = { entry; state = Atomic.make (Active initial) } in
                if not (Int64.equal initial 0L) then
                  ignore (add_atomic_int64 entry.value initial : int64);
                Lease lease)

  let rec add_lease lease delta =
    match Atomic.get lease.state with
    | Released ->
        invalid_arg "Awskit observability instrument lease is released"
    | Active current as state ->
        let updated = Active Int64.(current + delta) in
        if Atomic.compare_and_set lease.state state updated then
          if not (Int64.equal delta 0L) then
            ignore (add_atomic_int64 lease.entry.value delta : int64)
          else ()

  let add lease delta =
    match lease with Disabled -> () | Lease lease -> add_lease lease delta

  let rec release_lease lease =
    match Atomic.get lease.state with
    | Released -> ()
    | Active current as state ->
        if Atomic.compare_and_set lease.state state Released then
          if not (Int64.equal current 0L) then
            ignore
              (add_atomic_int64 lease.entry.value Int64.(neg current) : int64)
          else ()

  let release = function Disabled -> () | Lease lease -> release_lease lease

  let instrument_snapshot t =
    if t.hard_off then []
    else
      protect_engine t Instrument ~default:[] (fun () ->
          Atomic.get t.gauge_entries
          |> List.map ~f:(fun entry ->
              entry.observation (Atomic.get entry.value)))
end
