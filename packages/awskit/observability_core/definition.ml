open Base

module Terminal = struct
  type 'a t = {
    result : ('a, exn) Result.t;
    default_outcome : Fields.Outcome.t;
  }

  let create ~result ~default_outcome = { result; default_outcome }
  let result t = t.result
  let default_outcome t = t.default_outcome
end

type ('start, 'finish) operation_completion = {
  start : 'start;
  finish : 'finish option;
  outcome : Fields.Outcome.t;
  duration_ns : int64 option;
}

module Log = struct
  type decision =
    | Skip
    | Emit of { level : Logs.level; message : string Lazy.t }

  type ('start, 'finish) operation_policy = {
    levels : Logs.level list;
    decide : ('start, 'finish) operation_completion -> decision;
  }

  type 'payload event_policy = {
    levels : Logs.level list;
    decide : 'payload -> decision;
  }

  let validate_levels levels =
    if List.contains_dup levels ~compare:Poly.compare then
      invalid_arg "Awskit observability log policy has duplicate levels"

  let silent_operation : (_, _) operation_policy =
    { levels = []; decide = (fun _ -> Skip) }

  let operation ~levels ~decide : (_, _) operation_policy =
    validate_levels levels;
    { levels; decide }

  let silent_event : _ event_policy = { levels = []; decide = (fun _ -> Skip) }

  let event ~levels ~decide : _ event_policy =
    validate_levels levels;
    { levels; decide }

  let operation_levels (t : (_, _) operation_policy) = t.levels
  let decide_operation (t : (_, _) operation_policy) = t.decide
  let event_levels (t : _ event_policy) = t.levels
  let decide_event (t : _ event_policy) = t.decide
  let declares levels level = List.mem levels level ~equal:Poly.equal
end

module Operation = struct
  module Completion = struct
    type ('start, 'finish) t = ('start, 'finish) operation_completion

    let start t = t.start
    let finish t = t.finish
    let outcome t = t.outcome
    let duration_ns t = t.duration_ns
  end

  type ('start, 'result, 'finish) t = {
    info : Value.Info.t;
    encode_start : 'start -> Fields.Fields.t;
    classify : 'result Terminal.t -> Fields.Outcome.t * 'finish;
    encode_finish : 'finish -> Fields.Fields.t;
    log : ('start, 'finish) Log.operation_policy;
    metrics : ('start, 'finish) Completion.t Metric.Projection.t list;
  }

  let define ~name ~doc ~source ~span_kind ~start ~classify ~finish ~log
      ~metrics () =
    {
      info = Value.Info.create ~name ~doc ~source ~span_kind;
      encode_start = start;
      classify;
      encode_finish = finish;
      log;
      metrics;
    }

  let info t = t.info
  let encode_start t = t.encode_start
  let classify t = t.classify
  let encode_finish t = t.encode_finish
  let log t = t.log
  let metrics t = t.metrics

  let completion ~start ~finish ~outcome ~duration_ns =
    ({ start; finish; outcome; duration_ns } : (_, _) Completion.t)

  let project t ~start ~result ~default_outcome ~duration_ns =
    let terminal = Terminal.create ~result ~default_outcome in
    let outcome, finish = classify t terminal in
    Value.Operation.Completion.create ~info:(info t) ~outcome ~duration_ns
      ~start:(encode_start t start) ~finish:(encode_finish t finish)
      ~inherited:(Value.Scope.fields Value.Scope.root)
      ~correlation:[]
end

module Event = struct
  type 'payload t = {
    info : Value.Event.Info.t;
    fields : 'payload -> Fields.Fields.t;
    log : 'payload Log.event_policy;
    metrics : 'payload Metric.Projection.t list;
  }

  let define ~name ~doc ~source ~fields ~log ~metrics () =
    { info = Value.Event.Info.create ~name ~doc ~source; fields; log; metrics }

  let info t = t.info
  let fields t = t.fields
  let log t = t.log
  let metrics t = t.metrics
end

module Instrument = struct
  type 'labels t = { family : (Metric.gauge, 'labels, int64) Metric.Family.t }

  let define ~family = { family }
  let family t = t.family
end
