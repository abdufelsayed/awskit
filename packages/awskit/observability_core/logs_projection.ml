open Base

let level_rank = function
  | Logs.App -> 0
  | Error -> 1
  | Warning -> 2
  | Info -> 3
  | Debug -> 4

let level_enabled source level =
  match Logs.Src.level source with
  | None -> false
  | Some threshold -> level_rank level <= level_rank threshold

let any_level_enabled source levels =
  List.exists levels ~f:(level_enabled source)

let operation_enabled definition =
  let source = definition |> Definition.Operation.info |> Value.Info.source in
  definition
  |> Definition.Operation.log
  |> Definition.Log.operation_levels
  |> any_level_enabled source

let event_enabled definition =
  let source = definition |> Definition.Event.info |> Value.Event.Info.source in
  definition
  |> Definition.Event.log
  |> Definition.Log.event_levels
  |> any_level_enabled source

let operation_completion_tag =
  Logs.Tag.def "awskit.operation.completion" Value.Operation.Completion.pp
    ~doc:"Safe completed Awskit operation"

let event_tag =
  Logs.Tag.def "awskit.event" Value.Event.pp ~doc:"Safe Awskit event"

let decide_operation definition completion =
  let policy = Definition.Operation.log definition in
  let decision = Definition.Log.decide_operation policy completion in
  (match decision with
  | Definition.Log.Skip -> ()
  | Definition.Log.Emit { level; _ } ->
      if
        not
          (Definition.Log.declares
             (Definition.Log.operation_levels policy)
             level)
      then invalid_arg "Awskit operation log policy emitted an undeclared level");
  decision

let emit_operation decision completion =
  match decision with
  | Definition.Log.Skip -> ()
  | Definition.Log.Emit { level; message } ->
      let source =
        completion |> Value.Operation.Completion.info |> Value.Info.source
      in
      let module Log = (val Logs.src_log source : Logs.LOG) in
      Log.msg level @@ fun log ->
      log
        ~tags:(Logs.Tag.add operation_completion_tag completion Logs.Tag.empty)
        "%s" (Lazy.force message)

let decide_event definition payload =
  let policy = Definition.Event.log definition in
  let decision = Definition.Log.decide_event policy payload in
  (match decision with
  | Definition.Log.Skip -> ()
  | Definition.Log.Emit { level; _ } ->
      if
        not (Definition.Log.declares (Definition.Log.event_levels policy) level)
      then invalid_arg "Awskit event log policy emitted an undeclared level");
  decision

let emit_event decision event =
  match decision with
  | Definition.Log.Skip -> ()
  | Definition.Log.Emit { level; message } ->
      let source = event |> Value.Event.info |> Value.Event.Info.source in
      let module Log = (val Logs.src_log source : Logs.LOG) in
      Log.msg level @@ fun log ->
      log
        ~tags:(Logs.Tag.add event_tag event Logs.Tag.empty)
        "%s" (Lazy.force message)
