module Aws_error = Error
module Aws_request = Request
open Base

let source =
  Logs.Src.create "awskit.http" ~doc:"Awskit physical HTTP operations"

type replayability = Fields.Dimension.replayability =
  | Replayable
  | Non_replayable

type request_start = {
  method_ : Aws_request.Method.t;
  replayability : replayability;
}

let request_start ~method_ ~replayability = { method_; replayability }

type response = {
  status : int;
  request_id : string option;
  host_id : string option;
}

let response ~status ?request_id ?host_id () = { status; request_id; host_id }

type request_stats = {
  connector_request_bytes : int64 option;
  connector_response_bytes : int64;
  connector_drained_bytes : int64;
}

let request_stats ~connector_request_bytes ~connector_response_bytes
    ~connector_drained_bytes =
  { connector_request_bytes; connector_response_bytes; connector_drained_bytes }

type request_finish = {
  response : response option;
  stats : request_stats;
  retry_class : Aws_error.retry_class option;
}

type attempt_labels = {
  method_ : Aws_request.Method.t;
  outcome : Fields.Outcome.t;
}

type method_labels = { method_ : Aws_request.Method.t }

type response_labels = {
  method_ : Aws_request.Method.t;
  status_class : Fields.Dimension.status_class;
}

let attempt_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add Fields.Dimension.http_method
       ~get:(fun (labels : attempt_labels) -> labels.method_)
  |> Metric.Labels.add Fields.Dimension.outcome
       ~get:(fun (labels : attempt_labels) -> labels.outcome)

let method_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add Fields.Dimension.http_method
       ~get:(fun (labels : method_labels) -> labels.method_)

let response_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add Fields.Dimension.http_method
       ~get:(fun (labels : response_labels) -> labels.method_)
  |> Metric.Labels.add Fields.Dimension.status_class
       ~get:(fun (labels : response_labels) -> labels.status_class)

let attempts =
  Metric.Family.counter ~name:"awskit.http.attempts"
    ~doc:"Completed physical HTTP attempts" ~labels:attempt_labels
    ~value:Metric.Number.Int64 ()

let attempt_duration =
  Metric.Family.histogram ~name:"awskit.http.attempt.duration"
    ~doc:"Physical HTTP attempt duration" ~unit_:"ns" ~labels:attempt_labels
    ~value:Metric.Number.Int64 ()

let responses =
  Metric.Family.counter ~name:"awskit.http.responses"
    ~doc:"HTTP responses grouped by status class" ~labels:response_labels
    ~value:Metric.Number.Int64 ()

let connector_request_bytes =
  Metric.Family.histogram ~name:"awskit.http.connector_request_bytes"
    ~doc:
      "Bytes handed to or pulled by the configured HTTP connector during one \
       physical attempt; these are not socket or wire bytes"
    ~unit_:"By" ~labels:method_labels ~value:Metric.Number.Int64 ()

let connector_response_bytes =
  Metric.Family.histogram ~name:"awskit.http.connector_response_bytes"
    ~doc:
      "Caller-consumed bytes pulled from the configured HTTP connector \
       response body during one physical attempt; these are not socket or wire \
       bytes"
    ~unit_:"By" ~labels:method_labels ~value:Metric.Number.Int64 ()

let connector_drained_bytes =
  Metric.Family.histogram ~name:"awskit.http.connector_drained_bytes"
    ~doc:
      "Response bytes pulled from the configured HTTP connector during cleanup \
       or explicit discard, separate from caller consumption; these are not \
       socket or wire bytes"
    ~unit_:"By" ~labels:method_labels ~value:Metric.Number.Int64 ()

let attempts_in_flight_family =
  Metric.Family.gauge ~name:"awskit.http.attempts_in_flight"
    ~doc:"Physical HTTP attempts currently in flight" ~labels:method_labels
    ~value:Metric.Number.Int64 ()

let attempts_in_flight =
  Definition.Instrument.define ~family:attempts_in_flight_family

let state_labels method_ : method_labels = { method_ }

type streaming_direction = Request | Response

let streaming_direction =
  Fields.Dimension.Enum.define ~name:"direction" ~equal:Poly.equal
    ~values:[ (Request, "request"); (Response, "response") ]

type streaming_labels = { direction : streaming_direction }

let streaming_labels =
  Metric.Labels.empty ()
  |> Metric.Labels.add streaming_direction
       ~get:(fun (labels : streaming_labels) -> labels.direction)

let streaming_bytes_family =
  Metric.Family.gauge ~name:"awskit.http.streaming_bytes_in_flight"
    ~doc:"Bytes currently owned by HTTP streaming paths" ~unit_:"By"
    ~labels:streaming_labels ~value:Metric.Number.Int64 ()

let streaming_bytes_in_flight =
  Definition.Instrument.define ~family:streaming_bytes_family

let streaming_state_labels direction : streaming_labels = { direction }

let status_outcome status =
  if status >= 200 && status < 400 then Fields.Outcome.Ok
  else
    match status with
    | 404 -> Not_found
    | 409 -> Conflict
    | 429 -> Throttled
    | _ -> Error

let classify_request response stats terminal =
  let response = response () in
  let stats = stats () in
  match Definition.Terminal.result terminal with
  | Ok (Ok _) ->
      let outcome =
        Option.value_map response ~default:Fields.Outcome.Ok ~f:(fun response ->
            status_outcome response.status)
      in
      (outcome, { response; stats; retry_class = None })
  | Ok (Error aws_error) ->
      ( Fields.Outcome.of_error aws_error,
        {
          response;
          stats;
          retry_class = Some (Aws_error.retry_class aws_error);
        } )
  | Error _ ->
      ( Definition.Terminal.default_outcome terminal,
        { response; stats; retry_class = None } )

let request_start_fields (start : request_start) =
  Fields.Fields.create
    ~dimensions:
      [
        Fields.Dimension.Enum.value Fields.Dimension.http_method start.method_;
        Fields.Dimension.Enum.value Fields.Dimension.replayability
          start.replayability;
      ]
    ()

let request_finish_fields (finish : request_finish) =
  let response_dimensions =
    Option.to_list
      (Option.map finish.response ~f:(fun response ->
           Fields.Dimension.Enum.value Fields.Dimension.status_class
             (Fields.Dimension.status_class_of_code response.status)))
  in
  let retry_dimensions =
    Option.to_list
      (Option.map finish.retry_class ~f:(fun retry_class ->
           Fields.Dimension.Enum.value Fields.Dimension.retry_class retry_class))
  in
  let response_diagnostics =
    Option.value_map finish.response ~default:[] ~f:(fun response ->
        List.filter_opt
          [
            Fields.Diagnostic.http_status_code response.status;
            Option.bind response.request_id ~f:Fields.Diagnostic.aws_request_id;
            Option.bind response.host_id
              ~f:Fields.Diagnostic.aws_extended_request_id;
          ])
  in
  let request_measurements =
    Option.to_list
      (Option.map finish.stats.connector_request_bytes
         ~f:(fun connector_request_bytes ->
           Fields.Measurement.int64 ~unit_:"By"
             ~name:"http.connector_request_bytes" connector_request_bytes))
  in
  Fields.Fields.create
    ~dimensions:(response_dimensions @ retry_dimensions)
    ~measurements:
      (request_measurements
      @ [
          Fields.Measurement.int64 ~unit_:"By"
            ~name:"http.connector_response_bytes"
            finish.stats.connector_response_bytes;
          Fields.Measurement.int64 ~unit_:"By"
            ~name:"http.connector_drained_bytes"
            finish.stats.connector_drained_bytes;
        ])
    ~diagnostics:response_diagnostics ()

let completion_message
    (completion :
      (request_start, request_finish) Definition.Operation.Completion.t) =
  let method_ =
    completion |> Definition.Operation.Completion.start |> fun start ->
    Aws_request.Method.to_string start.method_
  in
  let outcome =
    completion
    |> Definition.Operation.Completion.outcome
    |> Fields.Outcome.to_string
  in
  lazy (Fmt.str "HTTP %s attempt finished with outcome %s" method_ outcome)

let request_log =
  Definition.Log.operation ~levels:[ Logs.Debug; Logs.Warning; Logs.Error ]
    ~decide:(fun
        (completion :
          (request_start, request_finish) Definition.Operation.Completion.t)
      ->
      match Definition.Operation.Completion.outcome completion with
      | Fields.Outcome.Ok -> Skip
      | Not_found | Conflict | Cancelled ->
          Emit { level = Logs.Debug; message = completion_message completion }
      | Throttled ->
          Emit { level = Logs.Warning; message = completion_message completion }
      | Error | Exception | Timeout ->
          Emit { level = Logs.Error; message = completion_message completion })

let request_metrics =
  let count
      (completion :
        (request_start, request_finish) Definition.Operation.Completion.t) =
    let start = Definition.Operation.Completion.start completion in
    Some
      ( {
          method_ = start.method_;
          outcome = Definition.Operation.Completion.outcome completion;
        },
        1L )
  in
  [
    Metric.Projection.sample attempts ~get:count;
    Metric.Projection.sample ~needs_duration:true attempt_duration
      ~get:(fun
          (completion :
            (request_start, request_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.map (Definition.Operation.Completion.duration_ns completion)
          ~f:(fun duration ->
            ( {
                method_ = start.method_;
                outcome = Definition.Operation.Completion.outcome completion;
              },
              duration )));
    Metric.Projection.sample responses
      ~get:(fun
          (completion :
            (request_start, request_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.bind (Definition.Operation.Completion.finish completion)
          ~f:(fun finish ->
            Option.map finish.response ~f:(fun response ->
                ( {
                    method_ = start.method_;
                    status_class =
                      Fields.Dimension.status_class_of_code response.status;
                  },
                  1L ))));
    Metric.Projection.sample connector_request_bytes
      ~get:(fun
          (completion :
            (request_start, request_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.bind (Definition.Operation.Completion.finish completion)
          ~f:(fun finish ->
            Option.map finish.stats.connector_request_bytes
              ~f:(fun connector_request_bytes ->
                ( ({ method_ = start.method_ } : method_labels),
                  connector_request_bytes ))));
    Metric.Projection.sample connector_response_bytes
      ~get:(fun
          (completion :
            (request_start, request_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.map (Definition.Operation.Completion.finish completion)
          ~f:(fun finish ->
            ( ({ method_ = start.method_ } : method_labels),
              finish.stats.connector_response_bytes )));
    Metric.Projection.sample connector_drained_bytes
      ~get:(fun
          (completion :
            (request_start, request_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.map (Definition.Operation.Completion.finish completion)
          ~f:(fun finish ->
            ( ({ method_ = start.method_ } : method_labels),
              finish.stats.connector_drained_bytes )));
  ]

let request ~response ~stats =
  Definition.Operation.define ~name:"awskit.http.attempt"
    ~doc:"One physical HTTP request attempt" ~source
    ~span_kind:Fields.Span_kind.Client ~start:request_start_fields
    ~classify:(classify_request response stats)
    ~finish:request_finish_fields ~log:request_log ~metrics:request_metrics ()

type phase_start = { method_ : Aws_request.Method.t }

let phase_start ~method_ = { method_ }

type phase_finish = {
  bytes : int64 option;
  retry_class : Aws_error.retry_class option;
}

type phase_labels = attempt_labels

let phase_labels = attempt_labels

let phase_start_fields (start : phase_start) =
  Fields.Fields.create
    ~dimensions:
      [ Fields.Dimension.Enum.value Fields.Dimension.http_method start.method_ ]
    ()

let phase_finish_fields ~measurement (finish : phase_finish) =
  Fields.Fields.create
    ~dimensions:
      (Option.to_list
         (Option.map finish.retry_class ~f:(fun retry_class ->
              Fields.Dimension.Enum.value Fields.Dimension.retry_class
                retry_class)))
    ~measurements:
      (Option.to_list
         (Option.map finish.bytes ~f:(fun bytes ->
              Fields.Measurement.int64 ~unit_:"By" ~name:measurement bytes)))
    ()

let phase_duration_family name doc =
  Metric.Family.histogram ~name ~doc ~unit_:"ns" ~labels:phase_labels
    ~value:Metric.Number.Int64 ()

let phase_bytes_family name doc =
  Metric.Family.histogram ~name ~doc ~unit_:"By" ~labels:method_labels
    ~value:Metric.Number.Int64 ()

let request_body_production_duration =
  phase_duration_family "awskit.http.request_body.production.duration"
    "HTTP request-body production duration"

let request_body_production_bytes =
  phase_bytes_family "awskit.http.request_body.production.bytes"
    "Bytes produced by an HTTP request body"

let response_headers_wait_duration =
  phase_duration_family "awskit.http.response_headers.wait.duration"
    "HTTP response-header wait duration"

let response_body_consumption_duration =
  phase_duration_family "awskit.http.response_body.consumption.duration"
    "HTTP response-body consumption duration"

let response_body_consumption_bytes =
  phase_bytes_family "awskit.http.response_body.consumption.bytes"
    "HTTP response bytes consumed by the caller"

let response_body_drain_duration =
  phase_duration_family "awskit.http.response_body.drain.duration"
    "HTTP response-body drain duration"

let response_body_drain_bytes =
  phase_bytes_family "awskit.http.response_body.drain.bytes"
    "HTTP response bytes drained by the runtime"

let classify_phase bytes terminal =
  let bytes = Option.map bytes ~f:(fun bytes -> bytes ()) in
  match Definition.Terminal.result terminal with
  | Ok (Ok _) -> (Fields.Outcome.Ok, { bytes; retry_class = None })
  | Ok (Error aws_error) ->
      ( Fields.Outcome.of_error aws_error,
        { bytes; retry_class = Some (Aws_error.retry_class aws_error) } )
  | Error _ ->
      ( Definition.Terminal.default_outcome terminal,
        { bytes; retry_class = None } )

let phase_metrics duration_family bytes_family =
  let duration =
    Metric.Projection.sample ~needs_duration:true duration_family
      ~get:(fun
          (completion :
            (phase_start, phase_finish) Definition.Operation.Completion.t)
        ->
        let start = Definition.Operation.Completion.start completion in
        Option.map (Definition.Operation.Completion.duration_ns completion)
          ~f:(fun duration ->
            ( {
                method_ = start.method_;
                outcome = Definition.Operation.Completion.outcome completion;
              },
              duration )))
  in
  let bytes =
    Option.map bytes_family ~f:(fun family ->
        Metric.Projection.sample family
          ~get:(fun
              (completion :
                (phase_start, phase_finish) Definition.Operation.Completion.t)
            ->
            let start = Definition.Operation.Completion.start completion in
            Option.bind (Definition.Operation.Completion.finish completion)
              ~f:(fun finish ->
                Option.map finish.bytes ~f:(fun bytes ->
                    (({ method_ = start.method_ } : method_labels), bytes)))))
  in
  duration :: Option.to_list bytes

let phase ~name ~doc ~measurement ~duration_family ?bytes_family bytes =
  Definition.Operation.define ~name ~doc ~source
    ~span_kind:Fields.Span_kind.Internal ~start:phase_start_fields
    ~classify:(classify_phase bytes)
    ~finish:(phase_finish_fields ~measurement)
    ~log:Definition.Log.silent_operation
    ~metrics:(phase_metrics duration_family bytes_family)
    ()

let request_body_production ~bytes =
  phase ~name:"awskit.http.request_body.production"
    ~doc:"Produce one physical HTTP request body"
    ~measurement:"http.request_body.bytes"
    ~duration_family:request_body_production_duration
    ~bytes_family:request_body_production_bytes (Some bytes)

let response_headers_wait () =
  phase ~name:"awskit.http.response_headers.wait"
    ~doc:"Wait for physical HTTP response headers"
    ~measurement:"http.response_headers.bytes"
    ~duration_family:response_headers_wait_duration None

let response_body_consumption ~bytes =
  phase ~name:"awskit.http.response_body.consumption"
    ~doc:"Consume an HTTP response body"
    ~measurement:"http.response_body.consumed_bytes"
    ~duration_family:response_body_consumption_duration
    ~bytes_family:response_body_consumption_bytes (Some bytes)

let response_body_drain ~bytes =
  phase ~name:"awskit.http.response_body.drain"
    ~doc:"Drain an HTTP response body"
    ~measurement:"http.response_body.drained_bytes"
    ~duration_family:response_body_drain_duration
    ~bytes_family:response_body_drain_bytes (Some bytes)
