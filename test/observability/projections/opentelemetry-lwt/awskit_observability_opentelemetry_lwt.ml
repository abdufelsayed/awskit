open Base
module O = Awskit.Observability
module P = O.For_projection
module Otel = Opentelemetry

type t = {
  trace_sink : Awskit_lwt.Observability.Trace_sink.t;
  metric_sink : O.Metric_sink.t;
}

let setup_ambient_context = Opentelemetry_lwt.setup_ambient_context

let value_of_diagnostic diagnostic : Otel.Value.t =
  match O.Diagnostic.Public.value diagnostic with
  | String value -> `String value
  | Bool value -> `Bool value
  | Int value -> `Int value
  | Int64 value -> `String (Int64.to_string value)
  | Float value -> `Float value

let attribute_of_diagnostic diagnostic =
  (O.Diagnostic.Public.name diagnostic, value_of_diagnostic diagnostic)

let attribute_of_dimension dimension =
  ( P.Dimension.name dimension,
    (`String (P.Dimension.value dimension) : Otel.Value.t) )

let attribute_of_measurement measurement =
  let value : Otel.Value.t =
    match P.Measurement.value measurement with
    | Int value -> `Int value
    | Int64 value -> `String (Int64.to_string value)
    | Float value -> `Float value
  in
  (P.Measurement.name measurement, value)

let outcome_attribute outcome =
  ("outcome", (`String (O.Outcome.to_string outcome) : Otel.Value.t))

let unique_attributes attributes =
  attributes
  |> List.fold
       ~init:(Set.empty (module String), [])
       ~f:(fun (names, unique) ((name, _) as attribute) ->
         if Set.mem names name then (names, unique)
         else (Set.add names name, attribute :: unique))
  |> snd
  |> List.rev

let without_names names attributes =
  List.filter attributes ~f:(fun (name, _) -> not (Set.mem names name))

let dimension name dimensions =
  List.find_map dimensions ~f:(fun value ->
      if String.equal name (P.Dimension.name value) then
        Some (P.Dimension.value value)
      else None)

let diagnostic name diagnostics =
  List.find_map diagnostics ~f:(fun value ->
      if String.equal name (O.Diagnostic.Public.name value) then
        Some (O.Diagnostic.Public.value value)
      else None)

let operation_name info = P.Operation.Info.name info

let semantic_start_attributes started =
  let info = P.Operation.Start.info started in
  match
    ( operation_name info,
      dimension "aws.operation" (P.Operation.Start.dimensions started) )
  with
  | "awskit.s3.operation", Some operation ->
      [
        ("rpc.system.name", (`String "aws-api" : Otel.Value.t));
        ("rpc.method", (`String ("S3." ^ operation) : Otel.Value.t));
      ]
  | _ -> []

let span_name started =
  let info = P.Operation.Start.info started in
  let name = operation_name info in
  let dimensions = P.Operation.Start.dimensions started in
  match
    ( name,
      dimension "aws.operation" dimensions,
      dimension "http.request.method" dimensions )
  with
  | "awskit.s3.operation", Some operation, _ -> "S3." ^ operation
  | "awskit.http.attempt", _, Some method_ -> method_
  | _ -> name

let span_kind info =
  match P.Operation.Info.span_kind info with
  | O.Span_kind.Internal -> Otel.Span.Span_kind_internal
  | Client -> Otel.Span.Span_kind_client

let error_type completion =
  let outcome = P.Operation.Completion.outcome completion in
  let name = completion |> P.Operation.Completion.info |> operation_name in
  match outcome with
  | O.Outcome.Ok | Cancelled -> None
  | Not_found | Conflict | Throttled | Error | Exception | Timeout -> (
      match
        diagnostic "http.response.status_code"
          (P.Operation.Completion.diagnostics completion)
      with
      | Some (Int status)
        when String.equal name "awskit.http.attempt" && status >= 400 ->
          Some (Int.to_string status)
      | Some (String _ | Bool _ | Int64 _ | Float _) | Some (Int _) | None ->
          Some (O.Outcome.to_string outcome))

let semantic_completion_attributes completion =
  let name = completion |> P.Operation.Completion.info |> operation_name in
  let error =
    Option.to_list
      (Option.map (error_type completion) ~f:(fun value ->
           ("error.type", (`String value : Otel.Value.t))))
  in
  let resend =
    if String.equal name "awskit.http.attempt" then
      match
        diagnostic "attempt" (P.Operation.Completion.diagnostics completion)
      with
      | Some (Int attempt) when attempt > 1 ->
          [ ("http.request.resend_count", (`Int (attempt - 1) : Otel.Value.t)) ]
      | Some (String _ | Bool _ | Int64 _ | Float _) | Some (Int _) | None -> []
    else []
  in
  error @ resend

let status_of_completion completion =
  Option.map (error_type completion) ~f:(fun _ ->
      Otel.Span_status.make ~message:"" ~code:Otel.Span_status.Status_code_error)

let correlation span =
  [
    O.Correlation.trace_id (span |> Otel.Span.trace_id |> Otel.Trace_id.to_hex);
    O.Correlation.span_id (span |> Otel.Span.id |> Otel.Span_id.to_hex);
  ]
  |> List.filter_map ~f:Result.ok

let make_trace_sink ~tracer =
  Awskit_lwt.Observability.Trace_sink.create ~name:"opentelemetry.trace"
    ~needs_clock:true
    ~enabled:(fun _ -> Otel.Tracer.enabled tracer)
    ~start:(fun started ->
      let info = P.Operation.Start.info started in
      let start_attributes =
        semantic_start_attributes started
        @ List.map
            (P.Operation.Start.dimensions started)
            ~f:attribute_of_dimension
        @ List.map
            (P.Operation.Start.measurements started)
            ~f:attribute_of_measurement
        @ List.map
            (P.Operation.Start.diagnostics started)
            ~f:attribute_of_diagnostic
        |> unique_attributes
      in
      let start_names =
        start_attributes |> List.map ~f:fst |> Set.of_list (module String)
      in
      let started_at = Otel.Clock.now tracer.clock in
      let stopped_at = ref started_at in
      let activation_tracer =
        { tracer with clock = { Otel.Clock.now = (fun () -> !stopped_at) } }
      in
      let thunk, finish_span =
        Otel.Tracer.with_thunk_and_finally activation_tracer
          ~kind:(span_kind info) ~attrs:start_attributes (span_name started)
          (fun span -> span)
      in
      let span = thunk () in
      {
        Awskit_lwt.Observability.Trace_sink.within =
          (fun f -> Otel.Ambient_span.with_ambient span f);
        correlation = correlation span;
        finish =
          (fun completion ->
            let attributes =
              List.map
                (P.Operation.Completion.dimensions completion)
                ~f:attribute_of_dimension
              @ List.map
                  (P.Operation.Completion.measurements completion)
                  ~f:attribute_of_measurement
              @ List.map
                  (P.Operation.Completion.diagnostics completion)
                  ~f:attribute_of_diagnostic
              |> without_names start_names
              |> unique_attributes
            in
            Otel.Span.add_attrs span
              (outcome_attribute (P.Operation.Completion.outcome completion)
               :: semantic_completion_attributes completion
               @ attributes
              |> unique_attributes);
            Option.iter
              (status_of_completion completion)
              ~f:(Otel.Span.set_status span);
            (stopped_at :=
               match P.Operation.Completion.duration_ns completion with
               | None -> Otel.Clock.now tracer.clock
               | Some duration -> Int64.(started_at + duration));
            finish_span (Ok ()));
      })
    ~event_enabled:(fun _ -> Otel.Tracer.enabled tracer)
    ~event:(fun event ->
      let attributes =
        List.map (P.Event.dimensions event) ~f:attribute_of_dimension
        @ List.map (P.Event.measurements event) ~f:attribute_of_measurement
        @ List.map (P.Event.diagnostics event) ~f:attribute_of_diagnostic
        |> unique_attributes
      in
      let name = event |> P.Event.info |> P.Event.Info.name in
      match Otel.Ambient_span.get () with
      | Some span ->
          Otel.Span.add_event span (Otel.Event.make ~attrs:attributes name)
      | None -> Otel.Tracer.with_ ~tracer ~attrs:attributes name ignore)

let metric_attributes observation =
  P.Metric.Observation.labels observation
  |> List.map ~f:(fun value ->
      ( P.Metric.Label.(value |> label |> name),
        (`String (P.Metric.Label.encoded value) : Otel.Value.t) ))

let value_as_float = function
  | P.Metric.Value.Int value -> Float.of_int value
  | Int64 value -> Int64.to_float value
  | Float value -> value

let number_point ~meter ~attributes = function
  | P.Metric.Value.Int value ->
      Otel.Metrics.int ~attrs:attributes
        ~now:(Otel.Clock.now meter.Otel.Meter.clock)
        value
  | Int64 value ->
      Otel.Metrics.float ~attrs:attributes
        ~now:(Otel.Clock.now meter.Otel.Meter.clock)
        (Int64.to_float value)
  | Float value ->
      Otel.Metrics.float ~attrs:attributes
        ~now:(Otel.Clock.now meter.Otel.Meter.clock)
        value

let observe_metric (meter : Otel.Meter.t) observation =
  let family = P.Metric.Observation.family observation in
  let value = P.Metric.Observation.value observation in
  let attributes = metric_attributes observation in
  let name = P.Metric.Family.name family in
  let description = P.Metric.Family.doc family in
  let unit_ = P.Metric.Family.unit_ family in
  match P.Metric.Family.aggregation family with
  | Counter ->
      Otel.Meter.emit1 meter
        (Otel.Metrics.sum ~name ~description ?unit_
           ~aggregation_temporality:Otel.Metrics.Aggregation_temporality_delta
           ~is_monotonic:true
           [ number_point ~meter ~attributes value ])
  | Gauge ->
      Otel.Meter.emit1 meter
        (Otel.Metrics.gauge ~name ~description ?unit_
           [ number_point ~meter ~attributes value ])
  | Histogram ->
      let point =
        Otel.Metrics.histogram_data_point ~attrs:attributes
          ~now:(Otel.Clock.now meter.clock)
          ~count:1L ~sum:(value_as_float value) ~bucket_counts:[ 1L ]
          ~explicit_bounds:[] ()
      in
      Otel.Meter.emit1 meter
        (Otel.Metrics.histogram ~name ~description ?unit_
           ~aggregation_temporality:Otel.Metrics.Aggregation_temporality_delta
           [ point ])

let make_metric_sink ~(meter : Otel.Meter.t) =
  O.Metric_sink.create ~name:"opentelemetry.metrics" ~needs_clock:true
    ~enabled:(fun _ -> Otel.Meter.enabled meter)
    ~observe:(observe_metric meter)

let create ?tracer ?meter () =
  let tracer =
    Option.value tracer ~default:(Otel.Sdk.get_tracer ~name:"awskit" ())
  in
  let meter =
    Option.value meter ~default:(Otel.Sdk.get_meter ~name:"awskit" ())
  in
  {
    trace_sink = make_trace_sink ~tracer;
    metric_sink = make_metric_sink ~meter;
  }

let trace_sink t = t.trace_sink
let metric_sink t = t.metric_sink
