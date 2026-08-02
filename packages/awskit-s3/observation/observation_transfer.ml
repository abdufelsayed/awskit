open Base
module F = Awskit.Observability.For_service

module Make_source (Source : sig
  val source : Logs.src
end) =
struct
  module Dimensions = Observation_dimensions

  type direction = Dimensions.transfer_direction = Upload | Download
  type summary = { logical_bytes : int64; parts : int }

  type finish = {
    summary : summary option;
    retry_class : Awskit.Error.retry_class option;
  }

  type completion_labels = {
    direction : direction;
    outcome : Awskit.Observability.Outcome.t;
  }

  type direction_labels = { direction : direction }

  let completion_labels =
    F.Metric.Labels.empty ()
    |> F.Metric.Labels.add Dimensions.transfer_direction_dimension
         ~get:(fun (labels : completion_labels) -> labels.direction)
    |> F.Metric.Labels.add Dimensions.outcome_dimension
         ~get:(fun (labels : completion_labels) -> labels.outcome)

  let direction_labels =
    F.Metric.Labels.empty ()
    |> F.Metric.Labels.add Dimensions.transfer_direction_dimension
         ~get:(fun (labels : direction_labels) -> labels.direction)

  let transfers =
    F.Metric.Family.counter ~name:"awskit.s3.transfers"
      ~doc:"Completed high-level S3 transfers" ~labels:completion_labels
      ~value:F.Metric.Number.Int64 ()

  let duration =
    F.Metric.Family.histogram ~name:"awskit.s3.transfer.duration"
      ~doc:"High-level S3 transfer duration" ~unit_:"ns"
      ~labels:completion_labels ~value:F.Metric.Number.Int64 ()

  let logical_bytes =
    F.Metric.Family.histogram ~name:"awskit.s3.transfer.logical_bytes"
      ~doc:"Logical bytes completed by a high-level S3 transfer" ~unit_:"By"
      ~labels:direction_labels ~value:F.Metric.Number.Int64 ()

  let parts =
    F.Metric.Family.histogram ~name:"awskit.s3.transfer.parts"
      ~doc:"Parts completed by a high-level S3 transfer"
      ~labels:direction_labels ~value:F.Metric.Number.Int64 ()

  let in_flight_family =
    F.Metric.Family.gauge ~name:"awskit.s3.transfers_in_flight"
      ~doc:"High-level S3 transfers currently in flight"
      ~labels:direction_labels ~value:F.Metric.Number.Int64 ()

  let in_flight = F.Instrument.define ~family:in_flight_family

  let start_fields direction =
    F.Fields.create
      ~dimensions:
        [
          F.Dimension.Enum.value Dimensions.transfer_direction_dimension
            direction;
        ]
      ()

  let finish_fields finish =
    let summary_measurements =
      Option.value_map finish.summary ~default:[] ~f:(fun summary ->
          [
            F.Measurement.int64 ~unit_:"By" ~name:"transfer.logical_bytes"
              summary.logical_bytes;
            F.Measurement.int ~name:"transfer.parts" summary.parts;
          ])
    in
    F.Fields.create
      ~dimensions:
        (Option.to_list
           (Option.map finish.retry_class ~f:(fun retry_class ->
                F.Dimension.Enum.value Dimensions.retry_class_dimension
                  retry_class)))
      ~measurements:summary_measurements ()

  let classify summarize terminal =
    match F.Terminal.result terminal with
    | Ok (Ok value) ->
        ( Awskit.Observability.Outcome.Ok,
          { summary = Some (summarize value); retry_class = None } )
    | Ok (Error error) ->
        ( Awskit.Observability.Outcome.of_error error,
          {
            summary = None;
            retry_class = Some (Awskit.Error.retry_class error);
          } )
    | Error _ ->
        ( F.Terminal.default_outcome terminal,
          { summary = None; retry_class = None } )

  let direction_to_string = function
    | Upload -> "upload"
    | Download -> "download"

  let message (completion : (direction, finish) F.Operation.Completion.t) =
    let direction =
      completion |> F.Operation.Completion.start |> direction_to_string
    in
    lazy
      (Fmt.str "S3 %s transfer finished with outcome %s" direction
         (completion
         |> F.Operation.Completion.outcome
         |> Awskit.Observability.Outcome.to_string))

  let log =
    F.Log.operation ~levels:[ Logs.Debug; Logs.Warning; Logs.Error ]
      ~decide:(fun
          (completion : (direction, finish) F.Operation.Completion.t) ->
        match F.Operation.Completion.outcome completion with
        | Awskit.Observability.Outcome.Ok -> F.Log.Skip
        | Cancelled -> Emit { level = Logs.Debug; message = message completion }
        | Throttled ->
            Emit { level = Logs.Warning; message = message completion }
        | Not_found | Conflict | Error | Exception | Timeout ->
            Emit { level = Logs.Error; message = message completion })

  let metrics =
    [
      F.Metric.Projection.sample transfers
        ~get:(fun (completion : (direction, finish) F.Operation.Completion.t) ->
          Some
            ( {
                direction = F.Operation.Completion.start completion;
                outcome = F.Operation.Completion.outcome completion;
              },
              1L ));
      F.Metric.Projection.sample ~needs_duration:true duration
        ~get:(fun (completion : (direction, finish) F.Operation.Completion.t) ->
          Option.map (F.Operation.Completion.duration_ns completion)
            ~f:(fun duration ->
              ( {
                  direction = F.Operation.Completion.start completion;
                  outcome = F.Operation.Completion.outcome completion;
                },
                duration )));
      F.Metric.Projection.sample logical_bytes
        ~get:(fun (completion : (direction, finish) F.Operation.Completion.t) ->
          Option.bind (F.Operation.Completion.finish completion)
            ~f:(fun finish ->
              Option.map finish.summary ~f:(fun summary ->
                  ( { direction = F.Operation.Completion.start completion },
                    summary.logical_bytes ))));
      F.Metric.Projection.sample parts
        ~get:(fun (completion : (direction, finish) F.Operation.Completion.t) ->
          Option.bind (F.Operation.Completion.finish completion)
            ~f:(fun finish ->
              Option.map finish.summary ~f:(fun summary ->
                  ( { direction = F.Operation.Completion.start completion },
                    Int64.of_int summary.parts ))));
    ]

  let operation summarize =
    F.Operation.define ~name:"awskit.s3.transfer"
      ~doc:"One high-level S3 upload or download" ~source:Source.source
      ~span_kind:Awskit.Observability.Span_kind.Internal ~start:start_fields
      ~classify:(classify summarize) ~finish:finish_fields ~log ~metrics ()

  let labels direction : direction_labels = { direction }

  module Make (Runtime : F.Observer) = struct
    let with_transfer direction connection ~summarize callback =
      Runtime.with_instrument connection in_flight
        ~labels:(fun () -> labels direction)
        1L
        (fun () ->
          Runtime.with_operation connection
            ~operation:(fun () -> operation summarize)
            ~start:(fun () -> direction)
            callback)

    let with_upload connection ~summarize callback =
      with_transfer Upload connection ~summarize callback

    let with_download connection ~summarize callback =
      with_transfer Download connection ~summarize callback
  end
end
