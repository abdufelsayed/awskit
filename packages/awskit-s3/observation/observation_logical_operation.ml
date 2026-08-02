open Base
module F = Awskit.Observability.For_service

module type Identity = sig
  type t

  val equal : t -> t -> bool
  val values : t list
  val to_string : t -> string
  val source : Logs.src
end

module Make (Identity : Identity) = struct
  type start = {
    operation : Identity.t;
    region : string option;
    bucket : string option;
  }

  type finish = {
    attempts : int option;
    logical_request_bytes : int64 option;
    logical_response_bytes : int64 option;
    retry_class : Awskit.Error.retry_class option;
  }

  let start ?region ?bucket operation = { operation; region; bucket }

  let operation_dimension =
    F.Dimension.Enum.define ~name:"aws.operation" ~equal:Identity.equal
      ~values:
        (List.map Identity.values ~f:(fun operation ->
             (operation, Identity.to_string operation)))

  let retry_class_dimension =
    F.Dimension.Enum.define ~name:"retry.class" ~equal:Poly.equal
      ~values:
        [
          (Awskit.Error.Retryable, "retryable");
          (Throttled, "throttled");
          (Auth, "auth");
          (Conflict, "conflict");
          (Not_found, "not_found");
          (Fatal, "fatal");
          (Unknown, "unknown");
        ]

  let start_fields start =
    F.Fields.create
      ~dimensions:[ F.Dimension.Enum.value operation_dimension start.operation ]
      ~diagnostics:
        (List.filter_map ~f:Fn.id
           [
             Option.bind start.region ~f:F.Diagnostic.aws_region;
             Option.bind start.bucket ~f:F.Diagnostic.bucket_name;
           ])
      ()

  let finish_fields finish =
    F.Fields.create
      ~dimensions:
        (Option.to_list
           (Option.map finish.retry_class ~f:(fun retry_class ->
                F.Dimension.Enum.value retry_class_dimension retry_class)))
      ~measurements:
        (Option.to_list
           (Option.map finish.attempts ~f:(fun attempts ->
                F.Measurement.int ~name:"attempts" attempts))
        @ Option.to_list
            (Option.map finish.logical_request_bytes ~f:(fun bytes ->
                 F.Measurement.int64 ~unit_:"By" ~name:"logical.request_bytes"
                   bytes))
        @ Option.to_list
            (Option.map finish.logical_response_bytes ~f:(fun bytes ->
                 F.Measurement.int64 ~unit_:"By" ~name:"logical.response_bytes"
                   bytes)))
      ()

  let message (completion : (start, finish) F.Operation.Completion.t) =
    let operation =
      completion |> F.Operation.Completion.start |> fun start ->
      Identity.to_string start.operation
    in
    lazy
      (Fmt.str "S3 %s finished with outcome %s" operation
         (completion
         |> F.Operation.Completion.outcome
         |> Awskit.Observability.Outcome.to_string))

  let log =
    F.Log.operation ~levels:[ Logs.Debug; Logs.Warning; Logs.Error ]
      ~decide:(fun (completion : (start, finish) F.Operation.Completion.t) ->
        match F.Operation.Completion.outcome completion with
        | Awskit.Observability.Outcome.Ok -> F.Log.Skip
        | Not_found | Conflict | Cancelled ->
            Emit { level = Logs.Debug; message = message completion }
        | Throttled ->
            Emit { level = Logs.Warning; message = message completion }
        | Error | Exception | Timeout ->
            Emit { level = Logs.Error; message = message completion })

  let define ~classify ~metrics =
    F.Operation.define ~name:"awskit.s3.operation"
      ~doc:"One logical S3 SDK operation" ~source:Identity.source
      ~span_kind:Awskit.Observability.Span_kind.Client ~start:start_fields
      ~classify ~finish:finish_fields ~log ~metrics ()

  type simulated_result = {
    outcome : Awskit.Observability.Outcome.t;
    retry_class : Awskit.Error.retry_class option;
    logical_request_bytes : int64 option;
    logical_response_bytes : int64 option;
  }

  let classify_simulated terminal =
    match F.Terminal.result terminal with
    | Ok simulated ->
        ( simulated.outcome,
          {
            attempts = None;
            logical_request_bytes = simulated.logical_request_bytes;
            logical_response_bytes = simulated.logical_response_bytes;
            retry_class = simulated.retry_class;
          } )
    | Error _ ->
        ( F.Terminal.default_outcome terminal,
          {
            attempts = None;
            logical_request_bytes = None;
            logical_response_bytes = None;
            retry_class = None;
          } )

  let simulated_operation = define ~classify:classify_simulated ~metrics:[]

  let complete ~operation ~outcome ?retry_class ?logical_request_bytes
      ?logical_response_bytes ?region ?bucket () =
    F.Operation.project simulated_operation
      ~start:(start ?region ?bucket operation)
      ~result:
        (Ok
           {
             outcome;
             retry_class;
             logical_request_bytes;
             logical_response_bytes;
           })
      ~default_outcome:outcome ~duration_ns:None
end
