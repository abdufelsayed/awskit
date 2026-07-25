open Base
module O = Awskit.Observability
module P = O.For_projection

let span_key = Lwt.new_key ()

let install_context () =
  Trace_core.set_ambient_context_provider
    (Trace_core.Ambient_span_provider.ASP_some
       ( (),
         {
           Trace_core.Ambient_span_provider.Callbacks.get_current_span =
             (fun () -> Lwt.get span_key);
           with_current_span_set_to =
             (fun () span f ->
               Lwt.with_value span_key (Some span) (fun () -> f span));
         } ))

let user_data_of_diagnostic diagnostic =
  let value =
    match O.Diagnostic.Public.value diagnostic with
    | String value -> `String value
    | Bool value -> `Bool value
    | Int value -> `Int value
    | Int64 value -> `String (Int64.to_string value)
    | Float value -> `Float value
  in
  (O.Diagnostic.Public.name diagnostic, value)

let user_data_of_dimension dimension =
  (P.Dimension.name dimension, `String (P.Dimension.value dimension))

let user_data_of_measurement measurement =
  let value =
    match P.Measurement.value measurement with
    | Int value -> `Int value
    | Int64 value -> `String (Int64.to_string value)
    | Float value -> `Float value
  in
  (P.Measurement.name measurement, value)

let sink =
  Awskit_lwt.Observability.Trace_sink.create ~name:"trace" ~needs_clock:true
    ~enabled:(fun _ -> Trace_core.enabled ())
    ~start:(fun started ->
      let info = P.Operation.Start.info started in
      let span =
        Trace_core.enter_span ~__FILE__:Stdlib.__FILE__ ~__LINE__:0
          ~flavor:`Async
          ~parent:(Trace_core.current_span ())
          (P.Operation.Info.name info)
      in
      {
        Awskit_lwt.Observability.Trace_sink.within =
          (fun f -> Lwt.with_value span_key (Some span) f);
        correlation = [];
        finish =
          (fun completion ->
            Exn.protect
              ~f:(fun () ->
                Trace_core.add_data_to_span span
                  (( "outcome",
                     `String
                       (completion
                       |> P.Operation.Completion.outcome
                       |> O.Outcome.to_string) )
                   :: (completion
                      |> P.Operation.Completion.dimensions
                      |> List.map ~f:user_data_of_dimension)
                  @ (completion
                    |> P.Operation.Completion.measurements
                    |> List.map ~f:user_data_of_measurement)
                  @ (completion
                    |> P.Operation.Completion.diagnostics
                    |> List.map ~f:user_data_of_diagnostic)))
              ~finally:(fun () -> Trace_core.exit_span span));
      })
    ~event_enabled:(fun _ -> Trace_core.enabled ())
    ~event:(fun event ->
      let data () =
        (event |> P.Event.dimensions |> List.map ~f:user_data_of_dimension)
        @ (event |> P.Event.measurements |> List.map ~f:user_data_of_measurement)
        @ (event |> P.Event.diagnostics |> List.map ~f:user_data_of_diagnostic)
      in
      Trace_core.message ~data (event |> P.Event.info |> P.Event.Info.name))
