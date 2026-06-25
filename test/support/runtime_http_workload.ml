open Base
module Model = Runtime_http_model

module type TARGET = sig
  val name : string
  val run_scenario : Model.scenario -> (string, Awskit.Error.t) Result.t
end

let seed_scenarios =
  let scenario = Model.scenario in
  [
    scenario ~name:"head-200-content-length" ~method_:`HEAD ~status:200
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"head-404-content-length" ~method_:`HEAD ~status:404
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"get-204-content-length" ~method_:`GET ~status:204
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"get-304-content-length" ~method_:`GET ~status:304
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"get-200-chunked" ~method_:`GET ~status:200
      ~framing:(Chunked [ "he"; "llo" ])
      ~connection:Keep_alive ();
    scenario ~name:"get-200-content-length-underflow" ~method_:`GET ~status:200
      ~framing:(Content_length { declared = 6; actual = "hello" })
      ~connection:Close ();
    scenario ~name:"get-200-malformed-chunked" ~method_:`GET ~status:200
      ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n") ~connection:Close
      ();
  ]

let count_from_env ~var ~default =
  match Stdlib.Sys.getenv_opt var with
  | None | Some "" -> default
  | Some value -> (
      match Stdlib.int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let char_gen = QCheck.Gen.oneof_list [ 'a'; 'b'; 'c'; ' '; '-'; '_' ]

let small_body_gen =
  QCheck.Gen.(
    string_size ~gen:char_gen (int_range 0 12) >|= fun body ->
    if String.is_empty body then "x" else body)

let chunked_gen =
  QCheck.Gen.(
    list_size (int_range 1 4) small_body_gen >|= fun chunks ->
    Model.Chunked chunks)

let content_length_gen =
  let open QCheck.Gen in
  small_body_gen >>= fun actual ->
  oneof_list
    [
      Model.Content_length { declared = String.length actual; actual };
      Model.Content_length { declared = String.length actual + 1; actual };
      Model.Content_length
        { declared = Int.max 0 (String.length actual - 1); actual };
    ]

let framing_gen =
  QCheck.Gen.(
    oneof_weighted
      [
        (1, return Model.Empty);
        (4, content_length_gen);
        (3, chunked_gen);
        (2, return (Model.Malformed_chunked "5\r\nhello\r\nnot-hex\r\n"));
      ])

let connection_for_framing_gen framing =
  match framing with
  | Model.Content_length { declared; actual }
    when not (Int.equal declared (String.length actual)) ->
      QCheck.Gen.return Model.Close
  | Malformed_chunked _ -> QCheck.Gen.return Model.Close
  | Empty | Content_length _ | Chunked _ ->
      QCheck.Gen.oneof_list [ Model.Close; Model.Keep_alive ]

let scenario_gen =
  let open QCheck.Gen in
  oneof_list [ `GET; `HEAD ] >>= fun method_ ->
  oneof_list [ 200; 204; 206; 304; 400; 404; 500 ] >>= fun status ->
  framing_gen >>= fun framing ->
  connection_for_framing_gen framing >>= fun connection ->
  return
    (Model.scenario
       ~name:
         (Printf.sprintf "generated-%s-%d"
            (String.lowercase (Model.method_to_string method_))
            status)
       ~method_ ~status ~framing ~connection ())

let generated_samples count = QCheck.Gen.generate ~n:count scenario_gen

let required_runtime_bins =
  [
    "http.method.get";
    "http.method.head";
    "http.status.bodiless";
    "http.status.error";
    "http.framing.content-length.exact";
    "http.framing.content-length.underflow";
    "http.framing.content-length.overflow";
    "http.framing.chunked";
    "http.framing.malformed-chunked";
    "http.expected.no-body";
    "http.expected.body";
    "http.expected.body-error";
  ]

let test_generator_coverage () =
  let coverage =
    Workload_coverage.of_lists (generated_samples 1_000)
      ~bins:Runtime_http_model.coverage_bins
  in
  Workload_coverage.require_all ~label:"runtime HTTP generator"
    ~required:required_runtime_bins coverage

let check_scenario ~target_name run_scenario scenario =
  match (scenario.Model.expected_body, run_scenario scenario) with
  | No_body, Ok "" -> true
  | No_body, Ok body ->
      QCheck.Test.fail_reportf "%s returned bodiless response bytes: %S\n%s"
        target_name body (Model.to_string scenario)
  | No_body, Error error ->
      QCheck.Test.fail_reportf "%s failed bodiless response: %s\n%s" target_name
        (Awskit.Error.to_string_hum error)
        (Model.to_string scenario)
  | Body expected, Ok actual when String.equal expected actual -> true
  | Body expected, Ok actual ->
      QCheck.Test.fail_reportf "%s body mismatch: expected %S, got %S\n%s"
        target_name expected actual (Model.to_string scenario)
  | Body _, Error error ->
      QCheck.Test.fail_reportf "%s unexpected body error: %s\n%s" target_name
        (Awskit.Error.to_string_hum error)
        (Model.to_string scenario)
  | Body_error, Error _ -> true
  | Body_error, Ok actual ->
      QCheck.Test.fail_reportf "%s expected body error, got %S\n%s" target_name
        actual (Model.to_string scenario)

module Make (Target : TARGET) = struct
  let deterministic_cases =
    List.map seed_scenarios ~f:(fun scenario ->
        Alcotest.test_case scenario.name `Quick (fun () ->
            ignore
              (check_scenario ~target_name:Target.name Target.run_scenario
                 scenario
                : bool)))

  let generated_case =
    let count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:250 in
    QCheck.Test.make ~count ~name:"generated scenarios"
      (QCheck.make ~print:Model.to_string scenario_gen)
      (check_scenario ~target_name:Target.name Target.run_scenario)
    |> QCheck_alcotest.to_alcotest ~speed_level:`Quick

  let suite =
    [
      ( Printf.sprintf "workload:%s:runtime-http" Target.name,
        deterministic_cases
        @ [
            Alcotest.test_case "generator semantic coverage" `Quick
              test_generator_coverage;
            generated_case;
          ] );
    ]
end
