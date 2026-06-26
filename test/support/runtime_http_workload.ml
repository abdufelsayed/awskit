open Base
module Model = Runtime_http_model

module type TARGET = sig
  val name : string
  val run_scenario : Model.scenario -> Model.observed
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
    scenario ~name:"get-200-duplicate-content-length" ~method_:`GET ~status:200
      ~framing:
        (Duplicate_content_length { first = 5; second = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"get-200-duplicate-content-length-mismatch" ~method_:`GET
      ~status:200
      ~framing:
        (Duplicate_content_length { first = 5; second = 6; actual = "hello" })
      ~connection:Close ();
    scenario ~name:"get-200-conflicting-length-and-chunked" ~method_:`GET
      ~status:200
      ~framing:
        (Conflicting_length_and_chunked { declared = 5; chunks = [ "hi" ] })
      ~connection:Close ();
    scenario ~name:"get-200-early-close" ~method_:`GET ~status:200
      ~framing:(Early_close "hello") ~connection:Close ();
    scenario ~name:"get-200-malformed-header-block" ~method_:`GET ~status:200
      ~framing:(Malformed_header_block "Content-Length: nope\r\n")
      ~connection:Close ();
    scenario ~name:"get-200-content-length-plus" ~method_:`GET ~status:200
      ~framing:(Malformed_header_block "Content-Length: +5\r\n")
      ~connection:Close ();
    scenario ~name:"get-200-content-length-empty" ~method_:`GET ~status:200
      ~framing:(Malformed_header_block "Content-Length:\r\n") ~connection:Close
      ();
    scenario ~name:"get-200-transfer-encoding-with-content-length" ~method_:`GET
      ~status:200
      ~framing:
        (Malformed_header_block
           "Transfer-Encoding: gzip\r\nContent-Length: 5\r\n") ~connection:Close
      ();
    scenario ~name:"get-200-duplicate-content-length-mismatch-raise"
      ~method_:`GET ~status:200
      ~framing:
        (Duplicate_content_length { first = 5; second = 6; actual = "hello" })
      ~connection:Close ~consume:Raise_in_consume ();
    scenario ~name:"get-200-conflicting-length-drop-without-read" ~method_:`GET
      ~status:200
      ~framing:
        (Conflicting_length_and_chunked { declared = 5; chunks = [ "hi" ] })
      ~connection:Close ~consume:Drop_without_read ();
    scenario ~name:"get-200-malformed-header-raise-in-consume" ~method_:`GET
      ~status:200 ~framing:(Malformed_header_block "Content-Length: nope\r\n")
      ~connection:Close ~consume:Raise_in_consume ();
    scenario ~name:"get-200-read-once" ~method_:`GET ~status:200
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ~consume:(Read_once 2) ();
    scenario ~name:"get-200-drop-without-read" ~method_:`GET ~status:200
      ~framing:(Chunked [ "he"; "llo" ])
      ~connection:Keep_alive ~consume:Drop_without_read ();
    scenario ~name:"get-200-raise-in-consume" ~method_:`GET ~status:200
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Close ~consume:Raise_in_consume ();
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

let consume_gen =
  QCheck.Gen.(
    oneof_weighted
      [
        (6, return Model.Read_all);
        (2, map (fun n -> Model.Read_once n) (int_range 1 8));
        (1, return Model.Drop_without_read);
        (1, return Model.Raise_in_consume);
      ])

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

let duplicate_content_length_gen =
  QCheck.Gen.(
    small_body_gen >>= fun actual ->
    oneof_list
      [
        Model.Duplicate_content_length
          {
            first = String.length actual;
            second = String.length actual;
            actual;
          };
        Model.Duplicate_content_length
          {
            first = String.length actual;
            second = String.length actual + 1;
            actual;
          };
      ])

let body_length chunks =
  List.fold chunks ~init:0 ~f:(fun total chunk -> total + String.length chunk)

let conflicting_length_and_chunked_gen =
  QCheck.Gen.(
    list_size (int_range 1 4) small_body_gen >>= fun chunks ->
    let actual_length = body_length chunks in
    oneof_list
      [
        Model.Conflicting_length_and_chunked
          { declared = actual_length; chunks };
        Model.Conflicting_length_and_chunked
          { declared = actual_length + 1; chunks };
      ])

let early_close_gen =
  QCheck.Gen.(small_body_gen >|= fun actual -> Model.Early_close actual)

let malformed_header_block_gen =
  QCheck.Gen.(
    oneof_list
      [
        Model.Malformed_header_block "Content-Length: nope\r\n";
        Model.Malformed_header_block "Broken response header\r\n";
      ])

let framing_gen =
  QCheck.Gen.(
    oneof_weighted
      [
        (1, return Model.Empty);
        (5, content_length_gen);
        (4, chunked_gen);
        (2, duplicate_content_length_gen);
        (2, return (Model.Malformed_chunked "5\r\nhello\r\nnot-hex\r\n"));
        (1, conflicting_length_and_chunked_gen);
        (1, early_close_gen);
        (1, malformed_header_block_gen);
      ])

let connection_for_scenario_gen ~method_ ~status framing =
  let bodiless = Model.is_bodiless_response ~method_ ~status in
  match framing with
  | Model.Empty when not bodiless -> QCheck.Gen.return Model.Close
  | Model.Content_length { declared; actual }
    when not (Int.equal declared (String.length actual)) ->
      QCheck.Gen.return Model.Close
  | Duplicate_content_length { first; second; actual }
    when (not (Int.equal first second))
         || not (Int.equal first (String.length actual)) ->
      QCheck.Gen.return Model.Close
  | Conflicting_length_and_chunked _ | Early_close _ | Malformed_chunked _
  | Malformed_header_block _ ->
      QCheck.Gen.return Model.Close
  | Empty | Content_length _ | Duplicate_content_length _ | Chunked _ ->
      QCheck.Gen.oneof_list [ Model.Close; Model.Keep_alive ]

let scenario_gen =
  let open QCheck.Gen in
  oneof_list [ `GET; `HEAD ] >>= fun method_ ->
  oneof_list [ 200; 204; 206; 304; 400; 404; 500 ] >>= fun status ->
  framing_gen >>= fun framing ->
  connection_for_scenario_gen ~method_ ~status framing >>= fun connection ->
  consume_gen >>= fun consume ->
  return
    (Model.scenario
       ~name:
         (Printf.sprintf "generated-%s-%d"
            (String.lowercase (Model.method_to_string method_))
            status)
       ~method_ ~status ~framing ~connection ~consume ())

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
    "http.framing.duplicate-content-length.equal";
    "http.framing.duplicate-content-length.mismatch";
    "http.framing.conflicting-length-and-chunked";
    "http.framing.early-close";
    "http.framing.chunked";
    "http.framing.malformed-chunked";
    "http.framing.malformed-header-block";
    "http.consume.read-all";
    "http.consume.read-once";
    "http.consume.drop-without-read";
    "http.consume.raise-in-consume";
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

let expected_observation_to_string scenario = function
  | `Body body -> Printf.sprintf "body:%S" body
  | `Body_prefix body -> Printf.sprintf "body-prefix:%S" body
  | `Body_error when Model.rejects_before_body_consumer scenario ->
      "body-error(pre-consumer)"
  | `Body_error -> "body-error"
  | `Exception_preserved -> "exception-preserved"
  | `No_body_required -> "no-body-required"

let fail_observation ~target_name scenario expected observed =
  QCheck.Test.fail_reportf "%s observation mismatch: expected %s, got %s\n%s"
    target_name
    (expected_observation_to_string scenario expected)
    (Model.observed_to_string observed)
    (Model.to_string scenario)

let valid_body_prefix ~max_prefix actual =
  String.is_prefix max_prefix ~prefix:actual
  && (String.is_empty max_prefix || not (String.is_empty actual))

let check_scenario ~target_name run_scenario scenario =
  let expected = Model.expected_observation scenario in
  let observed = run_scenario scenario in
  match (expected, observed) with
  | `Body expected, Model.Observed_body actual when String.equal expected actual
    ->
      true
  | `Body _, Model.Observed_body _
  | `Body _, Model.Observed_error _
  | `Body _, Model.Observed_exception ->
      fail_observation ~target_name scenario expected observed
  | `Body_prefix expected, Model.Observed_body actual
    when valid_body_prefix ~max_prefix:expected actual ->
      true
  | `Body_prefix _, Model.Observed_body _
  | `Body_prefix _, Model.Observed_error _
  | `Body_prefix _, Model.Observed_exception ->
      fail_observation ~target_name scenario expected observed
  | `Body_error, Model.Observed_error _ -> true
  | `Body_error, Model.Observed_body _ | `Body_error, Model.Observed_exception
    ->
      fail_observation ~target_name scenario expected observed
  | `Exception_preserved, Model.Observed_exception -> true
  | `Exception_preserved, Model.Observed_body _
  | `Exception_preserved, Model.Observed_error _ ->
      fail_observation ~target_name scenario expected observed
  | `No_body_required, Model.Observed_body "" -> true
  | `No_body_required, Model.Observed_body _ ->
      fail_observation ~target_name scenario expected observed
  | `No_body_required, Model.Observed_error _
  | `No_body_required, Model.Observed_exception ->
      fail_observation ~target_name scenario expected observed

module Make (Target : TARGET) = struct
  let deterministic_cases =
    List.map seed_scenarios ~f:(fun scenario ->
        Alcotest.test_case scenario.name `Quick (fun () ->
            ignore
              (check_scenario ~target_name:Target.name Target.run_scenario
                 scenario
                : bool)))

  let replay_cases =
    List.map Runtime_http_replay.all ~f:(fun replay ->
        Alcotest.test_case replay.path `Quick (fun () ->
            ignore
              (check_scenario ~target_name:Target.name Target.run_scenario
                 replay.scenario
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
        @ replay_cases
        @ [
            Alcotest.test_case "generator semantic coverage" `Quick
              test_generator_coverage;
            generated_case;
          ] );
    ]
end
