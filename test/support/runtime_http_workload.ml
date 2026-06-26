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
    scenario ~name:"get-100-content-length" ~method_:`GET ~status:100
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ();
    scenario ~name:"get-200-chunked" ~method_:`GET ~status:200
      ~framing:(Chunked [ "he"; "llo" ])
      ~connection:Keep_alive ();
    scenario ~name:"get-200-content-length-underflow" ~method_:`GET ~status:200
      ~framing:(Content_length { declared = 6; actual = "hello" })
      ~connection:Close ();
    scenario ~name:"head-200-malformed-chunked-bodiless" ~method_:`HEAD
      ~status:200 ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n")
      ~connection:Close ();
    scenario ~name:"get-204-malformed-chunked-bodiless" ~method_:`GET
      ~status:204 ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n")
      ~connection:Close ();
    scenario ~name:"get-304-malformed-chunked-bodiless" ~method_:`GET
      ~status:304 ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n")
      ~connection:Close ();
    scenario ~name:"head-200-conflicting-length-and-chunked-bodiless"
      ~method_:`HEAD ~status:200
      ~framing:
        (Conflicting_length_and_chunked { declared = 5; chunks = [ "hi" ] })
      ~connection:Close ();
    scenario ~name:"get-204-conflicting-length-and-chunked-bodiless"
      ~method_:`GET ~status:204
      ~framing:
        (Conflicting_length_and_chunked { declared = 5; chunks = [ "hi" ] })
      ~connection:Close ();
    scenario ~name:"head-200-transfer-encoding-with-content-length-bodiless"
      ~method_:`HEAD ~status:200
      ~framing:
        (Malformed_header_block
           "Transfer-Encoding: gzip\r\nContent-Length: 5\r\n") ~connection:Close
      ();
    scenario ~name:"head-200-duplicate-content-length-mismatch-strict"
      ~method_:`HEAD ~status:200
      ~framing:
        (Duplicate_content_length { first = 5; second = 6; actual = "hello" })
      ~connection:Close ();
    scenario ~name:"head-200-content-length-plus-strict" ~method_:`HEAD
      ~status:200 ~framing:(Malformed_header_block "Content-Length: +5\r\n")
      ~connection:Close ();
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
    scenario ~name:"get-200-malformed-header-line" ~method_:`GET ~status:200
      ~framing:(Malformed_header_block "Broken response header\r\n")
      ~connection:Close ();
    scenario ~name:"head-200-malformed-header-line" ~method_:`HEAD ~status:200
      ~framing:(Malformed_header_block "Broken response header\r\n")
      ~connection:Close ();
    scenario ~name:"get-204-malformed-header-line" ~method_:`GET ~status:204
      ~framing:(Malformed_header_block "Broken response header\r\n")
      ~connection:Close ();
    scenario ~name:"get-304-malformed-header-line" ~method_:`GET ~status:304
      ~framing:(Malformed_header_block "Broken response header\r\n")
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
    scenario ~name:"get-200-chunked-read-once-short-read" ~method_:`GET
      ~status:200
      ~framing:(Chunked [ "cbb_-"; "b" ])
      ~connection:Close ~consume:(Read_once 8) ();
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

let method_status_gen =
  QCheck.Gen.(
    oneof_list
      [
        (`GET, 200);
        (`GET, 204);
        (`GET, 206);
        (`GET, 304);
        (`GET, 400);
        (`GET, 404);
        (`GET, 500);
        (`HEAD, 200);
        (`HEAD, 204);
        (`HEAD, 304);
        (`HEAD, 400);
        (`HEAD, 404);
        (`HEAD, 500);
      ])

let bodiless_method_status_gen =
  QCheck.Gen.(
    oneof_list [ (`GET, 100); (`GET, 204); (`GET, 304); (`HEAD, 200) ])

let method_status_gen_for_framing = function
  | Model.Malformed_chunked _ -> bodiless_method_status_gen
  | Empty | Content_length _ | Duplicate_content_length _
  | Conflicting_length_and_chunked _ | Early_close _ | Chunked _
  | Malformed_header_block _ ->
      method_status_gen

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
  framing_gen >>= fun framing ->
  method_status_gen_for_framing framing >>= fun (method_, status) ->
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

let connection_allowed ~method_ ~status framing connection =
  let bodiless = Model.is_bodiless_response ~method_ ~status in
  match framing with
  | Model.Empty when not bodiless -> Poly.equal connection Model.Close
  | Model.Content_length { declared; actual }
    when not (Int.equal declared (String.length actual)) ->
      Poly.equal connection Model.Close
  | Duplicate_content_length { first; second; actual }
    when (not (Int.equal first second))
         || not (Int.equal first (String.length actual)) ->
      Poly.equal connection Model.Close
  | Conflicting_length_and_chunked _ | Early_close _ | Malformed_chunked _
  | Malformed_header_block _ ->
      Poly.equal connection Model.Close
  | Empty | Content_length _ | Duplicate_content_length _ | Chunked _ -> true

let normalize_connection ~method_ ~status framing connection =
  if connection_allowed ~method_ ~status framing connection then connection
  else Model.Close

let normalize_method_status framing method_ status =
  match framing with
  | Model.Malformed_chunked _
    when not (Model.is_bodiless_response ~method_ ~status) ->
      (`HEAD, status)
  | Empty | Content_length _ | Duplicate_content_length _
  | Conflicting_length_and_chunked _ | Early_close _ | Chunked _
  | Malformed_header_block _ | Malformed_chunked _ ->
      (method_, status)

let rebuild_scenario scenario ?name ?method_ ?status ?framing ?connection
    ?consume () =
  let name = Option.value name ~default:scenario.Model.name in
  let method_ = Option.value method_ ~default:scenario.method_ in
  let status = Option.value status ~default:scenario.status in
  let framing = Option.value framing ~default:scenario.framing in
  let method_, status = normalize_method_status framing method_ status in
  let connection = Option.value connection ~default:scenario.connection in
  let connection = normalize_connection ~method_ ~status framing connection in
  let consume = Option.value consume ~default:scenario.consume in
  Model.scenario ~name ~method_ ~status ~headers:scenario.headers ~framing
    ~connection ~consume ()

let shrink_method = function
  | `GET -> []
  | `HEAD | `PUT | `POST | `DELETE | `PATCH -> [ `GET ]

let shrink_status status = if Int.equal status 200 then [] else [ 200 ]

let shrink_connection = function
  | Model.Close -> []
  | Keep_alive -> [ Model.Close ]

let shrink_consume = function
  | Model.Read_all -> []
  | Read_once n ->
      if n > 1 then [ Model.Read_all; Model.Read_once 1 ] else [ Read_all ]
  | Drop_without_read | Raise_in_consume -> [ Read_all ]

let exact_content_length actual =
  Model.Content_length { declared = String.length actual; actual }

let shrink_framing = function
  | Model.Empty -> []
  | Content_length { declared; actual } ->
      [
        Model.Empty;
        exact_content_length actual;
        exact_content_length "";
        Content_length
          { declared = Int.min declared (String.length actual); actual };
      ]
  | Duplicate_content_length { first; second; actual } ->
      [
        Model.Empty;
        exact_content_length actual;
        Duplicate_content_length { first; second = first; actual };
        Duplicate_content_length
          {
            first = String.length actual;
            second = String.length actual;
            actual;
          };
      ]
  | Conflicting_length_and_chunked { chunks; _ } ->
      let actual = String.concat chunks in
      [ Model.Empty; Model.Chunked chunks; exact_content_length actual ]
  | Early_close actual -> [ Model.Empty; exact_content_length actual ]
  | Chunked chunks ->
      [ Model.Empty; exact_content_length (String.concat chunks) ]
  | Malformed_chunked wire ->
      let minimal = Model.Malformed_chunked "not-hex\r\n" in
      if String.equal wire "not-hex\r\n" then [ Model.Empty ]
      else [ minimal; Model.Empty ]
  | Malformed_header_block block ->
      let minimal = Model.Malformed_header_block "Broken response header\r\n" in
      if String.equal block "Broken response header\r\n" then [ Model.Empty ]
      else [ minimal; Model.Empty ]

let shrink_name name =
  if String.equal name "generated" then [] else [ "generated" ]

let distinct_strict_scenarios original candidates =
  let original_key = Model.to_string original in
  let seen = ref [] in
  List.filter candidates ~f:(fun candidate ->
      let key = Model.to_string candidate in
      if String.equal key original_key || List.mem !seen key ~equal:String.equal
      then false
      else (
        seen := key :: !seen;
        true))

let shrink_scenario scenario =
  let candidates =
    [
      List.map (shrink_name scenario.name) ~f:(fun name ->
          rebuild_scenario scenario ~name ());
      List.map (shrink_method scenario.method_) ~f:(fun method_ ->
          rebuild_scenario scenario ~method_ ());
      List.map (shrink_status scenario.status) ~f:(fun status ->
          rebuild_scenario scenario ~status ());
      List.map (shrink_framing scenario.framing) ~f:(fun framing ->
          rebuild_scenario scenario ~framing ());
      List.map (shrink_connection scenario.connection) ~f:(fun connection ->
          rebuild_scenario scenario ~connection ());
      List.map (shrink_consume scenario.consume) ~f:(fun consume ->
          rebuild_scenario scenario ~consume ());
    ]
    |> List.concat
    |> distinct_strict_scenarios scenario
  in
  QCheck.Iter.of_list candidates

let iter_to_list iter =
  let values = ref [] in
  iter (fun value -> values := value :: !values);
  List.rev !values

let test_scenario_shrinker_emits_recomputed_candidates () =
  let scenario =
    Model.scenario ~name:"generated-head-500" ~method_:`HEAD ~status:500
      ~framing:(Content_length { declared = 6; actual = "hello" })
      ~connection:Close ~consume:(Read_once 4) ()
  in
  let candidates = iter_to_list (shrink_scenario scenario) in
  Alcotest.(check bool)
    "emits shrink candidates" true
    (not (List.is_empty candidates));
  Alcotest.(check bool)
    "excludes original" false
    (List.exists candidates ~f:(fun candidate ->
         String.equal (Model.to_string candidate) (Model.to_string scenario)));
  List.iter candidates ~f:(fun candidate ->
      Alcotest.(check bool)
        ("connection is valid for " ^ candidate.name)
        true
        (connection_allowed ~method_:candidate.method_ ~status:candidate.status
           candidate.framing candidate.connection);
      Alcotest.(check string)
        ("expected body recomputed for " ^ candidate.name)
        (Model.expected_body_to_string
           (Model.expected_body_for ~headers:candidate.headers
              ~bodiless:(Model.is_bodiless candidate)
              candidate.framing))
        (Model.expected_body_to_string candidate.expected_body))

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

let framing_pair_segment = function
  | Model.Empty -> "empty"
  | Content_length { declared; actual } ->
      if Int.equal declared (String.length actual) then "content-length-exact"
      else if declared > String.length actual then "content-length-underflow"
      else "content-length-overflow"
  | Duplicate_content_length { first; second; _ } ->
      if Int.equal first second then "duplicate-content-length-equal"
      else "duplicate-content-length-mismatch"
  | Conflicting_length_and_chunked _ -> "conflicting-length-and-chunked"
  | Early_close _ -> "early-close"
  | Chunked _ -> "chunked"
  | Malformed_chunked _ -> "malformed-chunked"
  | Malformed_header_block _ -> "malformed-header-block"

let consume_pair_segment = function
  | Model.Read_all -> "read-all"
  | Read_once _ -> "read-once"
  | Drop_without_read -> "drop-without-read"
  | Raise_in_consume -> "raise-in-consume"

let framing_consume_pair_bin scenario =
  Printf.sprintf "http.pair.%s/%s"
    (framing_pair_segment scenario.Model.framing)
    (consume_pair_segment scenario.consume)

let runtime_coverage_bins scenario =
  Model.coverage_bins scenario @ [ framing_consume_pair_bin scenario ]

let scaled_thresholds ~sample_count specs =
  List.filter_map specs ~f:(fun (bin, minimum_per_1_000) ->
      let minimum = sample_count * minimum_per_1_000 / 1_000 in
      if minimum <= 0 then None
      else Some (Workload_coverage.threshold ~bin ~minimum))

let runtime_distribution_thresholds ~sample_count =
  scaled_thresholds ~sample_count
    [
      ("http.method.get", 100);
      ("http.method.head", 100);
      ("http.framing.content-length.exact", 40);
      ("http.framing.content-length.underflow", 20);
      ("http.framing.chunked", 40);
      ("http.consume.read-all", 100);
      ("http.consume.read-once", 20);
    ]

let runtime_pair_thresholds ~sample_count =
  scaled_thresholds ~sample_count
    [
      ("http.pair.content-length-underflow/read-all", 10);
      ("http.pair.chunked/read-all", 20);
      ("http.pair.content-length-exact/read-once", 2);
    ]

let test_generator_coverage () =
  let sample_count = 1_000 in
  let coverage =
    Workload_coverage.of_lists
      (generated_samples sample_count)
      ~bins:runtime_coverage_bins
  in
  let label =
    Printf.sprintf "runtime HTTP generator (%d samples)" sample_count
  in
  Workload_coverage.require_all ~label ~required:required_runtime_bins coverage;
  Workload_coverage.require_thresholds ~label ~sample_count
    ~thresholds:
      (runtime_distribution_thresholds ~sample_count
      @ runtime_pair_thresholds ~sample_count)
    coverage

let expected_observation_to_string scenario = function
  | `Body body -> Printf.sprintf "body:%S" body
  | `Body_prefix body -> Printf.sprintf "body-prefix:%S" body
  | `Body_error when Model.rejects_before_body_consumer scenario ->
      "body-error(pre-consumer)"
  | `Body_error -> "body-error"
  | `Exception_preserved -> "exception-preserved"
  | `No_body_required -> "no-body-required"

let failure_report ~target_name scenario expected observed =
  String.concat ~sep:"\n"
    [
      Printf.sprintf "%s observation mismatch" target_name;
      "runtime-http-replay:";
      Printf.sprintf "scenario: %s" scenario.Model.name;
      Printf.sprintf "method: %s" (Model.method_to_string scenario.method_);
      Printf.sprintf "status: %d" scenario.status;
      Printf.sprintf "framing: %s" (Model.framing_to_string scenario.framing);
      Printf.sprintf "connection-behavior: %s"
        (Model.connection_to_string scenario.connection);
      Printf.sprintf "consumption-mode: %s"
        (Model.consume_to_string scenario.consume);
      Printf.sprintf "expected-observation: %s"
        (expected_observation_to_string scenario expected);
      Printf.sprintf "observed-target-result: %s"
        (Model.observed_to_string observed);
      "copyable-replay-fixture:";
      Runtime_http_replay.scenario_to_replay_fixture scenario;
      Printf.sprintf "scenario-summary: %s" (Model.to_string scenario);
    ]

let assert_contains ~label ~substring text =
  Alcotest.(check bool) label true (String.is_substring text ~substring)

let test_failure_report_contains_replay_reduction_fields () =
  let scenario =
    Model.scenario ~name:"generated-get-500" ~method_:`GET ~status:500
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Close ~consume:(Read_once 2) ()
  in
  let report =
    failure_report ~target_name:"awskit-test" scenario
      (Model.expected_observation scenario)
      (Model.Observed_body "he")
  in
  List.iter
    [
      ("scenario name", "scenario: generated-get-500");
      ("method", "method: GET");
      ("status", "status: 500");
      ("framing", "framing: content-length declared=5 actual=5");
      ("connection behavior", "connection-behavior: close");
      ("consumption mode", "consumption-mode: read-once(2)");
      ("expected observation", "expected-observation: body-prefix:\"he\"");
      ("observed target result", "observed-target-result: body(2):\"he\"");
      ("copyable replay", "copyable-replay-fixture:");
      ("copyable framing payload", "framing=content-length 5 h68656c6c6f");
    ]
    ~f:(fun (label, substring) -> assert_contains ~label ~substring report)

let fail_observation ~target_name scenario expected observed =
  QCheck.Test.fail_reportf "%s"
    (failure_report ~target_name scenario expected observed)

(* `Body_prefix` carries the maximum bytes a single read may return; streaming
   runtimes may legally return a shorter non-empty prefix of that bound. *)
let valid_body_prefix ~max_prefix actual =
  String.is_prefix max_prefix ~prefix:actual
  && (String.is_empty max_prefix || not (String.is_empty actual))

let test_body_prefix_allows_bounded_short_read () =
  Alcotest.(check bool)
    "exact max prefix accepted" true
    (valid_body_prefix ~max_prefix:"cbb_-b" "cbb_-b");
  Alcotest.(check bool)
    "short streaming prefix accepted" true
    (valid_body_prefix ~max_prefix:"cbb_-b" "cbb_-");
  Alcotest.(check bool)
    "wrong prefix rejected" false
    (valid_body_prefix ~max_prefix:"cbb_-b" "cbb_+");
  Alcotest.(check bool)
    "too-long prefix rejected" false
    (valid_body_prefix ~max_prefix:"cbb_-" "cbb_-b");
  Alcotest.(check bool)
    "empty read rejected when body available" false
    (valid_body_prefix ~max_prefix:"cbb_-" "");
  Alcotest.(check bool)
    "empty read accepted when modeled prefix is empty" true
    (valid_body_prefix ~max_prefix:"" "")

let test_malformed_chunked_oracle_requires_body_error () =
  let scenario =
    Model.scenario ~name:"oracle-malformed-chunked" ~method_:`GET ~status:200
      ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n") ~connection:Close
      ()
  in
  Alcotest.(check string)
    "malformed chunked is not a successful body" "body-error"
    (Model.expected_body_to_string scenario.expected_body);
  Alcotest.(check string)
    "malformed chunked read-all expects body error" "body-error"
    (expected_observation_to_string scenario
       (Model.expected_observation scenario))

let test_bodiless_oracle_applies_body_length_precedence () =
  let conflicting =
    Model.scenario ~name:"oracle-bodiless-conflicting-framing" ~method_:`HEAD
      ~status:200
      ~framing:
        (Conflicting_length_and_chunked { declared = 5; chunks = [ "hi" ] })
      ~connection:Close ()
  in
  let strict_metadata =
    Model.scenario ~name:"oracle-bodiless-strict-content-length" ~method_:`HEAD
      ~status:200
      ~framing:
        (Duplicate_content_length { first = 5; second = 6; actual = "hello" })
      ~connection:Close ()
  in
  let informational =
    Model.scenario ~name:"oracle-100-content-length" ~method_:`GET ~status:100
      ~framing:(Content_length { declared = 5; actual = "hello" })
      ~connection:Keep_alive ()
  in
  Alcotest.(check string)
    "parseable framing conflict ignored for bodiless response" "no-body"
    (Model.expected_body_to_string conflicting.expected_body);
  Alcotest.(check bool)
    "parseable framing conflict is not pre-consumer metadata failure" false
    (Model.rejects_before_body_consumer conflicting);
  Alcotest.(check string)
    "strict content-length metadata still rejects" "body-error"
    (Model.expected_body_to_string strict_metadata.expected_body);
  Alcotest.(check bool)
    "strict content-length metadata rejects before consumer" true
    (Model.rejects_before_body_consumer strict_metadata);
  Alcotest.(check string)
    "informational response is bodiless" "no-body"
    (Model.expected_body_to_string informational.expected_body)

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
    List.map (Runtime_http_replay.all ()) ~f:(fun replay ->
        Alcotest.test_case replay.path `Quick (fun () ->
            ignore
              (check_scenario ~target_name:Target.name Target.run_scenario
                 replay.scenario
                : bool)))

  let generated_case =
    let count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:250 in
    QCheck.Test.make ~count ~name:"generated scenarios"
      (QCheck.make ~print:Model.to_string ~shrink:shrink_scenario scenario_gen)
      (check_scenario ~target_name:Target.name Target.run_scenario)
    |> QCheck_alcotest.to_alcotest ~speed_level:`Quick

  let suite =
    [
      ( Printf.sprintf "workload:%s:runtime-http" Target.name,
        deterministic_cases
        @ replay_cases
        @ [
            Alcotest.test_case "scenario shrinker" `Quick
              test_scenario_shrinker_emits_recomputed_candidates;
            Alcotest.test_case "failure report replay fields" `Quick
              test_failure_report_contains_replay_reduction_fields;
            Alcotest.test_case "body prefix streaming bounds" `Quick
              test_body_prefix_allows_bounded_short_read;
            Alcotest.test_case "malformed chunked oracle" `Quick
              test_malformed_chunked_oracle_requires_body_error;
            Alcotest.test_case "bodiless oracle precedence" `Quick
              test_bodiless_oracle_applies_body_length_precedence;
            Alcotest.test_case "generator semantic coverage" `Quick
              test_generator_coverage;
            generated_case;
          ] );
    ]
end
