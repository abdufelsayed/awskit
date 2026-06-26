let test_threshold_failures_report_observed_counts () =
  let coverage =
    Workload_coverage.empty
    |> Workload_coverage.add "http.method.get"
    |> Workload_coverage.add "http.method.get"
    |> Workload_coverage.add "http.method.head"
  in
  let failures =
    Workload_coverage.threshold_failures coverage
      ~thresholds:
        [
          Workload_coverage.threshold ~bin:"http.method.get" ~minimum:2;
          Workload_coverage.threshold ~bin:"http.method.head" ~minimum:2;
          Workload_coverage.threshold ~bin:"http.framing.chunked" ~minimum:1;
        ]
  in
  Alcotest.(check int) "failure count" 2 (List.length failures);
  Alcotest.(check (list string))
    "diagnostic lines"
    [
      "http.method.head observed=1 minimum=2";
      "http.framing.chunked observed=0 minimum=1";
    ]
    (Workload_coverage.threshold_failure_lines failures)

let test_adjacent_pairs_preserve_order () =
  let pairs =
    Workload_coverage.adjacent_pairs ~bin:Fun.id
      [ "put"; "head"; "delete"; "list" ]
  in
  Alcotest.(check (list string))
    "pairs"
    [ "put->head"; "head->delete"; "delete->list" ]
    pairs

let suite =
  [
    ( "contract:awskit-test:workload-coverage",
      [
        Alcotest.test_case "threshold diagnostics" `Quick
          test_threshold_failures_report_observed_counts;
        Alcotest.test_case "adjacent pairs" `Quick
          test_adjacent_pairs_preserve_order;
      ] );
  ]

let () = Alcotest.run "awskit-test-coverage" suite
