open Awskit_s3
open Test_sim_contract_support
module Simulator = Awskit_s3_sim

let check_span label expected actual =
  Alcotest.(check bool) label true (Ptime.Span.equal expected actual)

let test_clock_advance_ms_preserves_subseconds () =
  let clock = Simulator.Clock.create () in
  Simulator.Clock.advance_ms clock 250;
  check_span "positive 250ms"
    (Ptime.Span.v (0, 250_000_000_000L))
    (Ptime.diff (Simulator.Clock.now clock) Ptime.epoch);
  Simulator.Clock.advance_ms clock 750;
  check_span "accumulated one second" (Ptime.Span.of_int_s 1)
    (Ptime.diff (Simulator.Clock.now clock) Ptime.epoch);
  Simulator.Clock.advance_ms clock (-250);
  check_span "negative 250ms"
    (Ptime.Span.v (0, 750_000_000_000L))
    (Ptime.diff (Simulator.Clock.now clock) Ptime.epoch)

let test_delete_objects_uses_validated_collection () =
  let conn = make_simulator () in
  ignore (put_string conn "first" "one");
  ignore (put_string conn "second" "two");
  let objects =
    [
      Object.Delete_many.object_ ~key:(object_key "first") ();
      Object.Delete_many.object_ ~key:(object_key "second") ();
    ]
    |> Object.Delete_many.Objects.of_list_exn
  in
  let result =
    Simulator.Object.delete_objects conn
      ~bucket:(bucket_name "test-bucket")
      ~objects ()
    |> ok_or_fail "delete validated collection"
  in
  Alcotest.(check int) "deleted members" 2 (List.length result.deleted);
  Alcotest.(check bool)
    "first absent" false
    (Simulator.Object.exists conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "first") ()
    |> ok_or_fail "first exists");
  Alcotest.(check bool)
    "second absent" false
    (Simulator.Object.exists conn
       ~bucket:(bucket_name "test-bucket")
       ~key:(object_key "second") ()
    |> ok_or_fail "second exists")

let suite =
  [
    ( "contract:awskit-s3-sim:parity",
      [
        Alcotest.test_case "clock advance_ms preserves subseconds" `Quick
          test_clock_advance_ms_preserves_subseconds;
        Alcotest.test_case "DeleteObjects validated collection" `Quick
          test_delete_objects_uses_validated_collection;
      ] );
  ]
