open Awskit_s3
open Awskit_s3_test

module Simulator_subject = struct
  include Simulator

  type connection = t
  type request_body = Runtime.request_body
  type response_body_reader = Runtime.response_body_reader

  let fresh () =
    let clock = Clock.create ~now:test_time () in
    let store = create_store ~clock () in
    connect store ~credentials

  let request_body_of_string = Runtime.Request_body.of_string
  let read_response_body = Runtime.Response_body.read
end

module Simulator_contract = S3_contract.Make (Simulator_subject)

let test_simulator_slow_down_fault () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:"contract-bucket" ()
    |> ok_or_fail "create bucket");
  Simulator.inject_fault conn Simulator.Slow_down;
  (match
     Simulator.Object.put_string conn ~bucket:"contract-bucket" ~key:"fault"
       "body"
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected fault: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected injected SlowDown");
  match Simulator.history (Simulator.store conn) with
  | [ record ] ->
      Alcotest.(check bool) "faulted" true record.faulted;
      Alcotest.(check string) "fault key" "fault" (Option.get record.key)
  | _ -> Alcotest.fail "expected one faulted history record"

let test_simulator_response_lost_fault () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:"contract-bucket" ()
    |> ok_or_fail "create bucket");
  ignore
    (Simulator.Object.put_string conn ~bucket:"contract-bucket" ~key:"body"
       "abcdef"
    |> ok_or_fail "put body");
  Simulator.inject_fault conn Simulator.Response_lost;
  match
    Simulator.Object.get_as_string conn ~bucket:"contract-bucket" ~key:"body"
      ~max_bytes:16L ()
  with
  | Error (Awskit.Error.Body _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected response-lost body error"

let suite =
  [
    ("simulator contract", Simulator_contract.cases);
    ( "simulator faults",
      [
        Alcotest.test_case "slow down" `Quick test_simulator_slow_down_fault;
        Alcotest.test_case "response lost" `Quick
          test_simulator_response_lost_fault;
      ] );
  ]
