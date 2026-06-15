open Awskit_s3
open Awskit_s3_test
open Support

module Simulator_subject = struct
  type connection = Simulator.t
  type request_body = Simulator.Body.t
  type response_body_reader = Simulator.Reader.t

  module Runtime = Simulator.Runtime

  module Body = struct
    type 'a io = 'a

    include Simulator.Body
  end

  module Reader = struct
    type 'a io = 'a

    include Simulator.Reader
  end

  module Object = struct
    type connection = Simulator.t
    type 'a io = 'a
    type request_body = Body.t
    type response_body_reader = Reader.t

    include Simulator.Object
  end

  module Bucket = struct
    type connection = Simulator.t
    type 'a io = 'a

    include Simulator.Bucket
  end

  module Multipart = struct
    type connection = Simulator.t
    type 'a io = 'a
    type request_body = Body.t

    include Simulator.Multipart
  end

  module Presigned = struct
    type connection = Simulator.t
    type 'a io = 'a

    include Simulator.Presigned
  end

  let fresh () =
    let clock = Simulator.Clock.create ~now:test_time () in
    let store = Simulator.create_store ~clock () in
    Simulator.connect store ~credentials

  let read_response_body = Simulator.Reader.read
end

module Simulator_contract = S3_contract.Make (Simulator_subject)

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let test_simulator_slow_down_fault () =
  let conn = Simulator_subject.fresh () in
  ignore
    (Simulator.Bucket.create conn ~bucket:"contract-bucket" ()
    |> ok_or_fail "create bucket");
  Simulator.inject_fault conn Simulator.Slow_down;
  (match
     Simulator.Object.put conn ~bucket:"contract-bucket" ~key:"fault"
       ~body:(Simulator.Body.of_string "body")
       ()
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
    (Simulator.Object.put conn ~bucket:"contract-bucket" ~key:"body"
       ~body:(Simulator.Body.of_string "abcdef")
       ()
    |> ok_or_fail "put body");
  Simulator.inject_fault conn Simulator.Response_lost;
  match
    Simulator.Object.get conn ~bucket:"contract-bucket" ~key:"body"
      ~consume:(Simulator.Reader.to_string ~max_bytes:16L)
      ()
  with
  | Error error when is_body_error error -> ()
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
