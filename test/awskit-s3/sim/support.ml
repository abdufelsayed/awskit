open Awskit_s3_test
module Simulator = Awskit_s3_sim

let make_simulator () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials:creds in
  ignore
    (Simulator.Bucket.create conn ~bucket:"test-bucket" ()
    |> ok_or_fail "bucket");
  conn
