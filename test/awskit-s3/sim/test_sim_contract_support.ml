open Awskit_s3
module Simulator = Awskit_s3_sim

let test_time = Ptime.epoch

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let bucket_name value = Bucket_name.of_string_exn value
let object_key value = Object_key.of_string_exn value

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let make_simulator ?(bucket = "test-bucket") () =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  ignore
    (Simulator.Bucket.create conn ~bucket:(bucket_name bucket) ()
    |> ok_or_fail "create bucket");
  Simulator.clear_observations conn;
  conn

let is_body_error error =
  match Awskit.Error.kind error with Body _ -> true | _ -> false

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let expect_body_error label = function
  | Error error when is_body_error error -> ()
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body error" label

let expect_validation_field label field = function
  | Error error when is_validation_field field error -> ()
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation field %s" label field

let expect_service_code label code = function
  | Error error when Awskit.Error.service_code error = Some code -> ()
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected service error %s" label code

let put_string conn key contents =
  Simulator.Object.put_string conn
    ~bucket:(bucket_name "test-bucket")
    ~key:(object_key key) ~contents ()
  |> ok_or_fail ("put " ^ key)
