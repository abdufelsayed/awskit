open Awskit_s3
open Base

let test_time =
  Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0))
  |> Option.value ~default:Ptime.epoch

let creds =
  Credentials.make ~access_key_id:"AKID" ~secret_access_key:"SECRET" ()

let test_presigned_session_token () =
  let creds =
    Credentials.make ~access_key_id:"AKID" ~secret_access_key:"SECRET"
      ~session_token:"TOKEN" ()
  in
  let endpoint = Endpoint.https ~host:"s3.us-east-1.amazonaws.com" () in
  match
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~endpoint ~bucket:"bucket" ~key:"file.txt" ()
  with
  | Error (`Invalid_request msg) -> Alcotest.failf "presigned get: %s" msg
  | Ok url ->
      Alcotest.(check bool)
        "has session token query param" true
        (String.is_substring url ~substring:"X-Amz-Security-Token=TOKEN")

let test_presigned_put_signs_content_type () =
  let endpoint = Endpoint.https ~host:"s3.us-east-1.amazonaws.com" () in
  match
    Presigned.put_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~endpoint ~bucket:"bucket" ~key:"file.txt" ~content_type:"text/csv" ()
  with
  | Error (`Invalid_request msg) -> Alcotest.failf "presigned put: %s" msg
  | Ok url ->
      Alcotest.(check bool)
        "signed headers include content-type" true
        (String.is_substring url
           ~substring:"X-Amz-SignedHeaders=content-type%3Bhost");
      Alcotest.(check bool)
        "content type is not leaked as a query parameter" false
        (String.is_substring url ~substring:"Content-Type=")

let test_presigned_endpoint_scheme () =
  let endpoint = Endpoint.http ~host:"localhost" ~port:9000 () in
  match
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~endpoint ~bucket:"bucket" ~key:"file.txt" ()
  with
  | Error (`Invalid_request msg) -> Alcotest.failf "presigned get: %s" msg
  | Ok url ->
      Alcotest.(check bool)
        "uses explicit endpoint scheme" true
        (String.is_prefix url ~prefix:"http://localhost:9000/")

let suite =
  [
    ( "core:presigned",
      [
        Alcotest.test_case "session token query" `Quick
          test_presigned_session_token;
        Alcotest.test_case "put signs content-type" `Quick
          test_presigned_put_signs_content_type;
        Alcotest.test_case "explicit endpoint scheme" `Quick
          test_presigned_endpoint_scheme;
      ] );
  ]
