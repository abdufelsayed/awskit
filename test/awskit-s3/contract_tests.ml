open Awskit_s3

let test_time = Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.get

let credentials =
  Credentials.create_exn ~access_key_id:"AKID" ~secret_access_key:"SECRET" ()

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Error.pp error

let expect_not_found label = function
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected not found" label

let tag key value = { Tag.key; value }

module type SUBJECT = sig
  include S with type 'a io := 'a

  val fresh : unit -> connection
  val upload_body_of_string : string -> upload_body

  val read :
    download_reader -> bytes -> off:int -> len:int -> (int, Error.t) result
end

module Make (Client : SUBJECT) = struct
  let bucket = "contract-bucket"

  let create_bucket conn =
    ignore (Client.Bucket.create conn ~bucket () |> ok_or_fail "create bucket")

  let put_string conn key value =
    ignore
      (Client.Object.Buffer.put_string conn ~bucket ~key value
      |> ok_or_fail ("put " ^ key))

  let test_bucket_lifecycle () =
    let conn = Client.fresh () in
    Alcotest.(check bool)
      "missing bucket" false
      (Client.Bucket.exists conn ~bucket |> ok_or_fail "exists missing");
    create_bucket conn;
    let head = Client.Bucket.head conn ~bucket |> ok_or_fail "head bucket" in
    Alcotest.(check string) "bucket name" bucket head.name;
    Alcotest.(check bool)
      "created bucket" true
      (Client.Bucket.exists conn ~bucket |> ok_or_fail "exists present");
    let buckets = Client.Bucket.list conn |> ok_or_fail "list buckets" in
    Alcotest.(check (list string))
      "bucket list" [ bucket ]
      (List.map (fun (info : Bucket.info) -> info.name) buckets);
    let location =
      Client.Bucket.get_location conn ~bucket |> ok_or_fail "location"
    in
    Alcotest.(check (option string))
      "location" (Some "us-east-1")
      (Option.map Region.to_string location);
    ignore (Client.Bucket.delete conn ~bucket |> ok_or_fail "delete bucket");
    Alcotest.(check bool)
      "deleted bucket" false
      (Client.Bucket.exists conn ~bucket |> ok_or_fail "exists deleted")

  let test_object_buffer_lifecycle () =
    let conn = Client.fresh () in
    create_bucket conn;
    let options =
      {
        Object.Put.default_options with
        content_type = Some "text/plain";
        metadata = [ ("origin", "contract") ];
        tags = [ tag "env" "test" ];
      }
    in
    let put =
      Client.Object.Buffer.put_string conn ~bucket ~key:"hello.txt" ~options
        "hello"
      |> ok_or_fail "put object"
    in
    Alcotest.(check bool) "put etag" true (Option.is_some put.etag);
    let info, body =
      Client.Object.Buffer.get_string conn ~bucket ~key:"hello.txt"
        ~max_size:16L ()
      |> ok_or_fail "get object"
    in
    Alcotest.(check string) "body" "hello" body;
    Alcotest.(check (option string))
      "content type" (Some "text/plain") info.content_type;
    Alcotest.(check (option string))
      "metadata" (Some "contract")
      (List.assoc_opt "origin" info.metadata);
    let head =
      Client.Object.head conn ~bucket ~key:"hello.txt" () |> ok_or_fail "head"
    in
    Alcotest.(check (option int64))
      "content length" (Some 5L) head.content_length;
    Alcotest.(check bool)
      "object exists" true
      (Client.Object.exists conn ~bucket ~key:"hello.txt" |> ok_or_fail "exists");
    ignore
      (Client.Object.delete conn ~bucket ~key:"hello.txt" ()
      |> ok_or_fail "delete");
    Alcotest.(check bool)
      "object deleted" false
      (Client.Object.exists conn ~bucket ~key:"hello.txt"
      |> ok_or_fail "exists deleted")

  let test_streaming_get () =
    let conn = Client.fresh () in
    create_bucket conn;
    ignore
      (Client.Object.put conn ~bucket ~key:"stream.txt"
         ~body:(Client.upload_body_of_string "abcdef")
         ()
      |> ok_or_fail "put streaming");
    let consume reader =
      let bytes = Bytes.create 4 in
      match Client.read reader bytes ~off:0 ~len:4 with
      | Error _ as error -> error
      | Ok read -> Ok (Bytes.sub_string bytes 0 read)
    in
    let _info, body =
      Client.Object.get conn ~bucket ~key:"stream.txt" ~consume ()
      |> ok_or_fail "get streaming"
    in
    Alcotest.(check string) "partial body" "abcd" body

  let test_list_copy_delete_many () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "logs/a.txt" "a";
    put_string conn "logs/b.txt" "b";
    put_string conn "other.txt" "other";
    ignore
      (Client.Object.copy conn ~src_bucket:bucket ~src_key:"logs/a.txt"
         ~dst_bucket:bucket ~dst_key:"copy/a.txt" ()
      |> ok_or_fail "copy");
    let list_options =
      {
        Object.List.default_options with
        prefix = Some "logs/";
        max_keys = Some 1;
      }
    in
    let page =
      Client.Object.list conn ~bucket ~options:list_options ()
      |> ok_or_fail "list"
    in
    Alcotest.(check int) "listed count" 1 (List.length page.objects);
    Alcotest.(check bool) "truncated" true page.is_truncated;
    let delete_object key =
      {
        Object.Delete_many.key;
        version_id = None;
        etag = None;
        last_modified_time = None;
        size = None;
      }
    in
    ignore
      (Client.Object.delete_many conn ~bucket
         ~objects:[ delete_object "logs/a.txt"; delete_object "logs/b.txt" ]
      |> ok_or_fail "delete many");
    let keys = Client.Object.list_keys conn ~bucket () |> ok_or_fail "keys" in
    Alcotest.(check (list string))
      "remaining keys"
      [ "copy/a.txt"; "other.txt" ]
      keys

  let test_object_tagging () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "tagged.txt" "body";
    let tags = [ tag "env" "dev"; tag "owner" "sdk" ] in
    ignore
      (Client.Object.Tagging.put conn ~bucket ~key:"tagged.txt" tags
      |> ok_or_fail "put object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:"tagged.txt"
      |> ok_or_fail "get object tags"
    in
    Alcotest.(check int) "tag count" 2 (List.length result.tags);
    ignore
      (Client.Object.Tagging.delete conn ~bucket ~key:"tagged.txt"
      |> ok_or_fail "delete object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:"tagged.txt"
      |> ok_or_fail "get deleted tags"
    in
    Alcotest.(check int) "deleted tag count" 0 (List.length result.tags)

  let sample_policy_json =
    Fmt.str
      {|{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":"*","Action":"s3:GetObject","Resource":"arn:aws:s3:::%s/*"}]}|}
      bucket

  let sample_policy = Policy.of_json sample_policy_json |> ok_or_fail "policy"

  let test_bucket_config_roundtrips () =
    let conn = Client.fresh () in
    create_bucket conn;
    ignore
      (Client.Bucket.Policy.put conn ~bucket sample_policy
      |> ok_or_fail "put policy");
    let policy = Client.Bucket.Policy.get conn ~bucket |> ok_or_fail "policy" in
    Alcotest.(check string) "policy" sample_policy_json (Policy.to_json policy);
    ignore
      (Client.Bucket.Policy.delete conn ~bucket |> ok_or_fail "delete policy");
    expect_not_found "deleted policy" (Client.Bucket.Policy.get conn ~bucket);
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         Bucket.Versioning.Status.Enabled
      |> ok_or_fail "put versioning");
    let versioning =
      Client.Bucket.Versioning.get conn ~bucket |> ok_or_fail "get versioning"
    in
    Alcotest.(check bool)
      "versioning enabled" true
      (versioning.status = Some Bucket.Versioning.Status.Enabled);
    let bucket_tags = [ tag "team" "storage" ] in
    ignore
      (Client.Bucket.Tagging.put conn ~bucket bucket_tags
      |> ok_or_fail "put bucket tags");
    let result =
      Client.Bucket.Tagging.get conn ~bucket |> ok_or_fail "get bucket tags"
    in
    Alcotest.(check int) "bucket tag count" 1 (List.length result.tags);
    ignore
      (Client.Bucket.Tagging.delete conn ~bucket
      |> ok_or_fail "delete bucket tags");
    let result =
      Client.Bucket.Tagging.get conn ~bucket |> ok_or_fail "get deleted tags"
    in
    Alcotest.(check int) "deleted bucket tag count" 0 (List.length result.tags);
    let encryption =
      {
        Bucket.Encryption.rules =
          [
            {
              Bucket.Encryption.Rule.sse_algorithm =
                Bucket.Encryption.Algorithm.Aes256;
              kms_master_key_id = None;
            };
          ];
      }
    in
    ignore
      (Client.Bucket.Encryption.put conn ~bucket encryption
      |> ok_or_fail "put encryption");
    let result =
      Client.Bucket.Encryption.get conn ~bucket |> ok_or_fail "get encryption"
    in
    Alcotest.(check int)
      "encryption rule count" 1
      (List.length result.config.rules);
    ignore
      (Client.Bucket.Encryption.delete conn ~bucket
      |> ok_or_fail "delete encryption");
    expect_not_found "deleted encryption"
      (Client.Bucket.Encryption.get conn ~bucket);
    let cors =
      {
        Bucket.Cors.rules =
          [
            {
              Bucket.Cors.id = Some "web";
              allowed_origins = [ "https://example.com" ];
              allowed_methods = [ Bucket.Cors.Method.Get ];
              allowed_headers = [ "*" ];
              expose_headers = [ "etag" ];
              max_age_seconds = Some 300;
            };
          ];
      }
    in
    ignore (Client.Bucket.Cors.put conn ~bucket cors |> ok_or_fail "put cors");
    let result = Client.Bucket.Cors.get conn ~bucket |> ok_or_fail "get cors" in
    Alcotest.(check int) "cors rule count" 1 (List.length result.config.rules);
    ignore (Client.Bucket.Cors.delete conn ~bucket |> ok_or_fail "delete cors");
    expect_not_found "deleted cors" (Client.Bucket.Cors.get conn ~bucket);
    let website =
      {
        Bucket.Website.index_document_suffix = Some "index.html";
        error_document_key = Some "error.html";
      }
    in
    ignore
      (Client.Bucket.Website.put conn ~bucket website
      |> ok_or_fail "put website");
    let result =
      Client.Bucket.Website.get conn ~bucket |> ok_or_fail "get website"
    in
    Alcotest.(check (option string))
      "website index" (Some "index.html") result.config.index_document_suffix;
    ignore
      (Client.Bucket.Website.delete conn ~bucket |> ok_or_fail "delete website");
    expect_not_found "deleted website" (Client.Bucket.Website.get conn ~bucket);
    let public_access_block =
      {
        Bucket.Public_access_block.block_public_acls = true;
        ignore_public_acls = false;
        block_public_policy = true;
        restrict_public_buckets = false;
      }
    in
    ignore
      (Client.Bucket.Public_access_block.put conn ~bucket public_access_block
      |> ok_or_fail "put public access block");
    let result =
      Client.Bucket.Public_access_block.get conn ~bucket
      |> ok_or_fail "get public access block"
    in
    Alcotest.(check bool)
      "block public acls" true result.config.block_public_acls;
    ignore
      (Client.Bucket.Public_access_block.delete conn ~bucket
      |> ok_or_fail "delete public access block");
    expect_not_found "deleted public access block"
      (Client.Bucket.Public_access_block.get conn ~bucket);
    let ownership =
      {
        Bucket.Ownership_controls.object_ownership =
          Bucket.Ownership_controls.Object_ownership.Bucket_owner_enforced;
      }
    in
    ignore
      (Client.Bucket.Ownership_controls.put conn ~bucket ownership
      |> ok_or_fail "put ownership");
    let result =
      Client.Bucket.Ownership_controls.get conn ~bucket
      |> ok_or_fail "get ownership"
    in
    Alcotest.(check string)
      "ownership" "BucketOwnerEnforced"
      (Bucket.Ownership_controls.Object_ownership.to_string
         result.config.object_ownership);
    ignore
      (Client.Bucket.Ownership_controls.delete conn ~bucket
      |> ok_or_fail "delete ownership");
    expect_not_found "deleted ownership"
      (Client.Bucket.Ownership_controls.get conn ~bucket);
    ignore
      (Client.Bucket.Request_payment.put conn ~bucket
         Bucket.Request_payment.Payer.Requester
      |> ok_or_fail "put request payment");
    let result =
      Client.Bucket.Request_payment.get conn ~bucket
      |> ok_or_fail "get request payment"
    in
    Alcotest.(check bool)
      "request payment" true
      (result.payer = Some Bucket.Request_payment.Payer.Requester);
    ignore
      (Client.Bucket.Accelerate.put conn ~bucket
         Bucket.Accelerate.Status.Enabled
      |> ok_or_fail "put accelerate");
    let result =
      Client.Bucket.Accelerate.get conn ~bucket |> ok_or_fail "get accelerate"
    in
    Alcotest.(check bool)
      "accelerate" true
      (result.status = Some Bucket.Accelerate.Status.Enabled);
    let policy_status =
      Client.Bucket.Policy_status.get conn ~bucket |> ok_or_fail "policy status"
    in
    Alcotest.(check (option bool))
      "policy status" (Some false) policy_status.is_public;
    let logging =
      Bucket.Logging.enabled ~target_bucket:"log-bucket" ~target_prefix:"logs/"
    in
    ignore
      (Client.Bucket.Logging.put conn ~bucket logging
      |> ok_or_fail "put logging");
    let result =
      Client.Bucket.Logging.get conn ~bucket |> ok_or_fail "get logging"
    in
    match result.config.logging with
    | Some target ->
        Alcotest.(check string)
          "logging bucket" "log-bucket" target.target_bucket;
        Alcotest.(check string) "logging prefix" "logs/" target.target_prefix
    | None -> Alcotest.fail "expected logging target"

  let test_buffer_limit () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "large.txt" "abcdef";
    match
      Client.Object.Buffer.get_string conn ~bucket ~key:"large.txt" ~max_size:3L
        ()
    with
    | Error (Awskit.Error.Body _) -> ()
    | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected max_size failure"

  let cases =
    [
      Alcotest.test_case "bucket lifecycle" `Quick test_bucket_lifecycle;
      Alcotest.test_case "object buffer lifecycle" `Quick
        test_object_buffer_lifecycle;
      Alcotest.test_case "streaming get" `Quick test_streaming_get;
      Alcotest.test_case "list copy delete many" `Quick
        test_list_copy_delete_many;
      Alcotest.test_case "object tagging" `Quick test_object_tagging;
      Alcotest.test_case "bucket config roundtrips" `Quick
        test_bucket_config_roundtrips;
      Alcotest.test_case "buffer limit" `Quick test_buffer_limit;
    ]
end

module Sim_subject = struct
  include Sim

  type connection = t
  type upload_body = Runtime.upload_body
  type download_reader = Runtime.download_reader

  let fresh () =
    let clock = Clock.create ~now:test_time () in
    let store = create_store ~clock () in
    connect store ~credentials

  let upload_body_of_string = Runtime.string_body
  let read = Runtime.read
end

module Sim_contract = Make (Sim_subject)

let test_sim_slow_down_fault () =
  let conn = Sim_subject.fresh () in
  ignore
    (Sim.Bucket.create conn ~bucket:"contract-bucket" ()
    |> ok_or_fail "create bucket");
  Sim.inject_fault conn Sim.Slow_down;
  (match
     Sim.Object.Buffer.put_string conn ~bucket:"contract-bucket" ~key:"fault"
       "body"
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected fault: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected injected SlowDown");
  match Sim.history (Sim.store conn) with
  | [ record ] ->
      Alcotest.(check bool) "faulted" true record.faulted;
      Alcotest.(check string) "fault key" "fault" (Option.get record.key)
  | _ -> Alcotest.fail "expected one faulted history record"

let test_sim_response_lost_fault () =
  let conn = Sim_subject.fresh () in
  ignore
    (Sim.Bucket.create conn ~bucket:"contract-bucket" ()
    |> ok_or_fail "create bucket");
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"contract-bucket" ~key:"body"
       "abcdef"
    |> ok_or_fail "put body");
  Sim.inject_fault conn Sim.Response_lost;
  match
    Sim.Object.Buffer.get_string conn ~bucket:"contract-bucket" ~key:"body"
      ~max_size:16L ()
  with
  | Error (Awskit.Error.Body _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected response-lost body error"

let suite =
  [
    ("sim contract", Sim_contract.cases);
    ( "sim faults",
      [
        Alcotest.test_case "slow down" `Quick test_sim_slow_down_fault;
        Alcotest.test_case "response lost" `Quick test_sim_response_lost_fault;
      ] );
  ]
