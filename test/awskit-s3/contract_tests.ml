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

let expect_status label status = function
  | Error (Awskit.Error.Service service) when service.status = status -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected service status %d" label status

let expect_precondition_failed label result = expect_status label 412 result
let expect_not_modified label result = expect_status label 304 result

let expect_validation label = function
  | Error (Awskit.Error.Validation _) -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let check_checksum label algorithm value = function
  | None -> Alcotest.failf "%s: expected checksum" label
  | Some (checksum : Object.Checksum.response) ->
      Alcotest.(check bool)
        (label ^ " algorithm") true
        (checksum.algorithm = algorithm);
      Alcotest.(check string) (label ^ " value") value checksum.value

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
        checksum =
          Some
            ({ Object.Checksum.algorithm = `SHA256; value = None }
              : Object.Checksum.request);
      }
    in
    let put =
      Client.Object.Buffer.put_string conn ~bucket ~key:"hello.txt" ~options
        "hello"
      |> ok_or_fail "put object"
    in
    Alcotest.(check bool) "put etag" true (Option.is_some put.etag);
    check_checksum "put checksum" `SHA256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
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
    check_checksum "get checksum" `SHA256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" info.checksum;
    let head =
      Client.Object.head conn ~bucket ~key:"hello.txt" () |> ok_or_fail "head"
    in
    Alcotest.(check (option int64))
      "content length" (Some 5L) head.content_length;
    check_checksum "head checksum" `SHA256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" head.checksum;
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

  let test_large_streaming_roundtrip () =
    let conn = Client.fresh () in
    create_bucket conn;
    let chunk = String.make 8192 'x' in
    let chunks = 320 in
    let body = String.concat "" (List.init chunks (fun _ -> chunk)) in
    ignore
      (Client.Object.put conn ~bucket ~key:"large-stream.bin"
         ~body:(Client.upload_body_of_string body)
         ()
      |> ok_or_fail "put large stream");
    let consume reader =
      let bytes = Bytes.create 16384 in
      let rec loop total first last =
        match Client.read reader bytes ~off:0 ~len:(Bytes.length bytes) with
        | Error _ as error -> error
        | Ok 0 -> Ok (total, first, last)
        | Ok read ->
            let first =
              match first with
              | Some _ -> first
              | None -> Some (Bytes.get bytes 0)
            in
            let last = Some (Bytes.get bytes (read - 1)) in
            loop (total + read) first last
      in
      loop 0 None None
    in
    let info, (length, first, last) =
      Client.Object.get conn ~bucket ~key:"large-stream.bin" ~consume ()
      |> ok_or_fail "get large stream"
    in
    Alcotest.(check int) "large stream bytes" (String.length body) length;
    Alcotest.(check (option char)) "first byte" (Some 'x') first;
    Alcotest.(check (option char)) "last byte" (Some 'x') last;
    Alcotest.(check (option int64))
      "large content length"
      (Some (Int64.of_int (String.length body)))
      info.content_length

  let test_range_reads_and_metadata_copy () =
    let conn = Client.fresh () in
    create_bucket conn;
    let put_options =
      {
        Object.Put.default_options with
        content_type = Some "text/plain";
        metadata = [ ("origin", "source"); ("mode", "copy") ];
      }
    in
    ignore
      (Client.Object.Buffer.put_string conn ~bucket ~key:"range.txt"
         ~options:put_options "abcdefghij"
      |> ok_or_fail "put range source");
    let range_options =
      {
        Object.Get.default_options with
        range = Some (Range.bytes_exn ~start:2L ~finish:5L);
      }
    in
    let info, body =
      Client.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
        ~options:range_options ~max_size:16L ()
      |> ok_or_fail "get byte range"
    in
    Alcotest.(check string) "range body" "cdef" body;
    Alcotest.(check int)
      "range status" 206
      (Awskit.Response.status info.request);
    Alcotest.(check (option int64))
      "range content length" (Some 4L) info.content_length;
    Alcotest.(check (option string))
      "range content range" (Some "bytes 2-5/10")
      (Awskit.Response.header info.request "content-range");
    let suffix_options =
      { Object.Get.default_options with range = Some (Range.suffix_exn 3L) }
    in
    let _info, suffix =
      Client.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
        ~options:suffix_options ~max_size:16L ()
      |> ok_or_fail "get suffix range"
    in
    Alcotest.(check string) "suffix body" "hij" suffix;
    let invalid_range_options =
      { Object.Get.default_options with range = Some (Range.from_exn 99L) }
    in
    expect_status "invalid range" 416
      (Client.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
         ~options:invalid_range_options ~max_size:16L ());
    ignore
      (Client.Object.copy conn ~src_bucket:bucket ~src_key:"range.txt"
         ~dst_bucket:bucket ~dst_key:"copied.txt" ()
      |> ok_or_fail "copy metadata");
    let copied =
      Client.Object.head conn ~bucket ~key:"copied.txt" ()
      |> ok_or_fail "head copied"
    in
    Alcotest.(check (option string))
      "copied metadata" (Some "source")
      (List.assoc_opt "origin" copied.metadata);
    let replace_options =
      {
        Object.Copy.default_options with
        metadata = Some (`Replace [ ("origin", "replacement") ]);
      }
    in
    ignore
      (Client.Object.copy conn ~src_bucket:bucket ~src_key:"range.txt"
         ~dst_bucket:bucket ~dst_key:"replaced.txt" ~options:replace_options ()
      |> ok_or_fail "copy replace metadata");
    let replaced =
      Client.Object.head conn ~bucket ~key:"replaced.txt" ()
      |> ok_or_fail "head replaced"
    in
    Alcotest.(check (option string))
      "replaced metadata" (Some "replacement")
      (List.assoc_opt "origin" replaced.metadata);
    Alcotest.(check (option string))
      "removed copied metadata" None
      (List.assoc_opt "mode" replaced.metadata)

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

  let test_object_preconditions () =
    let conn = Client.fresh () in
    create_bucket conn;
    let put =
      Client.Object.Buffer.put_string conn ~bucket ~key:"conditional.txt" "body"
      |> ok_or_fail "put conditional"
    in
    let etag =
      match put.etag with
      | Some etag -> etag
      | None -> Alcotest.fail "expected put etag"
    in
    let bad_etag = Object.Etag.of_string_exn "\"bad\"" in
    let absent_options =
      {
        Object.Put.default_options with
        preconditions = Object.Preconditions.Write.if_absent;
      }
    in
    expect_precondition_failed "put if absent existing"
      (Client.Object.Buffer.put_string conn ~bucket ~key:"conditional.txt"
         ~options:absent_options "new-body");
    let match_options =
      {
        Object.Put.default_options with
        preconditions = Object.Preconditions.Write.if_etag bad_etag;
      }
    in
    expect_precondition_failed "put if match bad etag"
      (Client.Object.Buffer.put_string conn ~bucket ~key:"conditional.txt"
         ~options:match_options "new-body");
    let read_options =
      {
        Object.Get.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_match = Some (Object.Etag_condition.Etag bad_etag);
          };
      }
    in
    expect_precondition_failed "get if match bad etag"
      (Client.Object.Buffer.get_string conn ~bucket ~key:"conditional.txt"
         ~options:read_options ~max_size:16L ());
    let not_modified_options =
      {
        Object.Get.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_none_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    expect_not_modified "get if none match etag"
      (Client.Object.Buffer.get_string conn ~bucket ~key:"conditional.txt"
         ~options:not_modified_options ~max_size:16L ());
    let head_not_modified_options =
      {
        Object.Head.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_modified_since = Some test_time;
          };
      }
    in
    expect_not_modified "head if modified since same time"
      (Client.Object.head conn ~bucket ~key:"conditional.txt"
         ~options:head_not_modified_options ());
    let stale_options =
      {
        Object.Head.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_unmodified_since = Some Ptime.epoch;
          };
      }
    in
    expect_precondition_failed "head if unmodified since stale"
      (Client.Object.head conn ~bucket ~key:"conditional.txt"
         ~options:stale_options ());
    let copy_options =
      {
        Object.Copy.default_options with
        source_preconditions =
          {
            Object.Preconditions.Copy_source.none with
            if_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    ignore
      (Client.Object.copy conn ~src_bucket:bucket ~src_key:"conditional.txt"
         ~dst_bucket:bucket ~dst_key:"conditional-copy.txt"
         ~options:copy_options ()
      |> ok_or_fail "copy if match etag");
    let copy_fail_options =
      {
        Object.Copy.default_options with
        source_preconditions =
          {
            Object.Preconditions.Copy_source.none with
            if_none_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    expect_precondition_failed "copy if none match etag"
      (Client.Object.copy conn ~src_bucket:bucket ~src_key:"conditional.txt"
         ~dst_bucket:bucket ~dst_key:"conditional-copy-fail.txt"
         ~options:copy_fail_options ());
    let delete_key = "delete-conditional.txt" in
    let delete_put =
      Client.Object.Buffer.put_string conn ~bucket ~key:delete_key "abc"
      |> ok_or_fail "put delete conditional"
    in
    let delete_etag =
      match delete_put.etag with
      | Some etag -> etag
      | None -> Alcotest.fail "expected delete put etag"
    in
    let delete_options =
      {
        Object.Delete.default_options with
        preconditions =
          {
            Object.Preconditions.Delete.if_match =
              Some (Object.Etag_condition.Etag delete_etag);
            if_match_last_modified_time = Some test_time;
            if_match_size = Some 3L;
          };
      }
    in
    ignore
      (Client.Object.delete conn ~bucket ~key:delete_key ~options:delete_options
         ()
      |> ok_or_fail "delete preconditions");
    let delete_fail_key = "delete-conditional-fail.txt" in
    ignore
      (Client.Object.Buffer.put_string conn ~bucket ~key:delete_fail_key "abc"
      |> ok_or_fail "put delete fail conditional");
    let delete_fail_options =
      {
        Object.Delete.default_options with
        preconditions =
          { Object.Preconditions.Delete.none with if_match_size = Some 4L };
      }
    in
    expect_precondition_failed "delete if size mismatch"
      (Client.Object.delete conn ~bucket ~key:delete_fail_key
         ~options:delete_fail_options ());
    expect_precondition_failed "delete missing with precondition"
      (Client.Object.delete conn ~bucket ~key:"missing-delete-conditional.txt"
         ~options:delete_fail_options ())

  let test_multipart_lifecycle () =
    let conn = Client.fresh () in
    create_bucket conn;
    let upload_options =
      {
        Multipart.Create.default_options with
        checksum =
          Some
            ({ Object.Checksum.algorithm = `SHA256; value = None }
              : Object.Checksum.request);
      }
    in
    let upload =
      Client.Multipart.create conn ~bucket ~key:"multi.bin"
        ~options:upload_options ()
      |> ok_or_fail "create multipart"
    in
    let upload_id = upload.upload.upload_id in
    let part_options =
      {
        Multipart.Upload_part.checksum =
          Some
            ({ Object.Checksum.algorithm = `SHA1; value = None }
              : Object.Checksum.request);
      }
    in
    let part1 =
      Client.Multipart.upload_part conn ~bucket ~key:"multi.bin" ~upload_id
        ~part_number:1
        ~body:(Client.upload_body_of_string "hello-")
        ~options:part_options ()
      |> ok_or_fail "upload part 1"
    in
    let part2 =
      Client.Multipart.upload_part conn ~bucket ~key:"multi.bin" ~upload_id
        ~part_number:2
        ~body:(Client.upload_body_of_string "world")
        ~options:part_options ()
      |> ok_or_fail "upload part 2"
    in
    let list_options =
      { Multipart.List_parts.default_options with max_parts = Some 1 }
    in
    let page =
      Client.Multipart.list_parts conn ~bucket ~key:"multi.bin" ~upload_id
        ~options:list_options ()
      |> ok_or_fail "list multipart parts"
    in
    Alcotest.(check int) "first page count" 1 (List.length page.parts);
    Alcotest.(check bool) "parts truncated" true page.is_truncated;
    (match page.parts with
    | [ part ] ->
        check_checksum "listed part checksum" `SHA1
          "99fOUhWeYfviXce2x4zfE3HfH+I=" part.checksum
    | _ -> Alcotest.fail "expected first multipart page");
    let parts =
      Client.Multipart.Paginator.parts conn ~bucket ~key:"multi.bin" ~upload_id
        ~options:list_options ()
      |> ok_or_fail "paginate multipart parts"
    in
    Alcotest.(check (list int))
      "part numbers" [ 1; 2 ]
      (List.map
         (fun (part : Multipart.List_parts.part_info) -> part.part_number)
         parts);
    let complete =
      Client.Multipart.complete conn ~bucket ~key:"multi.bin" ~upload_id
        [ part1.part; part2.part ]
      |> ok_or_fail "complete multipart"
    in
    Alcotest.(check bool) "complete etag" true (Option.is_some complete.etag);
    check_checksum "complete checksum" `SHA256
      "r6J7RNQ7Aqn+pB0TztwuQBbPz4fF2/mQ5ZNmmqjOKG0=" complete.checksum;
    let info, body =
      Client.Object.Buffer.get_string conn ~bucket ~key:"multi.bin"
        ~max_size:16L ()
      |> ok_or_fail "get completed multipart object"
    in
    Alcotest.(check string) "completed body" "hello-world" body;
    check_checksum "completed object checksum" `SHA256
      "r6J7RNQ7Aqn+pB0TztwuQBbPz4fF2/mQ5ZNmmqjOKG0=" info.checksum;
    let aborted =
      Client.Multipart.create conn ~bucket ~key:"abort.bin" ()
      |> ok_or_fail "create abort multipart"
    in
    let aborted_upload_id = aborted.upload.upload_id in
    ignore
      (Client.Multipart.upload_part conn ~bucket ~key:"abort.bin"
         ~upload_id:aborted_upload_id ~part_number:1
         ~body:(Client.upload_body_of_string "discarded")
         ()
      |> ok_or_fail "upload aborted part");
    ignore
      (Client.Multipart.abort conn ~bucket ~key:"abort.bin"
         ~upload_id:aborted_upload_id
      |> ok_or_fail "abort multipart");
    match
      Client.Multipart.list_parts conn ~bucket ~key:"abort.bin"
        ~upload_id:aborted_upload_id ()
    with
    | Error error when Error.service_code error = Some "NoSuchUpload" -> ()
    | Error error ->
        Alcotest.failf "unexpected abort list error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected aborted upload to be unavailable"

  let test_multipart_completion_edges () =
    let conn = Client.fresh () in
    create_bucket conn;
    let upload =
      Client.Multipart.create conn ~bucket ~key:"edges.bin" ()
      |> ok_or_fail "create edge multipart"
    in
    let upload_id = upload.upload.upload_id in
    let first =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:1
        ~body:(Client.upload_body_of_string "first")
        ()
      |> ok_or_fail "upload first part"
    in
    let second =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:2
        ~body:(Client.upload_body_of_string "second")
        ()
      |> ok_or_fail "upload second part"
    in
    let overwritten =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:1
        ~body:(Client.upload_body_of_string "FIRST")
        ()
      |> ok_or_fail "overwrite first part"
    in
    expect_status "complete with stale part etag" 400
      (Client.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
         [ first.part; second.part ]);
    expect_validation "complete with unsorted parts"
      (Client.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
         [ second.part; overwritten.part ]);
    ignore
      (Client.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
         [ overwritten.part; second.part ]
      |> ok_or_fail "complete overwritten parts");
    let _info, body =
      Client.Object.Buffer.get_string conn ~bucket ~key:"edges.bin"
        ~max_size:16L ()
      |> ok_or_fail "get edge multipart object"
    in
    Alcotest.(check string) "completed overwritten body" "FIRSTsecond" body;
    expect_status "complete already completed upload" 404
      (Client.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
         [ overwritten.part; second.part ])

  let test_managed_multipart_upload () =
    let conn = Client.fresh () in
    create_bucket conn;
    let part_size = Multipart.Managed.min_part_size in
    let body = String.make part_size 'a' ^ "end" in
    let options = { Multipart.Managed.default_options with part_size } in
    let result =
      Client.Multipart.Managed.upload_string conn ~bucket ~key:"managed.bin"
        ~options body
      |> ok_or_fail "managed multipart upload"
    in
    Alcotest.(check (list int))
      "managed part numbers" [ 1; 2 ]
      (List.map
         (fun (part : Multipart.Part.t) -> part.part_number)
         result.parts);
    let _info, stored =
      Client.Object.Buffer.get_string conn ~bucket ~key:"managed.bin"
        ~max_size:(Int64.of_int (String.length body + 1))
        ()
      |> ok_or_fail "get managed multipart"
    in
    Alcotest.(check int)
      "managed body size" (String.length body) (String.length stored);
    Alcotest.(check string)
      "managed body suffix" "end"
      (String.sub stored (String.length stored - 3) 3);
    match
      Client.Multipart.Managed.upload_string conn ~bucket ~key:"empty.bin" ""
    with
    | Error (Awskit.Error.Validation _) -> ()
    | Error error ->
        Alcotest.failf "unexpected empty body error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected empty managed multipart failure"

  let cases =
    [
      Alcotest.test_case "bucket lifecycle" `Quick test_bucket_lifecycle;
      Alcotest.test_case "object buffer lifecycle" `Quick
        test_object_buffer_lifecycle;
      Alcotest.test_case "streaming get" `Quick test_streaming_get;
      Alcotest.test_case "large streaming roundtrip" `Quick
        test_large_streaming_roundtrip;
      Alcotest.test_case "range reads and metadata copy" `Quick
        test_range_reads_and_metadata_copy;
      Alcotest.test_case "list copy delete many" `Quick
        test_list_copy_delete_many;
      Alcotest.test_case "object tagging" `Quick test_object_tagging;
      Alcotest.test_case "bucket config roundtrips" `Quick
        test_bucket_config_roundtrips;
      Alcotest.test_case "buffer limit" `Quick test_buffer_limit;
      Alcotest.test_case "object preconditions" `Quick test_object_preconditions;
      Alcotest.test_case "multipart lifecycle" `Quick test_multipart_lifecycle;
      Alcotest.test_case "multipart completion edges" `Quick
        test_multipart_completion_edges;
      Alcotest.test_case "managed multipart upload" `Quick
        test_managed_multipart_upload;
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
