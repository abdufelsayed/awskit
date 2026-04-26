(** Integration tests against MinIO. Requires: docker compose up -d *)

open Awskit_s3_eio
module Error = Awskit_s3.Error
module Tag = Awskit_s3.Tag
module Range = Awskit_s3.Range

let test_prefix = ref 0

let fresh_prefix () =
  incr test_prefix;
  Fmt.str "test/%d/%d/" (Unix.getpid ()) !test_prefix

let bucket = "test-bucket"

let make_client env =
  create ~env ~region:"us-east-1"
    ~credentials:
      (Awskit_s3.Credentials.make ~access_key_id:"minioadmin"
         ~secret_access_key:"minioadmin" ())
    ~endpoint:(Awskit_s3.Endpoint.http ~host:"localhost" ~port:9000 ())
    ()

(* ── Helpers ────────────────────────────────────────────────────── *)

let error_t : Error.t Alcotest.testable = Alcotest.testable Error.pp Error.equal

let get_body_result =
  Alcotest.testable
    (fun fmt r ->
      match r with
      | Ok (v : Object.Get_result.t) -> Fmt.pf fmt "Ok(%S)" v.body
      | Error e -> Fmt.pf fmt "Error(%a)" Error.pp e)
    (fun a b ->
      match (a, b) with
      | Ok a, Ok b ->
          String.equal a.Object.Get_result.body b.Object.Get_result.body
      | Error a, Error b -> Error.equal a b
      | _ -> false)

let check_get_body msg expected actual =
  match actual with
  | Ok (r : Object.Get_result.t) -> Alcotest.(check string) msg expected r.body
  | Error e -> Alcotest.failf "%s: %a" msg Error.pp e

let check_error msg expected actual =
  match actual with
  | Ok _ -> Alcotest.failf "%s: expected error" msg
  | Error e -> Alcotest.check error_t msg expected e

let ignore_put r = ignore (r : (Object.Put_result.t, Error.t) result)

let list_all client ~prefix =
  let rec go acc token =
    match Object.list client ~bucket ~prefix ?continuation_token:token () with
    | Error e -> Error e
    | Ok r ->
        let acc = acc @ r.Object.List_result.keys in
        if r.is_truncated then go acc r.next_continuation_token else Ok acc
  in
  go [] None

(* ── Tests ──────────────────────────────────────────────────────── *)

let test_put_get env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "hello" in
  ignore_put (Object.put c ~bucket ~key "world");
  check_get_body "get" "world" (Object.get c ~bucket ~key ())

let test_put_overwrite env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "k" in
  ignore_put (Object.put c ~bucket ~key "v1");
  ignore_put (Object.put c ~bucket ~key "v2");
  check_get_body "latest" "v2" (Object.get c ~bucket ~key ())

let test_get_missing env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "nope" in
  check_error "not found" `Not_found (Object.get c ~bucket ~key ())

let test_head env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "k" in
  (match Object.head c ~bucket ~key () with
  | Ok None -> ()
  | _ -> Alcotest.fail "expected None before put");
  ignore_put (Object.put c ~bucket ~key "12345");
  match Object.head c ~bucket ~key () with
  | Ok (Some info) ->
      Alcotest.(check int) "size" 5 info.Object.Info.size;
      Alcotest.(check bool) "has etag" true (String.length info.etag > 0)
  | Ok None -> Alcotest.fail "expected Some"
  | Error e -> Alcotest.failf "%a" Error.pp e

let test_delete env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "k" in
  ignore_put (Object.put c ~bucket ~key "v");
  (match Object.delete c ~bucket ~key () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete: %a" Error.pp e);
  check_error "gone" `Not_found (Object.get c ~bucket ~key ());
  match Object.delete c ~bucket ~key () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete missing: %a" Error.pp e

let test_conditional_delete env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "conditional-delete" in
  ignore_put (Object.put c ~bucket ~key "v");
  let etag =
    match Object.get c ~bucket ~key () with
    | Ok result -> result.Object.Get_result.etag
    | Error e -> Alcotest.failf "get before delete: %a" Error.pp e
  in
  (match
     Object.delete c ~bucket ~key
       ~preconditions:(Object.Preconditions.Delete.if_etag etag)
       ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "conditional delete: %a" Error.pp e);
  check_error "gone" `Not_found (Object.get c ~bucket ~key ())

let test_cas env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "version" in
  Alcotest.(check bool)
    "first ok" true
    (Result.is_ok
       (Object.put c ~bucket ~key
          ~preconditions:Object.Preconditions.Write.if_absent "v1"));
  (match
     Object.put c ~bucket ~key
       ~preconditions:Object.Preconditions.Write.if_absent "v2"
   with
  | Error `Precondition_failed -> ()
  | Ok _ -> Alcotest.fail "expected 412"
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e);
  check_get_body "unchanged" "v1" (Object.get c ~bucket ~key ())

let test_list env () =
  let c = make_client env in
  let prefix = fresh_prefix () in
  ignore_put (Object.put c ~bucket ~key:(prefix ^ "a") "1");
  ignore_put (Object.put c ~bucket ~key:(prefix ^ "b") "2");
  ignore_put (Object.put c ~bucket ~key:(prefix ^ "c") "3");
  match list_all c ~prefix with
  | Ok keys ->
      Alcotest.(check (list string))
        "3 keys sorted"
        [ prefix ^ "a"; prefix ^ "b"; prefix ^ "c" ]
        keys
  | Error e -> Alcotest.failf "%a" Error.pp e

let test_list_pagination env () =
  let c = make_client env in
  let prefix = fresh_prefix () in
  for i = 1 to 5 do
    ignore_put (Object.put c ~bucket ~key:(Fmt.str "%s%03d" prefix i) "v")
  done;
  match Object.list c ~bucket ~prefix ~max_keys:2 () with
  | Ok r -> (
      Alcotest.(check int) "page size" 2 (List.length r.keys);
      Alcotest.(check bool) "truncated" true r.is_truncated;
      match list_all c ~prefix with
      | Ok all -> Alcotest.(check int) "all 5" 5 (List.length all)
      | Error e -> Alcotest.failf "%a" Error.pp e)
  | Error e -> Alcotest.failf "%a" Error.pp e

let test_etag env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "k" in
  match Object.put c ~bucket ~key "data" with
  | Ok r ->
      Alcotest.(check bool)
        "has etag" true
        (String.length r.Object.Put_result.etag > 0)
  | Error e -> Alcotest.failf "%a" Error.pp e

let test_copy env () =
  let c = make_client env in
  let prefix = fresh_prefix () in
  let src = prefix ^ "src" in
  let dst = prefix ^ "dst" in
  ignore_put (Object.put c ~bucket ~key:src "hello copy");
  (match
     Object.copy c ~src_bucket:bucket ~src_key:src ~dst_bucket:bucket
       ~dst_key:dst ()
   with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "copy: %a" Error.pp e);
  check_get_body "copy contents" "hello copy" (Object.get c ~bucket ~key:dst ())

let test_multipart env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "big" in
  match Multipart.create c ~bucket ~key () with
  | Error e -> Alcotest.failf "create: %a" Error.pp e
  | Ok upload -> (
      (* MinIO minimum part size is 5MB for non-final parts *)
      let part1_data = String.make (5 * 1024 * 1024) 'A' in
      let part2_data = "final" in
      match
        Multipart.upload_part c ~bucket ~key ~upload_id:upload.upload_id
          ~part_number:1 part1_data
      with
      | Ok p1 -> (
          match
            Multipart.upload_part c ~bucket ~key ~upload_id:upload.upload_id
              ~part_number:2 part2_data
          with
          | Ok p2 -> (
              match
                Multipart.complete c ~bucket ~key ~upload_id:upload.upload_id
                  [ p1; p2 ]
              with
              | Ok r -> (
                  Alcotest.(check bool)
                    "has etag" true
                    (String.length r.Object.Put_result.etag > 0);
                  match Object.head c ~bucket ~key () with
                  | Ok (Some info) ->
                      Alcotest.(check int)
                        "size"
                        (String.length part1_data + String.length part2_data)
                        info.Object.Info.size
                  | Ok None -> Alcotest.fail "object should exist"
                  | Error e -> Alcotest.failf "head: %a" Error.pp e)
              | Error e -> Alcotest.failf "complete: %a" Error.pp e)
          | Error e -> Alcotest.failf "upload p2: %a" Error.pp e)
      | Error e -> Alcotest.failf "upload p1: %a" Error.pp e)

let test_bucket_ops env () =
  let c = make_client env in
  let bname = Fmt.str "test-int-%d" (Unix.getpid ()) in
  (* Create *)
  (match Bucket.create c ~bucket:bname () with
  | Ok () -> ()
  | Error e -> Alcotest.failf "create bucket: %a" Error.pp e);
  (* Head *)
  (match Bucket.head c ~bucket:bname with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "bucket should exist"
  | Error e -> Alcotest.failf "head bucket: %a" Error.pp e);
  (* Delete *)
  (match Bucket.delete c ~bucket:bname with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete bucket: %a" Error.pp e);
  match Bucket.head c ~bucket:bname with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "bucket should be gone"
  | Error _ -> (* 404 variants *) ()

(* ── Metadata & content_type ─────────────────────────────────────── *)

let test_put_get_metadata env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "meta" in
  ignore_put
    (Object.put c ~bucket ~key ~content_type:"application/json"
       ~metadata:[ ("author", "test") ]
       "{}");
  match Object.get c ~bucket ~key () with
  | Ok r ->
      Alcotest.(check (option string))
        "content_type" (Some "application/json") r.content_type;
      (* MinIO returns metadata in x-amz-meta-* headers *)
      Alcotest.(check bool)
        "has metadata" true
        (List.exists (fun (k, _) -> k = "author") r.Object.Get_result.metadata)
  | Error e -> Alcotest.failf "%a" Error.pp e

let test_head_info env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "hi" in
  ignore_put (Object.put c ~bucket ~key ~content_type:"text/plain" "hello");
  match Object.head c ~bucket ~key () with
  | Ok (Some info) ->
      Alcotest.(check int) "size" 5 info.Object.Info.size;
      Alcotest.(check (option string))
        "ct" (Some "text/plain") info.content_type
  | Ok None -> Alcotest.fail "expected Some"
  | Error e -> Alcotest.failf "%a" Error.pp e

(* ── Delete batch ───────────────────────────────────────────────── *)

let test_delete_batch env () =
  let c = make_client env in
  let prefix = fresh_prefix () in
  let k1 = prefix ^ "a" and k2 = prefix ^ "b" and k3 = prefix ^ "c" in
  ignore_put (Object.put c ~bucket ~key:k1 "1");
  ignore_put (Object.put c ~bucket ~key:k2 "2");
  ignore_put (Object.put c ~bucket ~key:k3 "3");
  match
    Object.delete_batch c ~bucket
      ~objects:(List.map Object.Delete_object.v [ k1; k2 ])
  with
  | Ok r ->
      Alcotest.(check int)
        "2 deleted" 2
        (List.length r.Object.Delete_result.deleted);
      check_error "k1 gone" `Not_found (Object.get c ~bucket ~key:k1 ());
      check_get_body "k3 remains" "3" (Object.get c ~bucket ~key:k3 ())
  | Error e -> Alcotest.failf "batch: %a" Error.pp e

(* ── Object tagging ─────────────────────────────────────────────── *)

let test_object_tagging env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "tagged" in
  ignore_put (Object.put c ~bucket ~key "v");
  (match
     Object.Tagging.put c ~bucket ~key [ { Tag.key = "env"; value = "prod" } ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put tags: %a" Error.pp e);
  (match Object.Tagging.get c ~bucket ~key with
  | Ok tags ->
      Alcotest.(check int) "1 tag" 1 (List.length tags);
      Alcotest.(check string) "key" "env" (List.hd tags).Tag.key;
      Alcotest.(check string) "value" "prod" (List.hd tags).value
  | Error e -> Alcotest.failf "get tags: %a" Error.pp e);
  match Object.Tagging.delete c ~bucket ~key with
  | Ok () -> (
      match Object.Tagging.get c ~bucket ~key with
      | Ok tags -> Alcotest.(check int) "0 tags" 0 (List.length tags)
      | Error e -> Alcotest.failf "get after delete: %a" Error.pp e)
  | Error e -> Alcotest.failf "delete tags: %a" Error.pp e

(* ── Multipart abort ────────────────────────────────────────────── *)

let test_multipart_abort env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "aborted" in
  match Multipart.create c ~bucket ~key () with
  | Ok upload ->
      (match Multipart.abort c ~bucket ~key ~upload_id:upload.upload_id with
      | Ok () -> ()
      | Error e -> Alcotest.failf "abort: %a" Error.pp e);
      (* Object should not exist *)
      check_error "not created" `Not_found (Object.get c ~bucket ~key ())
  | Error e -> Alcotest.failf "create: %a" Error.pp e

(* ── Bucket list & location ─────────────────────────────────────── *)

let test_bucket_list env () =
  let c = make_client env in
  match Bucket.list c with
  | Ok buckets ->
      let names = List.map (fun (b : Bucket.Info.t) -> b.name) buckets in
      Alcotest.(check bool)
        "test-bucket present" true
        (List.mem "test-bucket" names)
  | Error e -> Alcotest.failf "list: %a" Error.pp e

let test_bucket_versioning env () =
  let c = make_client env in
  let bname = Fmt.str "test-ver-%d" (Unix.getpid ()) in
  ignore (Bucket.create c ~bucket:bname ());
  (match
     Bucket.Versioning.put c ~bucket:bname Bucket.Versioning.Status.Enabled
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put versioning: %a" Error.pp e);
  (match Bucket.Versioning.get c ~bucket:bname with
  | Ok (Some Bucket.Versioning.Status.Enabled) -> ()
  | Ok other ->
      Alcotest.failf "expected Enabled, got %s"
        (match other with
        | None -> "None"
        | Some Bucket.Versioning.Status.Suspended -> "Suspended"
        | _ -> "?")
  | Error e -> Alcotest.failf "get versioning: %a" Error.pp e);
  (* cleanup *)
  ignore
    (Bucket.Versioning.put c ~bucket:bname Bucket.Versioning.Status.Suspended);
  ignore (Bucket.delete c ~bucket:bname)

let test_bucket_tagging env () =
  let c = make_client env in
  let tags =
    [
      { Tag.key = "project"; value = "weldr" }; { key = "env"; value = "test" };
    ]
  in
  (match Bucket.Tagging.put c ~bucket tags with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put: %a" Error.pp e);
  (match Bucket.Tagging.get c ~bucket with
  | Ok got -> Alcotest.(check int) "2 tags" 2 (List.length got)
  | Error e -> Alcotest.failf "get: %a" Error.pp e);
  match Bucket.Tagging.delete c ~bucket with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete: %a" Error.pp e

let test_presigned_get env () =
  let c = make_client env in
  let key = fresh_prefix () ^ "presigned" in
  ignore_put (Object.put c ~bucket ~key "secret");
  match Presigned.get_object c ~bucket ~key ~expires_in:60 () with
  | Error e -> Alcotest.failf "presigned: %a" Error.pp e
  | Ok url ->
      (* Verify it's a well-formed URL *)
      Alcotest.(check bool)
        "has signature" true
        (let len = String.length "X-Amz-Signature=" in
         let rec check i =
           if i + len > String.length url then false
           else if String.sub url i len = "X-Amz-Signature=" then true
           else check (i + 1)
         in
         check 0)

let test_presigned_invalid_expiry env () =
  let c = make_client env in
  match Presigned.get_object c ~bucket ~key:"ignored" ~expires_in:0 () with
  | Error (`Invalid_request _) -> ()
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e
  | Ok _ -> Alcotest.fail "expected invalid expiry"

let test_invalid_range env () =
  let c = make_client env in
  match Object.get c ~bucket ~key:"ignored" ~range:(Range.Bytes (5, 1)) () with
  | Error (`Invalid_request _) -> ()
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e
  | Ok _ -> Alcotest.fail "expected invalid range"

(* ── Suite ──────────────────────────────────────────────────────── *)

let suite env =
  [
    ( "integration:minio:object",
      [
        Alcotest.test_case "put/get" `Quick (test_put_get env);
        Alcotest.test_case "overwrite" `Quick (test_put_overwrite env);
        Alcotest.test_case "get missing" `Quick (test_get_missing env);
        Alcotest.test_case "head" `Quick (test_head env);
        Alcotest.test_case "head with info" `Quick (test_head_info env);
        Alcotest.test_case "delete" `Quick (test_delete env);
        Alcotest.test_case "conditional delete" `Quick
          (test_conditional_delete env);
        Alcotest.test_case "delete batch" `Quick (test_delete_batch env);
        Alcotest.test_case "etag" `Quick (test_etag env);
        Alcotest.test_case "copy" `Quick (test_copy env);
        Alcotest.test_case "metadata" `Quick (test_put_get_metadata env);
      ] );
    ( "integration:minio:cas",
      [ Alcotest.test_case "put-if-not-exists" `Quick (test_cas env) ] );
    ( "integration:minio:list",
      [
        Alcotest.test_case "prefix" `Quick (test_list env);
        Alcotest.test_case "pagination" `Quick (test_list_pagination env);
      ] );
    ( "integration:minio:tagging",
      [ Alcotest.test_case "put/get/delete" `Quick (test_object_tagging env) ]
    );
    ( "integration:minio:multipart",
      [
        Alcotest.test_case "upload flow" `Slow (test_multipart env);
        Alcotest.test_case "abort" `Quick (test_multipart_abort env);
      ] );
    ( "integration:minio:bucket",
      [
        Alcotest.test_case "create/head/delete" `Quick (test_bucket_ops env);
        Alcotest.test_case "list" `Quick (test_bucket_list env);
        Alcotest.test_case "versioning" `Quick (test_bucket_versioning env);
        Alcotest.test_case "tagging" `Quick (test_bucket_tagging env);
      ] );
    ( "integration:minio:presigned",
      [
        Alcotest.test_case "get url" `Quick (test_presigned_get env);
        Alcotest.test_case "invalid expiry" `Quick
          (test_presigned_invalid_expiry env);
        Alcotest.test_case "invalid range" `Quick (test_invalid_range env);
      ] );
  ]
