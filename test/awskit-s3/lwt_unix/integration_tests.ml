(** Integration tests against MinIO. Requires: docker compose up -d *)

open Base
open Awskit_s3_lwt_unix

let ( let* ) = Lwt.bind
let test_prefix = ref 0

let fresh_prefix () =
  Int.incr test_prefix;
  Fmt.str "test/%d/%d/" (Unix.getpid ()) !test_prefix

let bucket = "test-bucket"

let make_client () =
  Awskit_lwt_unix.create ~scheme:`Http ~region:"us-east-1"
    ~credentials:
      (Awskit.Credentials.make ~access_key_id:"minioadmin"
         ~secret_access_key:"minioadmin" ())
    ~endpoint:"localhost" ~port:9000 ()

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

let list_all client ~prefix =
  let rec go acc token =
    let* result =
      Object.list client ~bucket ~prefix ?continuation_token:token ()
    in
    match result with
    | Error e -> Lwt.return (Error e)
    | Ok r ->
        let acc = acc @ r.Object.List_result.keys in
        if r.is_truncated then go acc r.next_continuation_token
        else Lwt.return (Ok acc)
  in
  go [] None

(* ── Tests ──────────────────────────────────────────────────────── *)

let test_put_get () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "hello" in
  let* _ = Object.put c ~bucket ~key "world" in
  let* result = Object.get c ~bucket ~key () in
  check_get_body "get" "world" result;
  Lwt.return_unit

let test_put_overwrite () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "k" in
  let* _ = Object.put c ~bucket ~key "v1" in
  let* _ = Object.put c ~bucket ~key "v2" in
  let* result = Object.get c ~bucket ~key () in
  check_get_body "latest" "v2" result;
  Lwt.return_unit

let test_get_missing () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "nope" in
  let* result = Object.get c ~bucket ~key () in
  check_error "not found" `Not_found result;
  Lwt.return_unit

let test_head () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "k" in
  let* head_before = Object.head c ~bucket ~key () in
  (match head_before with
  | Ok None -> ()
  | _ -> Alcotest.fail "expected None before put");
  let* _ = Object.put c ~bucket ~key "12345" in
  let* head_after = Object.head c ~bucket ~key () in
  (match head_after with
  | Ok (Some info) ->
      Alcotest.(check int) "size" 5 info.Object.Info.size;
      Alcotest.(check bool) "has etag" true (String.length info.etag > 0)
  | Ok None -> Alcotest.fail "expected Some"
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

let test_delete () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "k" in
  let* _ = Object.put c ~bucket ~key "v" in
  let* del_result = Object.delete c ~bucket ~key in
  (match del_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete: %a" Error.pp e);
  let* get_result = Object.get c ~bucket ~key () in
  check_error "gone" `Not_found get_result;
  let* del_missing = Object.delete c ~bucket ~key in
  (match del_missing with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete missing: %a" Error.pp e);
  Lwt.return_unit

let test_cas () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "version" in
  let* first = Object.put c ~bucket ~key ~if_none_match:true "v1" in
  Alcotest.(check bool) "first ok" true (Result.is_ok first);
  let* second = Object.put c ~bucket ~key ~if_none_match:true "v2" in
  (match second with
  | Error `Precondition_failed -> ()
  | Ok _ -> Alcotest.fail "expected 412"
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e);
  let* result = Object.get c ~bucket ~key () in
  check_get_body "unchanged" "v1" result;
  Lwt.return_unit

let test_list () =
  Lwt_main.run
  @@
  let c = make_client () in
  let prefix = fresh_prefix () in
  let* _ = Object.put c ~bucket ~key:(prefix ^ "a") "1" in
  let* _ = Object.put c ~bucket ~key:(prefix ^ "b") "2" in
  let* _ = Object.put c ~bucket ~key:(prefix ^ "c") "3" in
  let* result = list_all c ~prefix in
  (match result with
  | Ok keys ->
      Alcotest.(check (list string))
        "3 keys sorted"
        [ prefix ^ "a"; prefix ^ "b"; prefix ^ "c" ]
        keys
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

let test_list_pagination () =
  Lwt_main.run
  @@
  let c = make_client () in
  let prefix = fresh_prefix () in
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let* _ = Object.put c ~bucket ~key:(Fmt.str "%s%03d" prefix i) "v" in
        Lwt.return_unit)
      [ 1; 2; 3; 4; 5 ]
  in
  let* page = Object.list c ~bucket ~prefix ~max_keys:2 () in
  (match page with
  | Ok r ->
      Alcotest.(check int) "page size" 2 (List.length r.keys);
      Alcotest.(check bool) "truncated" true r.is_truncated
  | Error e -> Alcotest.failf "%a" Error.pp e);
  let* all = list_all c ~prefix in
  (match all with
  | Ok keys -> Alcotest.(check int) "all 5" 5 (List.length keys)
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

let test_etag () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "k" in
  let* result = Object.put c ~bucket ~key "data" in
  (match result with
  | Ok r ->
      Alcotest.(check bool)
        "has etag" true
        (String.length r.Object.Put_result.etag > 0)
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

let test_copy () =
  Lwt_main.run
  @@
  let c = make_client () in
  let prefix = fresh_prefix () in
  let src = prefix ^ "src" in
  let dst = prefix ^ "dst" in
  let* _ = Object.put c ~bucket ~key:src "hello copy" in
  let* copy_result =
    Object.copy c ~src_bucket:bucket ~src_key:src ~dst_bucket:bucket
      ~dst_key:dst ()
  in
  (match copy_result with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "copy: %a" Error.pp e);
  let* get_result = Object.get c ~bucket ~key:dst () in
  check_get_body "copy contents" "hello copy" get_result;
  Lwt.return_unit

let test_multipart () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "big" in
  let* create_result = Multipart.create c ~bucket ~key () in
  match create_result with
  | Error e -> Alcotest.failf "create: %a" Error.pp e
  | Ok upload ->
      (* MinIO minimum part size is 5MB for non-final parts *)
      let part1_data = String.make (5 * 1024 * 1024) 'A' in
      let part2_data = "final" in
      let* p1_result =
        Multipart.upload_part c ~bucket ~key ~upload_id:upload.upload_id
          ~part_number:1 part1_data
      in
      let p1 =
        match p1_result with
        | Ok p -> p
        | Error e -> Alcotest.failf "upload p1: %a" Error.pp e
      in
      let* p2_result =
        Multipart.upload_part c ~bucket ~key ~upload_id:upload.upload_id
          ~part_number:2 part2_data
      in
      let p2 =
        match p2_result with
        | Ok p -> p
        | Error e -> Alcotest.failf "upload p2: %a" Error.pp e
      in
      let* complete =
        Multipart.complete c ~bucket ~key ~upload_id:upload.upload_id [ p1; p2 ]
      in
      (match complete with
      | Ok r ->
          Alcotest.(check bool)
            "has etag" true
            (String.length r.Object.Put_result.etag > 0)
      | Error e -> Alcotest.failf "complete: %a" Error.pp e);
      let* head = Object.head c ~bucket ~key () in
      (match head with
      | Ok (Some info) ->
          Alcotest.(check int)
            "size"
            (String.length part1_data + String.length part2_data)
            info.Object.Info.size
      | Ok None -> Alcotest.fail "object should exist"
      | Error e -> Alcotest.failf "head: %a" Error.pp e);
      Lwt.return_unit

let test_bucket_ops () =
  Lwt_main.run
  @@
  let c = make_client () in
  let bname = Fmt.str "test-lwt-%d" (Unix.getpid ()) in
  let* create_result = Bucket.create c ~bucket:bname () in
  (match create_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "create bucket: %a" Error.pp e);
  let* head_result = Bucket.head c ~bucket:bname in
  (match head_result with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "bucket should exist"
  | Error e -> Alcotest.failf "head bucket: %a" Error.pp e);
  let* del_result = Bucket.delete c ~bucket:bname in
  (match del_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete bucket: %a" Error.pp e);
  let* head_gone = Bucket.head c ~bucket:bname in
  (match head_gone with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "bucket should be gone"
  | Error _ -> (* 404 variants *) ());
  Lwt.return_unit

(* ── Metadata & content_type ─────────────────────────────────────── *)

let test_put_get_metadata () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "meta" in
  let* _ =
    Object.put c ~bucket ~key ~content_type:"application/json"
      ~metadata:[ ("author", "test") ]
      "{}"
  in
  let* result = Object.get c ~bucket ~key () in
  (match result with
  | Ok r ->
      Alcotest.(check (option string))
        "content_type" (Some "application/json") r.content_type;
      Alcotest.(check bool)
        "has metadata" true
        (List.exists r.Object.Get_result.metadata ~f:(fun (k, _) ->
             String.equal k "author"))
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

let test_head_info () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "hi" in
  let* _ = Object.put c ~bucket ~key ~content_type:"text/plain" "hello" in
  let* result = Object.head c ~bucket ~key () in
  (match result with
  | Ok (Some info) ->
      Alcotest.(check int) "size" 5 info.Object.Info.size;
      Alcotest.(check (option string))
        "ct" (Some "text/plain") info.content_type
  | Ok None -> Alcotest.fail "expected Some"
  | Error e -> Alcotest.failf "%a" Error.pp e);
  Lwt.return_unit

(* ── Delete batch ───────────────────────────────────────────────── *)

let test_delete_batch () =
  Lwt_main.run
  @@
  let c = make_client () in
  let prefix = fresh_prefix () in
  let k1 = prefix ^ "a" and k2 = prefix ^ "b" and k3 = prefix ^ "c" in
  let* _ = Object.put c ~bucket ~key:k1 "1" in
  let* _ = Object.put c ~bucket ~key:k2 "2" in
  let* _ = Object.put c ~bucket ~key:k3 "3" in
  let* result = Object.delete_batch c ~bucket ~keys:[ k1; k2 ] in
  (match result with
  | Ok r ->
      Alcotest.(check int)
        "2 deleted" 2
        (List.length r.Object.Delete_result.deleted)
  | Error e -> Alcotest.failf "batch: %a" Error.pp e);
  let* k1_result = Object.get c ~bucket ~key:k1 () in
  check_error "k1 gone" `Not_found k1_result;
  let* k3_result = Object.get c ~bucket ~key:k3 () in
  check_get_body "k3 remains" "3" k3_result;
  Lwt.return_unit

(* ── Object tagging ─────────────────────────────────────────────── *)

let test_object_tagging () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "tagged" in
  let* _ = Object.put c ~bucket ~key "v" in
  let* put_tags =
    Object.Tagging.put c ~bucket ~key [ { Tag.key = "env"; value = "prod" } ]
  in
  (match put_tags with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put tags: %a" Error.pp e);
  let* get_tags = Object.Tagging.get c ~bucket ~key in
  (match get_tags with
  | Ok tags ->
      Alcotest.(check int) "1 tag" 1 (List.length tags);
      let first = List.hd_exn tags in
      Alcotest.(check string) "key" "env" first.Tag.key;
      Alcotest.(check string) "value" "prod" first.value
  | Error e -> Alcotest.failf "get tags: %a" Error.pp e);
  let* del_tags = Object.Tagging.delete c ~bucket ~key in
  (match del_tags with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete tags: %a" Error.pp e);
  let* after_del = Object.Tagging.get c ~bucket ~key in
  (match after_del with
  | Ok tags -> Alcotest.(check int) "0 tags" 0 (List.length tags)
  | Error e -> Alcotest.failf "get after delete: %a" Error.pp e);
  Lwt.return_unit

(* ── Multipart abort ────────────────────────────────────────────── *)

let test_multipart_abort () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "aborted" in
  let* create_result = Multipart.create c ~bucket ~key () in
  let upload =
    match create_result with
    | Ok u -> u
    | Error e -> Alcotest.failf "create: %a" Error.pp e
  in
  let* abort_result =
    Multipart.abort c ~bucket ~key ~upload_id:upload.upload_id
  in
  (match abort_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "abort: %a" Error.pp e);
  let* get_result = Object.get c ~bucket ~key () in
  check_error "not created" `Not_found get_result;
  Lwt.return_unit

(* ── Bucket list & location ─────────────────────────────────────── *)

let test_bucket_list () =
  Lwt_main.run
  @@
  let c = make_client () in
  let* result = Bucket.list c in
  (match result with
  | Ok buckets ->
      let names = List.map buckets ~f:(fun (b : Bucket.Info.t) -> b.name) in
      Alcotest.(check bool)
        "test-bucket present" true
        (List.mem names "test-bucket" ~equal:String.equal)
  | Error e -> Alcotest.failf "list: %a" Error.pp e);
  Lwt.return_unit

let test_bucket_versioning () =
  Lwt_main.run
  @@
  let c = make_client () in
  let bname = Fmt.str "test-lwt-ver-%d" (Unix.getpid ()) in
  let* _ = Bucket.create c ~bucket:bname () in
  let* put_ver =
    Bucket.Versioning.put c ~bucket:bname Bucket.Versioning.Status.Enabled
  in
  (match put_ver with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put versioning: %a" Error.pp e);
  let* get_ver = Bucket.Versioning.get c ~bucket:bname in
  (match get_ver with
  | Ok (Some Bucket.Versioning.Status.Enabled) -> ()
  | Ok other ->
      Alcotest.failf "expected Enabled, got %s"
        (match other with
        | None -> "None"
        | Some Bucket.Versioning.Status.Suspended -> "Suspended"
        | _ -> "?")
  | Error e -> Alcotest.failf "get versioning: %a" Error.pp e);
  (* cleanup *)
  let* _ =
    Bucket.Versioning.put c ~bucket:bname Bucket.Versioning.Status.Suspended
  in
  let* _ = Bucket.delete c ~bucket:bname in
  Lwt.return_unit

let test_bucket_tagging () =
  Lwt_main.run
  @@
  let c = make_client () in
  let tags =
    [
      { Tag.key = "project"; value = "weldr" }; { key = "env"; value = "test" };
    ]
  in
  let* put_result = Bucket.Tagging.put c ~bucket tags in
  (match put_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "put: %a" Error.pp e);
  let* get_result = Bucket.Tagging.get c ~bucket in
  (match get_result with
  | Ok got -> Alcotest.(check int) "2 tags" 2 (List.length got)
  | Error e -> Alcotest.failf "get: %a" Error.pp e);
  let* del_result = Bucket.Tagging.delete c ~bucket in
  (match del_result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "delete: %a" Error.pp e);
  Lwt.return_unit

let test_presigned_get () =
  Lwt_main.run
  @@
  let c = make_client () in
  let key = fresh_prefix () ^ "presigned" in
  let* _ = Object.put c ~bucket ~key "secret" in
  let result = Presigned.get_object c ~bucket ~key ~expires_in:60 () in
  (match result with
  | Error e -> Alcotest.failf "presigned: %a" Error.pp e
  | Ok url ->
      let sig_marker = "X-Amz-Signature=" in
      Alcotest.(check bool)
        "has signature" true
        (String.is_substring url ~substring:sig_marker));
  Lwt.return_unit

let test_presigned_invalid_expiry () =
  Lwt_main.run
  @@
  let c = make_client () in
  let result = Presigned.get_object c ~bucket ~key:"ignored" ~expires_in:0 () in
  (match result with
  | Error (`Invalid_request _) -> ()
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e
  | Ok _ -> Alcotest.fail "expected invalid expiry");
  Lwt.return_unit

let test_invalid_range () =
  Lwt_main.run
  @@
  let c = make_client () in
  let* result =
    Object.get c ~bucket ~key:"ignored" ~range:(Range.Bytes (5, 1)) ()
  in
  (match result with
  | Error (`Invalid_request _) -> ()
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e
  | Ok _ -> Alcotest.fail "expected invalid range");
  Lwt.return_unit

(* ── Suite ──────────────────────────────────────────────────────── *)

let suite () =
  [
    ( "integration:minio:object",
      [
        Alcotest.test_case "put/get" `Quick test_put_get;
        Alcotest.test_case "overwrite" `Quick test_put_overwrite;
        Alcotest.test_case "get missing" `Quick test_get_missing;
        Alcotest.test_case "head" `Quick test_head;
        Alcotest.test_case "head with info" `Quick test_head_info;
        Alcotest.test_case "delete" `Quick test_delete;
        Alcotest.test_case "delete batch" `Quick test_delete_batch;
        Alcotest.test_case "etag" `Quick test_etag;
        Alcotest.test_case "copy" `Quick test_copy;
        Alcotest.test_case "metadata" `Quick test_put_get_metadata;
      ] );
    ( "integration:minio:cas",
      [ Alcotest.test_case "put-if-not-exists" `Quick test_cas ] );
    ( "integration:minio:list",
      [
        Alcotest.test_case "prefix" `Quick test_list;
        Alcotest.test_case "pagination" `Quick test_list_pagination;
      ] );
    ( "integration:minio:tagging",
      [ Alcotest.test_case "put/get/delete" `Quick test_object_tagging ] );
    ( "integration:minio:multipart",
      [
        Alcotest.test_case "upload flow" `Slow test_multipart;
        Alcotest.test_case "abort" `Quick test_multipart_abort;
      ] );
    ( "integration:minio:bucket",
      [
        Alcotest.test_case "create/head/delete" `Quick test_bucket_ops;
        Alcotest.test_case "list" `Quick test_bucket_list;
        Alcotest.test_case "versioning" `Quick test_bucket_versioning;
        Alcotest.test_case "tagging" `Quick test_bucket_tagging;
      ] );
    ( "integration:minio:presigned",
      [
        Alcotest.test_case "get url" `Quick test_presigned_get;
        Alcotest.test_case "invalid expiry" `Quick test_presigned_invalid_expiry;
        Alcotest.test_case "invalid range" `Quick test_invalid_range;
      ] );
  ]
