(** Sim-specific tests: etags, clock, pagination, faults, history, policy,
    metadata, copy, delete_batch, object tagging, multipart, bucket ops. *)

open Awskit_s3

let bucket = "test-bucket"

let make () =
  let clock = Sim.Clock.create () in
  let store = Sim.create_store ~clock () in
  let creds =
    Awskit.Credentials.make ~access_key_id:"admin" ~secret_access_key:"secret"
      ()
  in
  let c = Sim.connect store ~credentials:creds in
  ignore (Sim.Bucket.create c ~bucket ());
  (store, clock, c)

let make_fresh () =
  let store, clock, client = make () in
  ignore store;
  ignore clock;
  client

let ok_or_fail msg = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %a" msg Error.pp e

let error_t : Error.t Alcotest.testable = Alcotest.testable Error.pp Error.equal

(* ── ETags ──────────────────────────────────────────────────────── *)

let test_etag_quoted () =
  let c = make_fresh () in
  let r = Sim.Object.put c ~bucket ~key:"k" "data" |> ok_or_fail "put" in
  Alcotest.(check bool)
    "starts with quote" true
    (r.Object.Put_result.etag.[0] = '"');
  Alcotest.(check bool)
    "ends with quote" true
    (r.etag.[String.length r.etag - 1] = '"')

let test_etag_varies () =
  let c = make_fresh () in
  let e1 = (Sim.Object.put c ~bucket ~key:"k1" "aaa" |> ok_or_fail "p1").etag in
  let e2 = (Sim.Object.put c ~bucket ~key:"k2" "bbb" |> ok_or_fail "p2").etag in
  Alcotest.(check bool) "different" true (not (String.equal e1 e2))

let test_etag_stable () =
  let c = make_fresh () in
  let e1 =
    (Sim.Object.put c ~bucket ~key:"k1" "same" |> ok_or_fail "p1").etag
  in
  let e2 =
    (Sim.Object.put c ~bucket ~key:"k2" "same" |> ok_or_fail "p2").etag
  in
  Alcotest.(check string) "same content same etag" e1 e2

(* ── Clock ──────────────────────────────────────────────────────── *)

let test_clock_last_modified () =
  let _store, clock, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"k" "v");
  Sim.Clock.advance_ms clock 5000;
  let info =
    Sim.Object.head c ~bucket ~key:"k" () |> ok_or_fail "head" |> Option.get
  in
  Alcotest.(check bool)
    "has timestamp" true
    (String.length info.Object.Info.last_modified > 0)

(* ── Pagination ─────────────────────────────────────────────────── *)

let test_pagination () =
  let clock = Sim.Clock.create () in
  let store = Sim.create_store ~config:{ max_list_keys = 2 } ~clock () in
  let creds =
    Awskit.Credentials.make ~access_key_id:"admin" ~secret_access_key:"s" ()
  in
  let c = Sim.connect store ~credentials:creds in
  ignore (Sim.Bucket.create c ~bucket ());
  for i = 1 to 5 do
    ignore (Sim.Object.put c ~bucket ~key:(Fmt.str "k%03d" i) "v")
  done;
  let r = Sim.Object.list c ~bucket ~prefix:"k" () |> ok_or_fail "page1" in
  Alcotest.(check int) "page 1" 2 (List.length r.keys);
  Alcotest.(check bool) "truncated" true r.is_truncated;
  let r2 =
    Sim.Object.list c ~bucket ~prefix:"k"
      ?continuation_token:r.next_continuation_token ()
    |> ok_or_fail "page2"
  in
  Alcotest.(check int) "page 2" 2 (List.length r2.keys);
  Alcotest.(check bool) "still truncated" true r2.is_truncated

(* ── Metadata & content_type ────────────────────────────────────── *)

let test_put_get_metadata () =
  let c = make_fresh () in
  let meta = [ ("author", "test"); ("version", "1") ] in
  ignore
    (Sim.Object.put c ~bucket ~key:"k" ~metadata:meta ~content_type:"text/plain"
       "hi");
  let r = Sim.Object.get c ~bucket ~key:"k" () |> ok_or_fail "get" in
  Alcotest.(check (list (pair string string)))
    "metadata" meta r.Object.Get_result.metadata;
  Alcotest.(check (option string))
    "content_type" (Some "text/plain") r.content_type

let test_head_metadata () =
  let c = make_fresh () in
  let meta = [ ("foo", "bar") ] in
  ignore
    (Sim.Object.put c ~bucket ~key:"k" ~metadata:meta
       ~storage_class:Storage_class.Standard_ia "x");
  let info =
    Sim.Object.head c ~bucket ~key:"k" () |> ok_or_fail "head" |> Option.get
  in
  Alcotest.(check (list (pair string string)))
    "metadata" meta info.Object.Info.metadata;
  Alcotest.(check (option string))
    "storage_class" (Some "STANDARD_IA")
    (Option.map Storage_class.to_string info.storage_class)

(* ── Copy ───────────────────────────────────────────────────────── *)

let test_sim_copy () =
  let c = make_fresh () in
  ignore
    (Sim.Object.put c ~bucket ~key:"src" ~metadata:[ ("m", "1") ] "payload");
  let r =
    Sim.Object.copy c ~src_bucket:bucket ~src_key:"src" ~dst_bucket:bucket
      ~dst_key:"dst" ()
    |> ok_or_fail "copy"
  in
  Alcotest.(check bool)
    "has etag" true
    (String.length r.Object.Copy_result.etag > 0);
  let got = Sim.Object.get c ~bucket ~key:"dst" () |> ok_or_fail "get dst" in
  Alcotest.(check string) "body copied" "payload" got.body;
  Alcotest.(check (list (pair string string)))
    "metadata copied"
    [ ("m", "1") ]
    got.metadata

let test_sim_copy_missing () =
  let c = make_fresh () in
  match
    Sim.Object.copy c ~src_bucket:bucket ~src_key:"nope" ~dst_bucket:bucket
      ~dst_key:"d" ()
  with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "expected Not_found"

let test_sim_copy_cross_bucket () =
  let c = make_fresh () in
  ignore (Sim.Bucket.create c ~bucket:"other" ());
  ignore (Sim.Object.put c ~bucket ~key:"src" "cross");
  ignore
    (Sim.Object.copy c ~src_bucket:bucket ~src_key:"src" ~dst_bucket:"other"
       ~dst_key:"dst" ()
    |> ok_or_fail "cross copy");
  let got =
    Sim.Object.get c ~bucket:"other" ~key:"dst" () |> ok_or_fail "get"
  in
  Alcotest.(check string) "body" "cross" got.body

(* ── Delete batch ───────────────────────────────────────────────── *)

let test_sim_delete_batch () =
  let c = make_fresh () in
  ignore (Sim.Object.put c ~bucket ~key:"a" "1");
  ignore (Sim.Object.put c ~bucket ~key:"b" "2");
  ignore (Sim.Object.put c ~bucket ~key:"c" "3");
  let r =
    Sim.Object.delete_batch c ~bucket
      ~objects:(List.map Sim.Object.Delete_object.v [ "a"; "b"; "missing" ])
    |> ok_or_fail "batch"
  in
  Alcotest.(check int)
    "3 deleted" 3
    (List.length r.Object.Delete_result.deleted);
  Alcotest.(check int) "0 errors" 0 (List.length r.errors);
  (* a,b gone; c remains *)
  (match Sim.Object.get c ~bucket ~key:"a" () with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "a");
  (match Sim.Object.get c ~bucket ~key:"b" () with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "b");
  let got = Sim.Object.get c ~bucket ~key:"c" () |> ok_or_fail "c" in
  Alcotest.(check string) "c intact" "3" got.body

let test_sim_conditional_delete () =
  let c = make_fresh () in
  let put = Sim.Object.put c ~bucket ~key:"k" "v" |> ok_or_fail "put" in
  (match
     Sim.Object.delete c ~bucket ~key:"k"
       ~preconditions:(Object.Preconditions.Delete.if_etag "\"wrong\"")
       ()
   with
  | Error `Precondition_failed -> ()
  | Ok () -> Alcotest.fail "expected conditional delete to fail"
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e);
  ignore (Sim.Object.get c ~bucket ~key:"k" () |> ok_or_fail "still present");
  (match
     Sim.Object.delete c ~bucket ~key:"k"
       ~preconditions:(Object.Preconditions.Delete.if_etag put.etag)
       ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "conditional delete: %a" Error.pp e);
  match Sim.Object.get c ~bucket ~key:"k" () with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "k should be gone"

let test_sim_conditional_delete_batch () =
  let c = make_fresh () in
  let a = Sim.Object.put c ~bucket ~key:"a" "1" |> ok_or_fail "put a" in
  ignore (Sim.Object.put c ~bucket ~key:"b" "2" |> ok_or_fail "put b");
  let result =
    Sim.Object.delete_batch c ~bucket
      ~objects:
        [
          Sim.Object.Delete_object.v ~etag:a.etag "a";
          Sim.Object.Delete_object.v ~etag:"\"wrong\"" "b";
        ]
    |> ok_or_fail "delete batch"
  in
  Alcotest.(check int)
    "one deleted" 1
    (List.length result.Object.Delete_result.deleted);
  Alcotest.(check int)
    "one error" 1
    (List.length result.Object.Delete_result.errors);
  (match Sim.Object.get c ~bucket ~key:"a" () with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "a should be gone");
  ignore (Sim.Object.get c ~bucket ~key:"b" () |> ok_or_fail "b remains")

(* ── Object tagging ─────────────────────────────────────────────── *)

let test_sim_object_tags () =
  let c = make_fresh () in
  ignore
    (Sim.Object.put c ~bucket ~key:"k"
       ~tags:[ { Tag.key = "env"; value = "prod" } ]
       "v");
  let tags =
    Sim.Object.Tagging.get c ~bucket ~key:"k" |> ok_or_fail "get tags"
  in
  Alcotest.(check int) "1 tag" 1 (List.length tags);
  Alcotest.(check string) "tag key" "env" (List.hd tags).Tag.key;
  (* Replace tags *)
  ignore
    (Sim.Object.Tagging.put c ~bucket ~key:"k"
       [ { Tag.key = "a"; value = "1" }; { key = "b"; value = "2" } ]
    |> ok_or_fail "put tags");
  let tags2 =
    Sim.Object.Tagging.get c ~bucket ~key:"k" |> ok_or_fail "get tags 2"
  in
  Alcotest.(check int) "2 tags" 2 (List.length tags2);
  (* Delete tags *)
  ignore (Sim.Object.Tagging.delete c ~bucket ~key:"k" |> ok_or_fail "del tags");
  let tags3 =
    Sim.Object.Tagging.get c ~bucket ~key:"k" |> ok_or_fail "get tags 3"
  in
  Alcotest.(check int) "0 tags" 0 (List.length tags3)

let test_sim_object_tags_missing () =
  let c = make_fresh () in
  match Sim.Object.Tagging.get c ~bucket ~key:"nope" with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "expected Not_found"

(* ── Bucket operations ──────────────────────────────────────────── *)

let test_sim_bucket_create_delete () =
  let c = make_fresh () in
  ignore (Sim.Bucket.create c ~bucket:"new-b" () |> ok_or_fail "create");
  let exists = Sim.Bucket.head c ~bucket:"new-b" |> ok_or_fail "head" in
  Alcotest.(check bool) "exists" true exists;
  ignore (Sim.Bucket.delete c ~bucket:"new-b" |> ok_or_fail "delete");
  let gone = Sim.Bucket.head c ~bucket:"new-b" |> ok_or_fail "head2" in
  Alcotest.(check bool) "gone" false gone

let test_sim_bucket_already_exists () =
  let c = make_fresh () in
  match Sim.Bucket.create c ~bucket () with
  | Error `Bucket_already_exists -> ()
  | _ -> Alcotest.fail "expected Bucket_already_exists"

let test_sim_bucket_not_empty () =
  let c = make_fresh () in
  ignore (Sim.Object.put c ~bucket ~key:"k" "v");
  match Sim.Bucket.delete c ~bucket with
  | Error `Bucket_not_empty -> ()
  | _ -> Alcotest.fail "expected Bucket_not_empty"

let test_sim_bucket_list () =
  let store, _, c = make () in
  ignore store;
  ignore (Sim.Bucket.create c ~bucket:"alpha" ());
  ignore (Sim.Bucket.create c ~bucket:"zeta" ());
  let buckets = Sim.Bucket.list c |> ok_or_fail "list" in
  let names = List.map (fun (b : Bucket.Info.t) -> b.name) buckets in
  Alcotest.(check bool) "contains alpha" true (List.mem "alpha" names);
  Alcotest.(check bool) "contains zeta" true (List.mem "zeta" names);
  Alcotest.(check bool) "sorted" true (names = List.sort String.compare names)

let test_sim_no_such_bucket () =
  let c = make_fresh () in
  match Sim.Object.get c ~bucket:"nonexistent" ~key:"k" () with
  | Error `No_such_bucket -> ()
  | _ -> Alcotest.fail "expected No_such_bucket"

(* ── Bucket policy CRUD ─────────────────────────────────────────── *)

let test_sim_bucket_policy_crud () =
  let c = make_fresh () in
  (* No policy initially *)
  (match Sim.Bucket.Policy.get c ~bucket with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "expected no policy");
  let policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.Any;
            actions = [ "s3:GetObject" ];
            resources = [ Policy.Resource.Bucket_objects bucket ];
          };
        ];
    }
  in
  ignore (Sim.Bucket.Policy.put c ~bucket policy |> ok_or_fail "put policy");
  let got = Sim.Bucket.Policy.get c ~bucket |> ok_or_fail "get policy" in
  Alcotest.(check int) "1 statement" 1 (List.length got.statements);
  ignore (Sim.Bucket.Policy.delete c ~bucket |> ok_or_fail "delete policy");
  match Sim.Bucket.Policy.get c ~bucket with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "expected deleted"

(* ── Policy JSON roundtrip ──────────────────────────────────────── *)

let test_policy_json_roundtrip () =
  let policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.User [ "user-1"; "user-2" ];
            actions = [ "s3:GetObject"; "s3:PutObject"; "s3:ListBucket" ];
            resources =
              [
                Policy.Resource.Bucket "my-bucket";
                Policy.Resource.Bucket_objects "my-bucket";
              ];
          };
          {
            Policy.Statement.effect_ = Policy.Effect.Deny;
            principal = Policy.Principal.Any;
            actions = [ "s3:DeleteObject" ];
            resources = [ Policy.Resource.Bucket_objects "my-bucket" ];
          };
        ];
    }
  in
  let json = Policy.to_json policy in
  let parsed = Policy.of_json json |> ok_or_fail "parse" in
  Alcotest.(check int) "2 statements" 2 (List.length parsed.statements);
  let s1 = List.nth parsed.statements 0 in
  Alcotest.(check int) "3 actions" 3 (List.length s1.actions);
  Alcotest.(check int) "2 resources" 2 (List.length s1.resources);
  let s2 = List.nth parsed.statements 1 in
  Alcotest.(check bool) "deny" true (s2.effect_ = Policy.Effect.Deny)

let expect_invalid_policy json =
  match Policy.of_json json with
  | Error (`Invalid_json _) -> ()
  | Error e -> Alcotest.failf "expected Invalid_json, got %a" Error.pp e
  | Ok _ -> Alcotest.fail "expected invalid policy"

let test_policy_invalid_effect () =
  expect_invalid_policy
    {|{
      "Version":"2012-10-17",
      "Statement":[{
        "Effect":"Nope",
        "Principal":"*",
        "Action":"s3:GetObject",
        "Resource":"arn:aws:s3:::my-bucket/*"
      }]
    }|}

let test_policy_invalid_principal () =
  expect_invalid_policy
    {|{
      "Version":"2012-10-17",
      "Statement":[{
        "Effect":"Allow",
        "Principal":{"Service":"ec2.amazonaws.com"},
        "Action":"s3:GetObject",
        "Resource":"arn:aws:s3:::my-bucket/*"
      }]
    }|}

let test_policy_invalid_resource () =
  expect_invalid_policy
    {|{
      "Version":"2012-10-17",
      "Statement":[{
        "Effect":"Allow",
        "Principal":"*",
        "Action":"s3:GetObject",
        "Resource":"not-an-arn"
      }]
    }|}

let test_policy_deny_overrides_allow () =
  let c = make_fresh () in
  let policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.User [ "user-x" ];
            actions = [ "s3:*" ];
            resources =
              [
                Policy.Resource.Bucket bucket;
                Policy.Resource.Bucket_objects bucket;
              ];
          };
          {
            Policy.Statement.effect_ = Policy.Effect.Deny;
            principal = Policy.Principal.User [ "user-x" ];
            actions = [ "s3:DeleteObject" ];
            resources = [ Policy.Resource.Bucket_objects bucket ];
          };
        ];
    }
  in
  ignore (Sim.Bucket.Policy.put c ~bucket policy);
  let user_creds =
    Awskit.Credentials.make ~access_key_id:"user-x" ~secret_access_key:"s" ()
  in
  let store = Sim.store c in
  let user = Sim.connect store ~credentials:user_creds in
  (* Put allowed *)
  ignore (Sim.Object.put user ~bucket ~key:"k" "v" |> ok_or_fail "put");
  (* Get allowed *)
  ignore (Sim.Object.get user ~bucket ~key:"k" () |> ok_or_fail "get");
  (* Delete denied *)
  match Sim.Object.delete user ~bucket ~key:"k" () with
  | Error `Access_denied -> ()
  | _ -> Alcotest.fail "delete should be denied"

let test_policy_resource_scope () =
  let bucket_only_policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.User [ "user-x" ];
            actions = [ "s3:GetObject" ];
            resources = [ Policy.Resource.Bucket bucket ];
          };
        ];
    }
  in
  let object_only_policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.User [ "user-x" ];
            actions = [ "s3:ListBucket" ];
            resources = [ Policy.Resource.Bucket_objects bucket ];
          };
        ];
    }
  in
  let result_pp fmt = function
    | `Allowed -> Fmt.string fmt "Allowed"
    | `Denied -> Fmt.string fmt "Denied"
    | `No_match -> Fmt.string fmt "No_match"
  in
  let result_t = Alcotest.of_pp result_pp in
  Alcotest.(check result_t)
    "bucket resource does not match object target" `No_match
    (Policy.evaluate bucket_only_policy ~access_key_id:"user-x"
       ~action:"s3:GetObject"
       ~target:(Policy.Object_target (bucket, "k")));
  Alcotest.(check result_t)
    "object resource does not match bucket target" `No_match
    (Policy.evaluate object_only_policy ~access_key_id:"user-x"
       ~action:"s3:ListBucket" ~target:(Policy.Bucket_target bucket))

(* ── Faults ─────────────────────────────────────────────────────── *)

let test_fault_slowdown () =
  let c = make_fresh () in
  Sim.inject_fault c Slow_down;
  Alcotest.check error_t "503" (`Service_unavailable "SlowDown")
    (match Sim.Object.put c ~bucket ~key:"k" "v" with
    | Error e -> e
    | _ -> `Not_found)

let test_fault_internal () =
  let c = make_fresh () in
  Sim.inject_fault c Internal_error;
  Alcotest.check error_t "500"
    (`Request_error (500, "InternalError"))
    (match Sim.Object.put c ~bucket ~key:"k" "v" with
    | Error e -> e
    | _ -> `Not_found)

let test_fault_reset () =
  let c = make_fresh () in
  Sim.inject_fault c Connection_reset;
  Alcotest.check error_t "reset"
    (`Request_error (0, "connection reset"))
    (match Sim.Object.put c ~bucket ~key:"k" "v" with
    | Error e -> e
    | _ -> `Not_found)

let test_fault_response_lost_put () =
  let c = make_fresh () in
  Sim.inject_fault c Response_lost;
  (match Sim.Object.put c ~bucket ~key:"k" "v" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  let r = Sim.Object.get c ~bucket ~key:"k" () |> ok_or_fail "get" in
  Alcotest.(check string) "written" "v" r.body

let test_fault_response_lost_get () =
  let c = make_fresh () in
  ignore (Sim.Object.put c ~bucket ~key:"k" "v");
  Sim.inject_fault c Response_lost;
  match Sim.Object.get c ~bucket ~key:"k" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"

let test_fault_response_lost_delete () =
  let c = make_fresh () in
  ignore (Sim.Object.put c ~bucket ~key:"k" "v");
  Sim.inject_fault c Response_lost;
  (match Sim.Object.delete c ~bucket ~key:"k" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error");
  match Sim.Object.get c ~bucket ~key:"k" () with
  | Error `Not_found -> ()
  | _ -> Alcotest.fail "should be deleted"

let test_fault_consumed_once () =
  let c = make_fresh () in
  Sim.inject_fault c Internal_error;
  ignore (Sim.Object.put c ~bucket ~key:"k" "v");
  match Sim.Object.put c ~bucket ~key:"k2" "v2" with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "fault should be consumed"

let test_fault_per_client () =
  let store, _, c1 = make () in
  let creds2 =
    Awskit.Credentials.make ~access_key_id:"admin" ~secret_access_key:"secret"
      ()
  in
  let c2 = Sim.connect store ~credentials:creds2 in
  Sim.inject_fault c1 Internal_error;
  (match Sim.Object.put c1 ~bucket ~key:"k" "v" with
  | Error _ -> ()
  | _ -> Alcotest.fail "c1 should fail");
  match Sim.Object.put c2 ~bucket ~key:"k" "v" with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "c2 should succeed"

let test_cas_ambiguity () =
  let c = make_fresh () in
  ignore
    (Sim.Object.put c ~bucket ~key:"k"
       ~preconditions:Object.Preconditions.Write.if_absent "v1");
  Sim.inject_fault c Response_lost;
  match
    Sim.Object.put c ~bucket ~key:"k"
      ~preconditions:Object.Preconditions.Write.if_absent "v2"
  with
  | Error `Precondition_failed -> ()
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should not succeed"

let test_fault_on_copy () =
  let c = make_fresh () in
  ignore (Sim.Object.put c ~bucket ~key:"src" "v");
  Sim.inject_fault c Internal_error;
  match
    Sim.Object.copy c ~src_bucket:bucket ~src_key:"src" ~dst_bucket:bucket
      ~dst_key:"dst" ()
  with
  | Error (`Request_error (500, _)) -> ()
  | _ -> Alcotest.fail "expected fault"

(* ── History ────────────────────────────────────────────────────── *)

let test_history_records () =
  let store, _, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"a" "1");
  ignore (Sim.Object.get c ~bucket ~key:"a" ());
  ignore (Sim.Object.delete c ~bucket ~key:"a" ());
  let h = Sim.history store in
  Alcotest.(check int) "3 ops" 3 (List.length h);
  let ops = List.map (fun (r : Sim.op_record) -> r.op) h in
  Alcotest.(check bool) "put/get/delete" true (ops = [ `Put; `Get; `Delete ])

let test_history_faults () =
  let store, _, c = make () in
  Sim.inject_fault c Internal_error;
  ignore (Sim.Object.put c ~bucket ~key:"a" "1");
  let h = Sim.history store in
  Alcotest.(check bool)
    "marked faulted" true
    (List.exists (fun (r : Sim.op_record) -> r.faulted) h)

let test_history_copy_and_batch () =
  let store, _, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"s" "v");
  ignore
    (Sim.Object.copy c ~src_bucket:bucket ~src_key:"s" ~dst_bucket:bucket
       ~dst_key:"d" ());
  ignore
    (Sim.Object.delete_batch c ~bucket
       ~objects:(List.map Sim.Object.Delete_object.v [ "s"; "d" ]));
  let h = Sim.history store in
  let ops = List.map (fun (r : Sim.op_record) -> r.op) h in
  Alcotest.(check bool) "has copy" true (List.mem `Copy ops);
  Alcotest.(check bool) "has delete_batch" true (List.mem `Delete_batch ops)

(* ── Multi-client CAS race ──────────────────────────────────────── *)

let test_cas_race () =
  let store, _, _ = make () in
  let mk i =
    let creds =
      Awskit.Credentials.make ~access_key_id:(Fmt.str "c%d" i)
        ~secret_access_key:"s" ()
    in
    Sim.connect store ~credentials:creds
  in
  let c1 = mk 1 in
  let c2 = mk 2 in
  let r1 =
    Sim.Object.put c1 ~bucket ~key:"race"
      ~preconditions:Object.Preconditions.Write.if_absent "c1"
  in
  let r2 =
    Sim.Object.put c2 ~bucket ~key:"race"
      ~preconditions:Object.Preconditions.Write.if_absent "c2"
  in
  let ok_count =
    (if Result.is_ok r1 then 1 else 0) + if Result.is_ok r2 then 1 else 0
  in
  Alcotest.(check int) "exactly one winner" 1 ok_count

(* ── Policy enforcement ─────────────────────────────────────────── *)

let test_policy_deny () =
  let store, _, admin = make () in
  let user =
    Sim.connect store
      ~credentials:
        (Awskit.Credentials.make ~access_key_id:"user-42" ~secret_access_key:"s"
           ())
  in
  let policy =
    {
      Policy.version = "2012-10-17";
      statements =
        [
          {
            Policy.Statement.effect_ = Policy.Effect.Allow;
            principal = Policy.Principal.User [ "admin" ];
            actions = [ "s3:*" ];
            resources =
              [
                Policy.Resource.Bucket bucket;
                Policy.Resource.Bucket_objects bucket;
              ];
          };
          {
            Policy.Statement.effect_ = Policy.Effect.Deny;
            principal = Policy.Principal.User [ "user-42" ];
            actions = [ "s3:*" ];
            resources =
              [
                Policy.Resource.Bucket bucket;
                Policy.Resource.Bucket_objects bucket;
              ];
          };
        ];
    }
  in
  ignore (Sim.Bucket.Policy.put admin ~bucket policy);
  ignore (Sim.Object.put admin ~bucket ~key:"k" "v" |> ok_or_fail "admin put");
  match Sim.Object.put user ~bucket ~key:"k2" "v2" with
  | Error `Access_denied -> ()
  | Ok _ -> Alcotest.fail "user should be denied"
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e

let test_policy_isolation () =
  let store, _, admin = make () in
  ignore (Sim.Bucket.create admin ~bucket:"env-99" ());
  let user =
    Sim.connect store
      ~credentials:
        (Awskit.Credentials.make ~access_key_id:"env-42-user"
           ~secret_access_key:"s" ())
  in
  ignore
    (Sim.Bucket.Policy.put admin ~bucket
       {
         Policy.version = "2012-10-17";
         statements =
           [
             {
               Policy.Statement.effect_ = Policy.Effect.Allow;
               principal = Policy.Principal.User [ "env-42-user" ];
               actions = [ "s3:*" ];
               resources =
                 [
                   Policy.Resource.Bucket bucket;
                   Policy.Resource.Bucket_objects bucket;
                 ];
             };
           ];
       });
  ignore
    (Sim.Bucket.Policy.put admin ~bucket:"env-99"
       {
         Policy.version = "2012-10-17";
         statements =
           [
             {
               Policy.Statement.effect_ = Policy.Effect.Deny;
               principal = Policy.Principal.User [ "env-42-user" ];
               actions = [ "s3:*" ];
               resources =
                 [
                   Policy.Resource.Bucket "env-99";
                   Policy.Resource.Bucket_objects "env-99";
                 ];
             };
           ];
       });
  ignore (Sim.Object.put user ~bucket ~key:"ok" "yes" |> ok_or_fail "allowed");
  match Sim.Object.put user ~bucket:"env-99" ~key:"no" "nope" with
  | Error `Access_denied -> ()
  | Ok _ -> Alcotest.fail "should be denied"
  | Error e -> Alcotest.failf "unexpected: %a" Error.pp e

(* ── Sim inspection ─────────────────────────────────────────────── *)

let test_sim_keys () =
  let store, _, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"b" "2");
  ignore (Sim.Object.put c ~bucket ~key:"a" "1");
  ignore (Sim.Object.put c ~bucket ~key:"c" "3");
  Alcotest.(check (list string))
    "sorted" [ "a"; "b"; "c" ] (Sim.keys store ~bucket)

let test_sim_dump () =
  let store, _, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"x" "10");
  ignore (Sim.Object.put c ~bucket ~key:"y" "20");
  let d = Sim.dump store ~bucket in
  Alcotest.(check (list (pair string string)))
    "dump"
    [ ("x", "10"); ("y", "20") ]
    d

let test_sim_object_meta () =
  let store, _, c = make () in
  ignore (Sim.Object.put c ~bucket ~key:"k" "hello");
  match Sim.object_meta store ~bucket "k" with
  | Some m ->
      Alcotest.(check int) "size" 5 m.size;
      Alcotest.(check bool) "has etag" true (String.length m.etag > 0)
  | None -> Alcotest.fail "expected meta"

(* ── Suite ──────────────────────────────────────────────────────── *)

let suite =
  [
    ( "sim:object:etag",
      [
        Alcotest.test_case "quoted MD5" `Quick test_etag_quoted;
        Alcotest.test_case "varies" `Quick test_etag_varies;
        Alcotest.test_case "stable" `Quick test_etag_stable;
      ] );
    ( "sim:object:clock",
      [ Alcotest.test_case "last_modified" `Quick test_clock_last_modified ] );
    ( "sim:object:pagination",
      [ Alcotest.test_case "pagination" `Quick test_pagination ] );
    ( "sim:object:metadata",
      [
        Alcotest.test_case "put/get metadata" `Quick test_put_get_metadata;
        Alcotest.test_case "head metadata+storage_class" `Quick
          test_head_metadata;
      ] );
    ( "sim:object:copy",
      [
        Alcotest.test_case "basic" `Quick test_sim_copy;
        Alcotest.test_case "missing source" `Quick test_sim_copy_missing;
        Alcotest.test_case "cross bucket" `Quick test_sim_copy_cross_bucket;
      ] );
    ( "sim:object:delete-batch",
      [
        Alcotest.test_case "batch" `Quick test_sim_delete_batch;
        Alcotest.test_case "conditional delete" `Quick
          test_sim_conditional_delete;
        Alcotest.test_case "conditional batch" `Quick
          test_sim_conditional_delete_batch;
      ] );
    ( "sim:object:tagging",
      [
        Alcotest.test_case "put/get/delete" `Quick test_sim_object_tags;
        Alcotest.test_case "missing key" `Quick test_sim_object_tags_missing;
      ] );
    ( "sim:bucket:crud",
      [
        Alcotest.test_case "create/delete" `Quick test_sim_bucket_create_delete;
        Alcotest.test_case "already exists" `Quick
          test_sim_bucket_already_exists;
        Alcotest.test_case "not empty" `Quick test_sim_bucket_not_empty;
        Alcotest.test_case "list" `Quick test_sim_bucket_list;
        Alcotest.test_case "no such bucket" `Quick test_sim_no_such_bucket;
      ] );
    ( "sim:bucket:policy",
      [
        Alcotest.test_case "CRUD" `Quick test_sim_bucket_policy_crud;
        Alcotest.test_case "JSON roundtrip" `Quick test_policy_json_roundtrip;
        Alcotest.test_case "invalid effect" `Quick test_policy_invalid_effect;
        Alcotest.test_case "invalid principal" `Quick
          test_policy_invalid_principal;
        Alcotest.test_case "invalid resource" `Quick
          test_policy_invalid_resource;
        Alcotest.test_case "deny overrides allow" `Quick
          test_policy_deny_overrides_allow;
        Alcotest.test_case "resource scope" `Quick test_policy_resource_scope;
      ] );
    ( "sim:faults:inject",
      [
        Alcotest.test_case "503 SlowDown" `Quick test_fault_slowdown;
        Alcotest.test_case "500 InternalError" `Quick test_fault_internal;
        Alcotest.test_case "connection reset" `Quick test_fault_reset;
        Alcotest.test_case "response lost put" `Quick
          test_fault_response_lost_put;
        Alcotest.test_case "response lost get" `Quick
          test_fault_response_lost_get;
        Alcotest.test_case "response lost delete" `Quick
          test_fault_response_lost_delete;
        Alcotest.test_case "consumed once" `Quick test_fault_consumed_once;
        Alcotest.test_case "per-client" `Quick test_fault_per_client;
        Alcotest.test_case "CAS ambiguity" `Quick test_cas_ambiguity;
        Alcotest.test_case "fault on copy" `Quick test_fault_on_copy;
      ] );
    ( "sim:faults:history",
      [
        Alcotest.test_case "records all" `Quick test_history_records;
        Alcotest.test_case "marks faults" `Quick test_history_faults;
        Alcotest.test_case "copy and batch" `Quick test_history_copy_and_batch;
      ] );
    ( "sim:object:cas-race",
      [ Alcotest.test_case "two clients" `Quick test_cas_race ] );
    ( "sim:policy:enforce",
      [
        Alcotest.test_case "deny" `Quick test_policy_deny;
        Alcotest.test_case "bucket isolation" `Quick test_policy_isolation;
      ] );
    ( "sim:inspect:helpers",
      [
        Alcotest.test_case "keys" `Quick test_sim_keys;
        Alcotest.test_case "dump" `Quick test_sim_dump;
        Alcotest.test_case "object_meta" `Quick test_sim_object_meta;
      ] );
  ]
