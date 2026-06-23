open Awskit_s3
open Awskit_s3_test

type capabilities = {
  exclusive_bucket_list : bool;
  exact_policy_json : bool;
  response_checksums : bool;
  copy_returns_current_source_version : bool;
  time_preconditions : bool;
  multipart_checksums : bool;
  deleted_bucket_tags : [ `Empty_set | `Not_found ];
  missing_version_delete : [ `Succeeds | `Invalid_argument ];
  delete_preconditions : bool;
  bucket_encryption : bool;
  bucket_cors : bool;
  bucket_public_access_block : bool;
  bucket_ownership_controls : bool;
  suspended_versioning_null : bool;
}

let strict_capabilities =
  {
    exclusive_bucket_list = true;
    exact_policy_json = true;
    response_checksums = true;
    copy_returns_current_source_version = true;
    time_preconditions = true;
    multipart_checksums = true;
    deleted_bucket_tags = `Empty_set;
    missing_version_delete = `Succeeds;
    delete_preconditions = true;
    bucket_encryption = true;
    bucket_cors = true;
    bucket_public_access_block = true;
    bucket_ownership_controls = true;
    suspended_versioning_null = true;
  }

module type SUBJECT = sig
  include S

  val bucket : Bucket_name.t
  val capabilities : capabilities
  val fresh : unit -> connection
  val cleanup : connection -> unit io
  val run : 'a io -> 'a

  val read_response_body :
    Reader.t -> bytes -> off:int -> len:int -> (int, Error.t) result io
end

module Make (Client : SUBJECT) = struct
  let bucket = Client.bucket
  let bucket_string = Bucket_name.to_string bucket
  let return = Client.Runtime.IO.return
  let ( let* ) = Client.Runtime.IO.bind

  let with_fresh test =
    let conn = Client.fresh () in
    Fun.protect
      ~finally:(fun () -> Client.run (Client.cleanup conn))
      (fun () -> test conn)

  let test_case name speed test =
    Alcotest.test_case name speed (fun () -> with_fresh test)

  let run_result label io = Client.run io |> ok_or_fail label
  let run_expect_not_found label io = Client.run io |> expect_not_found label

  let run_expect_status label status io =
    Client.run io |> expect_status label status

  let run_expect_precondition_failed label io =
    Client.run io |> expect_precondition_failed label

  let run_expect_not_modified label io =
    Client.run io |> expect_not_modified label

  let run_expect_validation label io = Client.run io |> expect_validation label

  let is_body_error error =
    let open Awskit.Error in
    match kind error with Body _ -> true | _ -> false

  let create_bucket conn =
    ignore (Client.Bucket.create conn ~bucket () |> run_result "create bucket")

  let put_object_string conn ~bucket ~key ?options value =
    Client.Object.put conn ~bucket ~key ?options
      ~body:(Client.Body.of_string value)
      ()
    |> Client.run

  let get_object_as_string conn ~bucket ~key ?options ~max_bytes () =
    Client.Object.get conn ~bucket ~key ?options
      ~consume:(Client.Reader.to_string ~max_bytes)
      ()
    |> Client.run

  let ok_get_or_fail label result = ok_or_fail label result

  let put_string conn key value =
    let key = object_key key in
    ignore
      (put_object_string conn ~bucket ~key value
      |> ok_or_fail ("put " ^ Object_key.to_string key))

  let require_version label = function
    | Some version_id -> version_id
    | None -> Alcotest.failf "%s: expected version id" label

  let version_string = Option.map Object.Version_id.to_string

  let test_bucket_lifecycle conn =
    Alcotest.(check bool)
      "missing bucket" false
      (Client.Bucket.exists conn ~bucket () |> run_result "exists missing");
    create_bucket conn;
    let head = Client.Bucket.head conn ~bucket () |> run_result "head bucket" in
    Alcotest.(check string)
      "bucket name" bucket_string
      (Bucket_name.to_string head.name);
    Alcotest.(check bool)
      "created bucket" true
      (Client.Bucket.exists conn ~bucket () |> run_result "exists present");
    let list_result = Client.Bucket.list conn |> run_result "list buckets" in
    let buckets =
      List.map
        (fun (info : Bucket.info) -> Bucket_name.to_string info.name)
        list_result.buckets
    in
    if Client.capabilities.exclusive_bucket_list then
      Alcotest.(check (list string)) "bucket list" [ bucket_string ] buckets
    else
      Alcotest.(check bool)
        "bucket appears in list" true
        (List.mem bucket_string buckets);
    let location =
      Client.Bucket.get_location conn ~bucket () |> run_result "location"
    in
    Alcotest.(check (option string))
      "location" (Some "us-east-1")
      (Option.map Region.to_string location.region);
    ignore (Client.Bucket.delete conn ~bucket () |> run_result "delete bucket");
    Alcotest.(check bool)
      "deleted bucket" false
      (Client.Bucket.exists conn ~bucket () |> run_result "exists deleted")

  let test_object_buffer_lifecycle conn =
    create_bucket conn;
    let options =
      {
        Put_object.default_options with
        content_type = Some (content_type "text/plain");
        metadata = Metadata.of_list_exn [ ("origin", "contract") ];
        tags = tag_set [ tag "env" "test" ];
        checksum =
          Some
            {
              Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
              value = "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=";
            };
      }
    in
    let put =
      put_object_string conn ~bucket ~key:(object_key "hello.txt") ~options
        "hello"
      |> ok_or_fail "put object"
    in
    Alcotest.(check bool) "put etag" true (Option.is_some put.etag);
    check_checksum "put checksum" Object.Checksum.Algorithm.Sha256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
    let result =
      get_object_as_string conn ~bucket ~key:(object_key "hello.txt")
        ~max_bytes:16L ()
      |> ok_get_or_fail "get object"
    in
    Alcotest.(check string) "body" "hello" result.Get_object.value;
    Alcotest.(check (option string))
      "content type" (Some "text/plain")
      (Option.map Content_type.to_string result.content_type);
    Alcotest.(check (option string))
      "metadata" (Some "contract")
      (List.assoc_opt "origin" (Metadata.to_list result.metadata));
    if Client.capabilities.response_checksums then
      check_checksum "get checksum" Object.Checksum.Algorithm.Sha256
        "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" result.checksum;
    let head =
      Client.Object.head conn ~bucket ~key:(object_key "hello.txt") ()
      |> run_result "head"
    in
    Alcotest.(check (option int64))
      "content length" (Some 5L) head.content_length;
    if Client.capabilities.response_checksums then
      check_checksum "head checksum" Object.Checksum.Algorithm.Sha256
        "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" head.checksum;
    Alcotest.(check bool)
      "object exists" true
      (Client.Object.exists conn ~bucket ~key:(object_key "hello.txt")
      |> run_result "exists");
    ignore
      (Client.Object.delete conn ~bucket ~key:(object_key "hello.txt") ()
      |> run_result "delete");
    Alcotest.(check bool)
      "object deleted" false
      (Client.Object.exists conn ~bucket ~key:(object_key "hello.txt")
      |> run_result "exists deleted")

  let test_streaming_get conn =
    create_bucket conn;
    ignore
      (Client.Object.put conn ~bucket ~key:(object_key "stream.txt")
         ~body:(Client.Body.of_string "abcdef")
         ()
      |> run_result "put streaming");
    let consume reader =
      let bytes = Bytes.create 4 in
      let* result = Client.read_response_body reader bytes ~off:0 ~len:4 in
      match result with
      | Error _ as error -> return error
      | Ok read -> return (Ok (Bytes.sub_string bytes 0 read))
    in
    let result =
      Client.Object.get conn ~bucket ~key:(object_key "stream.txt") ~consume ()
      |> run_result "get streaming"
    in
    Alcotest.(check string) "partial body" "abcd" result.Get_object.value

  let test_large_streaming_roundtrip conn =
    create_bucket conn;
    let chunk = String.make 8192 'x' in
    let chunks = 320 in
    let body = String.concat "" (List.init chunks (fun _ -> chunk)) in
    ignore
      (Client.Object.put conn ~bucket
         ~key:(object_key "large-stream.bin")
         ~body:(Client.Body.of_string body)
         ()
      |> run_result "put large stream");
    let consume reader =
      let bytes = Bytes.create 16384 in
      let rec loop total first last =
        let* result =
          Client.read_response_body reader bytes ~off:0
            ~len:(Bytes.length bytes)
        in
        match result with
        | Error _ as error -> return error
        | Ok 0 -> return (Ok (total, first, last))
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
    let result =
      Client.Object.get conn ~bucket
        ~key:(object_key "large-stream.bin")
        ~consume ()
      |> run_result "get large stream"
    in
    let length, first, last = result.Get_object.value in
    Alcotest.(check int) "large stream bytes" (String.length body) length;
    Alcotest.(check (option char)) "first byte" (Some 'x') first;
    Alcotest.(check (option char)) "last byte" (Some 'x') last;
    Alcotest.(check (option int64))
      "large content length"
      (Some (Int64.of_int (String.length body)))
      result.content_length

  let test_range_reads_and_metadata_copy conn =
    create_bucket conn;
    let put_options =
      {
        Put_object.default_options with
        content_type = Some (content_type "text/plain");
        metadata =
          Metadata.of_list_exn [ ("origin", "source"); ("mode", "copy") ];
      }
    in
    ignore
      (put_object_string conn ~bucket ~key:(object_key "range.txt")
         ~options:put_options "abcdefghij"
      |> ok_or_fail "put range source");
    let range_options =
      {
        Get_object.default_options with
        range = Some (Range.bytes_exn ~start:2L ~finish:5L);
      }
    in
    let result =
      get_object_as_string conn ~bucket ~key:(object_key "range.txt")
        ~options:range_options ~max_bytes:16L ()
      |> ok_get_or_fail "get byte range"
    in
    Alcotest.(check string) "range body" "cdef" result.Get_object.value;
    Alcotest.(check int)
      "range status" 206
      (Awskit.Response.status result.response);
    Alcotest.(check (option int64))
      "range content length" (Some 4L) result.content_length;
    (match result.content_range with
    | None -> Alcotest.fail "expected typed content range"
    | Some content_range ->
        Alcotest.(check int64) "range start" 2L content_range.start;
        Alcotest.(check int64) "range finish" 5L content_range.finish;
        Alcotest.(check (option int64))
          "range complete length" (Some 10L) content_range.complete_length);
    Alcotest.(check (option string))
      "range content range" (Some "bytes 2-5/10")
      (Awskit.Response.header result.response "content-range");
    let suffix_options =
      { Get_object.default_options with range = Some (Range.suffix_exn 3L) }
    in
    let result =
      get_object_as_string conn ~bucket ~key:(object_key "range.txt")
        ~options:suffix_options ~max_bytes:16L ()
      |> ok_get_or_fail "get suffix range"
    in
    Alcotest.(check string) "suffix body" "hij" result.Get_object.value;
    let invalid_range_options =
      { Get_object.default_options with range = Some (Range.from_exn 99L) }
    in
    expect_status "invalid range" 416
      (get_object_as_string conn ~bucket ~key:(object_key "range.txt")
         ~options:invalid_range_options ~max_bytes:16L ());
    ignore
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:(object_key "range.txt") ~destination_bucket:bucket
         ~destination_key:(object_key "copied.txt") ()
      |> run_result "copy metadata");
    let copied =
      Client.Object.head conn ~bucket ~key:(object_key "copied.txt") ()
      |> run_result "head copied"
    in
    Alcotest.(check (option string))
      "copied metadata" (Some "source")
      (List.assoc_opt "origin" (Metadata.to_list copied.metadata));
    let replace_options =
      {
        Copy_object.default_options with
        metadata_directive =
          Some (`Replace (Metadata.of_list_exn [ ("origin", "replacement") ]));
      }
    in
    ignore
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:(object_key "range.txt") ~destination_bucket:bucket
         ~destination_key:(object_key "replaced.txt")
         ~options:replace_options ()
      |> run_result "copy replace metadata");
    let replaced =
      Client.Object.head conn ~bucket ~key:(object_key "replaced.txt") ()
      |> run_result "head replaced"
    in
    Alcotest.(check (option string))
      "replaced metadata" (Some "replacement")
      (List.assoc_opt "origin" (Metadata.to_list replaced.metadata));
    Alcotest.(check (option string))
      "removed copied metadata" None
      (List.assoc_opt "mode" (Metadata.to_list replaced.metadata))

  let test_list_copy_delete_objects conn =
    create_bucket conn;
    put_string conn "logs/a.txt" "a";
    put_string conn "logs/b.txt" "b";
    put_string conn "other.txt" "other";
    ignore
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:(object_key "logs/a.txt") ~destination_bucket:bucket
         ~destination_key:(object_key "copy/a.txt") ()
      |> run_result "copy");
    let list_options =
      List_objects_v2.options_exn
        ~prefix:(Object_key.Prefix.of_string_exn "logs/")
        ~max_keys:1 ()
    in
    let page =
      Client.Object.list conn ~bucket ~options:list_options ()
      |> run_result "list"
    in
    Alcotest.(check int) "listed count" 1 (List.length page.objects);
    Alcotest.(check bool) "truncated" true page.is_truncated;
    let delete_object key = Delete_objects.object_ ~key:(object_key key) () in
    ignore
      (Client.Object.delete_objects conn ~bucket
         ~objects:[ delete_object "logs/a.txt"; delete_object "logs/b.txt" ]
         ()
      |> run_result "delete objects");
    let keys =
      Client.Object.List.keys conn ~bucket ~max_pages:10 ()
      |> run_result "keys"
      |> List.map Object_key.to_string
    in
    Alcotest.(check (list string))
      "remaining keys"
      [ "copy/a.txt"; "other.txt" ]
      keys

  let test_object_tagging conn =
    create_bucket conn;
    put_string conn "tagged.txt" "body";
    let tags = tag_set [ tag "env" "dev"; tag "owner" "sdk" ] in
    ignore
      (Client.Object.Tagging.put conn ~bucket ~key:(object_key "tagged.txt")
         ~tags ()
      |> run_result "put object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:(object_key "tagged.txt") ()
      |> run_result "get object tags"
    in
    Alcotest.(check int) "tag count" 2 (tag_count result.tags);
    ignore
      (Client.Object.Tagging.delete conn ~bucket ~key:(object_key "tagged.txt")
         ()
      |> run_result "delete object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:(object_key "tagged.txt") ()
      |> run_result "get deleted tags"
    in
    Alcotest.(check int) "deleted tag count" 0 (tag_count result.tags)

  let test_object_versioning conn =
    create_bucket conn;
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         ~status:Bucket.Versioning.Status.Enabled ()
      |> run_result "enable versioning");
    let put1 =
      put_object_string conn ~bucket ~key:(object_key "versioned.txt") "one"
      |> ok_or_fail "put version one"
    in
    let v1 = require_version "put version one" put1.version_id in
    let put2 =
      put_object_string conn ~bucket ~key:(object_key "versioned.txt") "two"
      |> ok_or_fail "put version two"
    in
    let v2 = require_version "put version two" put2.version_id in
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "versioned.txt")
        ~max_bytes:16L ()
      |> ok_get_or_fail "get current version"
    in
    Alcotest.(check string) "current body" "two" result.Get_object.value;
    Alcotest.(check (option string))
      "current version"
      (Some (Object.Version_id.to_string v2))
      (version_string result.version_id);
    let previous_options =
      { Get_object.default_options with version_id = Some v1 }
    in
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "versioned.txt")
        ~max_bytes:16L ~options:previous_options ()
      |> ok_get_or_fail "get previous version"
    in
    Alcotest.(check string) "previous body" "one" result.Get_object.value;
    Alcotest.(check (option string))
      "previous version"
      (Some (Object.Version_id.to_string v1))
      (version_string result.version_id);
    let head_options =
      { Head_object.default_options with version_id = Some v1 }
    in
    let head =
      Client.Object.head conn ~bucket
        ~key:(object_key "versioned.txt")
        ~options:head_options ()
      |> run_result "head previous version"
    in
    Alcotest.(check (option int64))
      "previous length" (Some 3L) head.content_length;
    Alcotest.(check (option string))
      "previous head version"
      (Some (Object.Version_id.to_string v1))
      (version_string head.version_id);
    let copy =
      Client.Object.copy conn ~source_bucket:bucket
        ~source_key:(object_key "versioned.txt")
        ~destination_bucket:bucket ~destination_key:(object_key "copy.txt") ()
      |> run_result "copy versioned object"
    in
    ignore (require_version "copy destination version" copy.version_id);
    if Client.capabilities.copy_returns_current_source_version then
      Alcotest.(check (option string))
        "copy source version"
        (Some (Object.Version_id.to_string v2))
        (version_string copy.copy_source_version_id);
    let copy_previous_options =
      { Copy_object.default_options with source_version_id = Some v1 }
    in
    let copy_previous =
      Client.Object.copy conn ~source_bucket:bucket
        ~source_key:(object_key "versioned.txt")
        ~destination_bucket:bucket
        ~destination_key:(object_key "copy-previous.txt")
        ~options:copy_previous_options ()
      |> run_result "copy previous version"
    in
    Alcotest.(check (option string))
      "copy previous source version"
      (Some (Object.Version_id.to_string v1))
      (version_string copy_previous.copy_source_version_id);
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "copy-previous.txt")
        ~max_bytes:16L ()
      |> ok_get_or_fail "get copied previous"
    in
    Alcotest.(check string) "copied previous body" "one" result.Get_object.value;
    let upload =
      Client.Multipart.create_upload conn ~bucket
        ~key:(object_key "multi-versioned.txt")
        ()
      |> run_result "create versioned multipart"
    in
    let part =
      Client.Multipart.upload_part conn ~upload:upload.upload
        ~part_number:(Multipart.Part_number.of_int_exn 1)
        ~body:(Client.Body.of_string "multipart")
        ()
      |> run_result "upload versioned part"
    in
    let complete =
      Client.Multipart.complete_upload conn ~upload:upload.upload
        ~parts:[ part.part ] ()
      |> run_result "complete versioned multipart"
    in
    ignore (require_version "complete multipart version" complete.version_id);
    let deleted =
      Client.Object.delete conn ~bucket ~key:(object_key "versioned.txt") ()
      |> run_result "delete current version"
    in
    let marker = require_version "delete marker version" deleted.version_id in
    Alcotest.(check (option bool))
      "delete marker" (Some true) deleted.delete_marker;
    Alcotest.(check bool)
      "delete marker hides current" false
      (Client.Object.exists conn ~bucket ~key:(object_key "versioned.txt")
      |> run_result "exists after delete marker");
    let marker_get_options =
      { Get_object.default_options with version_id = Some marker }
    in
    expect_status "get delete marker version" 405
      (get_object_as_string conn ~bucket
         ~key:(object_key "versioned.txt")
         ~max_bytes:16L ~options:marker_get_options ());
    let versions_options =
      List_object_versions.options_exn
        ~prefix:(Object_key.Prefix.of_string_exn "versioned.txt")
        ~max_keys:2 ()
    in
    let first_versions_page =
      Client.Object.list_versions conn ~bucket ~options:versions_options ()
      |> run_result "list object versions page"
    in
    Alcotest.(check bool)
      "version page truncated" true first_versions_page.is_truncated;
    Alcotest.(check int)
      "version page entries" 2
      (List.length first_versions_page.versions
      + List.length first_versions_page.delete_markers);
    let all_versions =
      Client.Object.Versions.object_versions conn ~bucket
        ~options:versions_options ~max_pages:10 ()
      |> run_result "paginate object versions"
    in
    Alcotest.(check (list string))
      "listed object versions"
      [ Object.Version_id.to_string v2; Object.Version_id.to_string v1 ]
      (List.filter_map
         (fun (version : List_object_versions.object_version) ->
           Option.map Object.Version_id.to_string version.version_id)
         all_versions);
    let all_markers =
      Client.Object.Versions.delete_markers conn ~bucket
        ~options:versions_options ~max_pages:10 ()
      |> run_result "paginate delete markers"
    in
    Alcotest.(check (list string))
      "listed delete markers"
      [ Object.Version_id.to_string marker ]
      (List.filter_map
         (fun (marker : List_object_versions.delete_marker) ->
           Option.map Object.Version_id.to_string marker.version_id)
         all_markers);
    let missing_version_options =
      {
        Delete_object.default_options with
        version_id = Some (Object.Version_id.of_string_exn "missing-version");
      }
    in
    (match Client.capabilities.missing_version_delete with
    | `Succeeds ->
        ignore
          (Client.Object.delete conn ~bucket
             ~key:(object_key "versioned.txt")
             ~options:missing_version_options ()
          |> run_result "delete missing version id");
        Alcotest.(check bool)
          "missing version delete keeps marker" false
          (Client.Object.exists conn ~bucket ~key:(object_key "versioned.txt")
          |> run_result "exists after missing version delete")
    | `Invalid_argument ->
        run_expect_status "delete missing version id" 400
          (Client.Object.delete conn ~bucket
             ~key:(object_key "versioned.txt")
             ~options:missing_version_options ()));
    let version_two_options =
      { Get_object.default_options with version_id = Some v2 }
    in
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "versioned.txt")
        ~max_bytes:16L ~options:version_two_options ()
      |> ok_get_or_fail "get hidden version"
    in
    Alcotest.(check string) "hidden body" "two" result.Get_object.value;
    let delete_marker_options =
      { Delete_object.default_options with version_id = Some marker }
    in
    ignore
      (Client.Object.delete conn ~bucket
         ~key:(object_key "versioned.txt")
         ~options:delete_marker_options ()
      |> run_result "delete delete marker");
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "versioned.txt")
        ~max_bytes:16L ()
      |> ok_get_or_fail "get restored current"
    in
    Alcotest.(check string) "restored current" "two" result.Get_object.value;
    let delete_v2_options =
      { Delete_object.default_options with version_id = Some v2 }
    in
    ignore
      (Client.Object.delete conn ~bucket
         ~key:(object_key "versioned.txt")
         ~options:delete_v2_options ()
      |> run_result "delete version two");
    let result =
      get_object_as_string conn ~bucket
        ~key:(object_key "versioned.txt")
        ~max_bytes:16L ()
      |> ok_get_or_fail "get remaining version"
    in
    Alcotest.(check string) "remaining current" "one" result.Get_object.value;
    if Client.capabilities.delete_preconditions then (
      let wrong_delete =
        Delete_objects.object_
          ~key:(object_key "versioned.txt")
          ~version_id:v1
          ~etag:(Object.Etag.of_string_exn "\"wrong\"")
          ()
      in
      let failed_many =
        Client.Object.delete_objects conn ~bucket ~objects:[ wrong_delete ] ()
        |> run_result "delete objects wrong etag"
      in
      Alcotest.(check int)
        "delete objects precondition errors" 1
        (List.length failed_many.errors);
      Alcotest.(check int)
        "delete objects failed deleted" 0
        (List.length failed_many.deleted));
    let correct_delete =
      Delete_objects.object_ ~key:(object_key "versioned.txt") ~version_id:v1 ()
    in
    let deleted_many =
      Client.Object.delete_objects conn ~bucket ~objects:[ correct_delete ] ()
      |> run_result "delete objects version"
    in
    Alcotest.(check int)
      "delete objects deleted version" 1
      (List.length deleted_many.deleted);
    Alcotest.(check bool)
      "all versions removed" false
      (Client.Object.exists conn ~bucket ~key:(object_key "versioned.txt")
      |> run_result "exists after delete objects version");
    if Client.capabilities.suspended_versioning_null then (
      let enabled_put =
        put_object_string conn ~bucket
          ~key:(object_key "suspended.txt")
          "enabled"
        |> ok_or_fail "put before suspend"
      in
      let enabled_version =
        require_version "put before suspend" enabled_put.version_id
      in
      ignore
        (Client.Bucket.Versioning.put conn ~bucket
           ~status:Bucket.Versioning.Status.Suspended ()
        |> run_result "suspend versioning");
      let suspended_put =
        put_object_string conn ~bucket
          ~key:(object_key "suspended.txt")
          "suspended"
        |> ok_or_fail "put while suspended"
      in
      Alcotest.(check (option string))
        "suspended put version" (Some "null")
        (version_string suspended_put.version_id);
      let result =
        get_object_as_string conn ~bucket
          ~key:(object_key "suspended.txt")
          ~max_bytes:16L ()
        |> ok_get_or_fail "get suspended current"
      in
      Alcotest.(check string)
        "suspended current body" "suspended" result.Get_object.value;
      let enabled_options =
        { Get_object.default_options with version_id = Some enabled_version }
      in
      let result =
        get_object_as_string conn ~bucket
          ~key:(object_key "suspended.txt")
          ~max_bytes:16L ~options:enabled_options ()
        |> ok_get_or_fail "get enabled version after suspend"
      in
      Alcotest.(check string)
        "enabled version body" "enabled" result.Get_object.value;
      let suspended_versions =
        Client.Object.Versions.object_versions conn ~bucket
          ~options:
            (List_object_versions.options_exn
               ~prefix:(Object_key.Prefix.of_string_exn "suspended.txt")
               ())
          ~max_pages:10 ()
        |> run_result "list suspended versions"
      in
      Alcotest.(check bool)
        "listed null object version" true
        (List.exists
           (fun (version : List_object_versions.object_version) ->
             version_string version.version_id = Some "null")
           suspended_versions);
      Alcotest.(check bool)
        "listed enabled object version" true
        (List.exists
           (fun (version : List_object_versions.object_version) ->
             version_string version.version_id
             = Some (Object.Version_id.to_string enabled_version))
           suspended_versions);
      let suspended_delete =
        Client.Object.delete conn ~bucket ~key:(object_key "suspended.txt") ()
        |> run_result "delete while suspended"
      in
      Alcotest.(check (option bool))
        "suspended delete marker" (Some true) suspended_delete.delete_marker;
      Alcotest.(check (option string))
        "suspended delete marker version" (Some "null")
        (version_string suspended_delete.version_id);
      Alcotest.(check bool)
        "suspended marker hides current" false
        (Client.Object.exists conn ~bucket ~key:(object_key "suspended.txt")
        |> run_result "exists after suspended delete");
      let null_version = Object.Version_id.of_string_exn "null" in
      let null_marker_options =
        { Delete_object.default_options with version_id = Some null_version }
      in
      ignore
        (Client.Object.delete conn ~bucket
           ~key:(object_key "suspended.txt")
           ~options:null_marker_options ()
        |> run_result "delete suspended marker");
      let result =
        get_object_as_string conn ~bucket
          ~key:(object_key "suspended.txt")
          ~max_bytes:16L ()
        |> ok_get_or_fail "get restored enabled version"
      in
      Alcotest.(check string)
        "restored enabled body" "enabled" result.Get_object.value)

  let sample_policy_json =
    Fmt.str
      {|{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":"*","Action":"s3:GetObject","Resource":"arn:aws:s3:::%s/*"}]}|}
      bucket_string

  let sample_policy = Policy.of_json sample_policy_json |> ok_or_fail "policy"

  let test_bucket_config_roundtrips conn =
    create_bucket conn;
    ignore
      (Client.Bucket.Policy.put conn ~bucket ~policy:sample_policy ()
      |> run_result "put policy");
    let policy =
      Client.Bucket.Policy.get conn ~bucket () |> run_result "policy"
    in
    if Client.capabilities.exact_policy_json then
      Alcotest.(check string)
        "policy" sample_policy_json (Policy.to_json policy)
    else
      Alcotest.(check bool)
        "policy response JSON" true
        (String.length (Policy.to_json policy) > 0);
    ignore
      (Client.Bucket.Policy.delete conn ~bucket () |> run_result "delete policy");
    run_expect_not_found "deleted policy"
      (Client.Bucket.Policy.get conn ~bucket ());
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         ~status:Bucket.Versioning.Status.Enabled ()
      |> run_result "put versioning");
    let versioning =
      Client.Bucket.Versioning.get conn ~bucket ()
      |> run_result "get versioning"
    in
    Alcotest.(check bool)
      "versioning enabled" true
      (versioning.status = Some Bucket.Versioning.Status.Enabled);
    let bucket_tags = tag_set [ tag "team" "storage" ] in
    ignore
      (Client.Bucket.Tagging.put conn ~bucket ~tags:bucket_tags ()
      |> run_result "put bucket tags");
    let result =
      Client.Bucket.Tagging.get conn ~bucket () |> run_result "get bucket tags"
    in
    Alcotest.(check int) "bucket tag count" 1 (tag_count result.tags);
    ignore
      (Client.Bucket.Tagging.delete conn ~bucket ()
      |> run_result "delete bucket tags");
    (match Client.capabilities.deleted_bucket_tags with
    | `Empty_set ->
        let result =
          Client.Bucket.Tagging.get conn ~bucket ()
          |> run_result "get deleted tags"
        in
        Alcotest.(check int)
          "deleted bucket tag count" 0 (tag_count result.tags)
    | `Not_found ->
        run_expect_not_found "deleted bucket tags"
          (Client.Bucket.Tagging.get conn ~bucket ()));
    if Client.capabilities.bucket_encryption then (
      let encryption =
        {
          Bucket.Encryption.rules =
            [
              {
                Bucket.Encryption.Rule.sse_algorithm =
                  Some Bucket.Encryption.Algorithm.Aes256;
                kms_master_key_id = None;
                bucket_key_enabled = None;
                blocked_encryption_types = [];
              };
            ];
        }
      in
      ignore
        (Client.Bucket.Encryption.put conn ~bucket ~config:encryption ()
        |> run_result "put encryption");
      let result =
        Client.Bucket.Encryption.get conn ~bucket ()
        |> run_result "get encryption"
      in
      Alcotest.(check int)
        "encryption rule count" 1
        (List.length result.config.rules);
      ignore
        (Client.Bucket.Encryption.delete conn ~bucket ()
        |> run_result "delete encryption");
      run_expect_not_found "deleted encryption"
        (Client.Bucket.Encryption.get conn ~bucket ()));
    if Client.capabilities.bucket_cors then (
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
      ignore
        (Client.Bucket.Cors.put conn ~bucket ~config:cors ()
        |> run_result "put cors");
      let result =
        Client.Bucket.Cors.get conn ~bucket () |> run_result "get cors"
      in
      Alcotest.(check int) "cors rule count" 1 (List.length result.config.rules);
      ignore
        (Client.Bucket.Cors.delete conn ~bucket () |> run_result "delete cors");
      run_expect_not_found "deleted cors"
        (Client.Bucket.Cors.get conn ~bucket ()));
    if Client.capabilities.bucket_public_access_block then (
      let public_access_block =
        {
          Bucket.Public_access_block.block_public_acls = true;
          ignore_public_acls = false;
          block_public_policy = true;
          restrict_public_buckets = false;
        }
      in
      ignore
        (Client.Bucket.Public_access_block.put conn ~bucket
           ~config:public_access_block ()
        |> run_result "put public access block");
      let result =
        Client.Bucket.Public_access_block.get conn ~bucket ()
        |> run_result "get public access block"
      in
      Alcotest.(check bool)
        "block public acls" true result.config.block_public_acls;
      ignore
        (Client.Bucket.Public_access_block.delete conn ~bucket ()
        |> run_result "delete public access block");
      run_expect_not_found "deleted public access block"
        (Client.Bucket.Public_access_block.get conn ~bucket ()));
    if Client.capabilities.bucket_ownership_controls then (
      let ownership =
        {
          Bucket.Ownership_controls.object_ownership =
            Bucket.Ownership_controls.Object_ownership.Bucket_owner_enforced;
        }
      in
      ignore
        (Client.Bucket.Ownership_controls.put conn ~bucket ~config:ownership ()
        |> run_result "put ownership");
      let result =
        Client.Bucket.Ownership_controls.get conn ~bucket ()
        |> run_result "get ownership"
      in
      Alcotest.(check string)
        "ownership" "BucketOwnerEnforced"
        (Bucket.Ownership_controls.Object_ownership.to_string
           result.config.object_ownership);
      ignore
        (Client.Bucket.Ownership_controls.delete conn ~bucket ()
        |> run_result "delete ownership");
      run_expect_not_found "deleted ownership"
        (Client.Bucket.Ownership_controls.get conn ~bucket ()))

  let test_buffer_limit conn =
    create_bucket conn;
    put_string conn "large.txt" "abcdef";
    match
      get_object_as_string conn ~bucket ~key:(object_key "large.txt")
        ~max_bytes:3L ()
    with
    | Error error when is_body_error error -> ()
    | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected max_bytes failure"

  let test_object_preconditions conn =
    create_bucket conn;
    let put =
      put_object_string conn ~bucket ~key:(object_key "conditional.txt") "body"
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
        Put_object.default_options with
        preconditions = Object.Preconditions.Write.if_absent;
      }
    in
    expect_precondition_failed "put if absent existing"
      (put_object_string conn ~bucket
         ~key:(object_key "conditional.txt")
         ~options:absent_options "new-body");
    let match_options =
      {
        Put_object.default_options with
        preconditions = Object.Preconditions.Write.if_etag bad_etag;
      }
    in
    expect_precondition_failed "put if match bad etag"
      (put_object_string conn ~bucket
         ~key:(object_key "conditional.txt")
         ~options:match_options "new-body");
    let read_options =
      {
        Get_object.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_match = Some (Object.Etag_condition.Etag bad_etag);
          };
      }
    in
    expect_precondition_failed "get if match bad etag"
      (get_object_as_string conn ~bucket
         ~key:(object_key "conditional.txt")
         ~options:read_options ~max_bytes:16L ());
    let not_modified_options =
      {
        Get_object.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_none_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    expect_not_modified "get if none match etag"
      (get_object_as_string conn ~bucket
         ~key:(object_key "conditional.txt")
         ~options:not_modified_options ~max_bytes:16L ());
    let head_not_modified_options =
      {
        Head_object.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_modified_since = Some test_time;
          };
      }
    in
    if Client.capabilities.time_preconditions then
      run_expect_not_modified "head if modified since same time"
        (Client.Object.head conn ~bucket
           ~key:(object_key "conditional.txt")
           ~options:head_not_modified_options ());
    let stale_options =
      {
        Head_object.default_options with
        preconditions =
          {
            Object.Preconditions.Read.none with
            if_unmodified_since = Some Ptime.epoch;
          };
      }
    in
    if Client.capabilities.time_preconditions then
      run_expect_precondition_failed "head if unmodified since stale"
        (Client.Object.head conn ~bucket
           ~key:(object_key "conditional.txt")
           ~options:stale_options ());
    let copy_options =
      {
        Copy_object.default_options with
        source_preconditions =
          {
            Object.Preconditions.Copy_source.none with
            if_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    ignore
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:(object_key "conditional.txt")
         ~destination_bucket:bucket
         ~destination_key:(object_key "conditional-copy.txt")
         ~options:copy_options ()
      |> run_result "copy if match etag");
    let copy_fail_options =
      {
        Copy_object.default_options with
        source_preconditions =
          {
            Object.Preconditions.Copy_source.none with
            if_none_match = Some (Object.Etag_condition.Etag etag);
          };
      }
    in
    run_expect_precondition_failed "copy if none match etag"
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:(object_key "conditional.txt")
         ~destination_bucket:bucket
         ~destination_key:(object_key "conditional-copy-fail.txt")
         ~options:copy_fail_options ());
    let delete_key = object_key "delete-conditional.txt" in
    let delete_put =
      put_object_string conn ~bucket ~key:delete_key "abc"
      |> ok_or_fail "put delete conditional"
    in
    let delete_etag =
      match delete_put.etag with
      | Some etag -> etag
      | None -> Alcotest.fail "expected delete put etag"
    in
    let delete_options =
      {
        Delete_object.default_options with
        preconditions =
          {
            Object.Preconditions.Delete.if_match =
              Some (Object.Etag_condition.Etag delete_etag);
          };
      }
    in
    ignore
      (Client.Object.delete conn ~bucket ~key:delete_key ~options:delete_options
         ()
      |> run_result "delete preconditions");
    let delete_fail_key_name = "delete-conditional-fail.txt" in
    let delete_fail_key = object_key delete_fail_key_name in
    put_string conn delete_fail_key_name "abc";
    let delete_fail_options =
      {
        Delete_object.default_options with
        preconditions =
          {
            Object.Preconditions.Delete.if_match =
              Some
                (Object.Etag_condition.Etag
                   (Object.Etag.of_string_exn "\"wrong\""));
          };
      }
    in
    if Client.capabilities.delete_preconditions then (
      run_expect_precondition_failed "delete if match mismatch"
        (Client.Object.delete conn ~bucket ~key:delete_fail_key
           ~options:delete_fail_options ());
      run_expect_precondition_failed "delete missing with precondition"
        (Client.Object.delete conn ~bucket
           ~key:(object_key "missing-delete-conditional.txt")
           ~options:delete_options ()))

  let test_multipart_lifecycle conn =
    create_bucket conn;
    let part1_body = String.make Transfer.min_part_size 'h' in
    let part2_body = "world" in
    let completed_size = String.length part1_body + String.length part2_body in
    let upload =
      (if Client.capabilities.multipart_checksums then
         let options =
           {
             Create_multipart_upload.default_options with
             checksum_algorithm = Some Object.Checksum.Algorithm.Sha256;
           }
         in
         Client.Multipart.create_upload conn ~bucket
           ~key:(object_key "multi.bin") ~options ()
       else
         Client.Multipart.create_upload conn ~bucket
           ~key:(object_key "multi.bin") ())
      |> run_result "create multipart"
    in
    let upload_part part_number body label =
      (if Client.capabilities.multipart_checksums then
         let options =
           {
             Upload_part.default_options with
             checksum =
               Some
                 {
                   Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
                   value = "provided-sha256";
                 };
           }
         in
         Client.Multipart.upload_part conn ~upload:upload.upload ~part_number
           ~body:(Client.Body.of_string body)
           ~options ()
       else
         Client.Multipart.upload_part conn ~upload:upload.upload ~part_number
           ~body:(Client.Body.of_string body)
           ())
      |> run_result label
    in
    let part1 =
      upload_part
        (Multipart.Part_number.of_int_exn 1)
        part1_body "upload part 1"
    in
    let part2 =
      upload_part
        (Multipart.Part_number.of_int_exn 2)
        part2_body "upload part 2"
    in
    let list_options = List_parts.options_exn ~max_parts:1 () in
    let page =
      Client.Multipart.list_parts conn ~upload:upload.upload
        ~options:list_options ()
      |> run_result "list multipart parts"
    in
    Alcotest.(check int) "first page count" 1 (List.length page.parts);
    Alcotest.(check bool) "parts truncated" true page.is_truncated;
    (match page.parts with
    | [ part ] when Client.capabilities.multipart_checksums ->
        check_checksum "listed part checksum" Object.Checksum.Algorithm.Sha256
          "provided-sha256" part.checksum
    | [ _ ] -> ()
    | _ -> Alcotest.fail "expected first multipart page");
    let parts =
      Client.Multipart.List_parts.parts conn ~upload:upload.upload
        ~options:list_options ()
      |> run_result "paginate multipart parts"
    in
    Alcotest.(check (list int))
      "part numbers" [ 1; 2 ]
      (List.map
         (fun (part : List_parts.part_info) ->
           Multipart.Part_number.to_int part.part_number)
         parts);
    let complete =
      Client.Multipart.complete_upload conn ~upload:upload.upload
        ~parts:[ part1.part; part2.part ] ()
      |> run_result "complete multipart"
    in
    Alcotest.(check bool) "complete etag" true (Option.is_some complete.etag);
    if Client.capabilities.multipart_checksums then
      check_checksum "complete checksum" Object.Checksum.Algorithm.Sha256
        "pvs7IaTrOkof6IZmHexkX/ojbcQuYLNipUgSmw1ws7k=" complete.checksum;
    let result =
      get_object_as_string conn ~bucket ~key:(object_key "multi.bin")
        ~max_bytes:(Int64.of_int completed_size)
        ()
      |> ok_get_or_fail "get completed multipart object"
    in
    Alcotest.(check int)
      "completed body length" completed_size
      (String.length result.Get_object.value);
    Alcotest.(check char) "completed first byte" 'h' result.Get_object.value.[0];
    Alcotest.(check string)
      "completed suffix" part2_body
      (String.sub result.Get_object.value
         (String.length result.Get_object.value - String.length part2_body)
         (String.length part2_body));
    if Client.capabilities.multipart_checksums then
      check_checksum "completed object checksum"
        Object.Checksum.Algorithm.Sha256
        "pvs7IaTrOkof6IZmHexkX/ojbcQuYLNipUgSmw1ws7k=" result.checksum;
    let aborted =
      Client.Multipart.create_upload conn ~bucket ~key:(object_key "abort.bin")
        ()
      |> run_result "create abort multipart"
    in
    ignore
      (Client.Multipart.upload_part conn ~upload:aborted.upload
         ~part_number:(Multipart.Part_number.of_int_exn 1)
         ~body:(Client.Body.of_string "discarded")
         ()
      |> run_result "upload aborted part");
    ignore
      (Client.Multipart.abort_upload conn ~upload:aborted.upload ()
      |> run_result "abort multipart");
    match
      Client.run (Client.Multipart.list_parts conn ~upload:aborted.upload ())
    with
    | Error error when Error.service_code error = Some "NoSuchUpload" -> ()
    | Error error ->
        Alcotest.failf "unexpected abort list error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected aborted upload to be unavailable"

  let test_multipart_completion_edges conn =
    create_bucket conn;
    let first_body = String.make Transfer.min_part_size 'f' in
    let second_body = "second" in
    let overwritten_body = String.make Transfer.min_part_size 'F' in
    let completed_size =
      String.length overwritten_body + String.length second_body
    in
    let upload =
      Client.Multipart.create_upload conn ~bucket ~key:(object_key "edges.bin")
        ()
      |> run_result "create edge multipart"
    in
    let first =
      Client.Multipart.upload_part conn ~upload:upload.upload
        ~part_number:(Multipart.Part_number.of_int_exn 1)
        ~body:(Client.Body.of_string first_body)
        ()
      |> run_result "upload first part"
    in
    let second =
      Client.Multipart.upload_part conn ~upload:upload.upload
        ~part_number:(Multipart.Part_number.of_int_exn 2)
        ~body:(Client.Body.of_string second_body)
        ()
      |> run_result "upload second part"
    in
    let overwritten =
      Client.Multipart.upload_part conn ~upload:upload.upload
        ~part_number:(Multipart.Part_number.of_int_exn 1)
        ~body:(Client.Body.of_string overwritten_body)
        ()
      |> run_result "overwrite first part"
    in
    run_expect_status "complete with stale part etag" 400
      (Client.Multipart.complete_upload conn ~upload:upload.upload
         ~parts:[ first.part; second.part ]
         ());
    run_expect_validation "complete with unsorted parts"
      (Client.Multipart.complete_upload conn ~upload:upload.upload
         ~parts:[ second.part; overwritten.part ]
         ());
    ignore
      (Client.Multipart.complete_upload conn ~upload:upload.upload
         ~parts:[ overwritten.part; second.part ]
         ()
      |> run_result "complete overwritten parts");
    let result =
      get_object_as_string conn ~bucket ~key:(object_key "edges.bin")
        ~max_bytes:(Int64.of_int completed_size)
        ()
      |> ok_get_or_fail "get edge multipart object"
    in
    Alcotest.(check int)
      "completed overwritten body length" completed_size
      (String.length result.Get_object.value);
    Alcotest.(check char)
      "completed overwritten first byte" 'F'
      result.Get_object.value.[0];
    Alcotest.(check string)
      "completed overwritten suffix" second_body
      (String.sub result.Get_object.value
         (String.length result.Get_object.value - String.length second_body)
         (String.length second_body));
    run_expect_status "complete already completed upload" 404
      (Client.Multipart.complete_upload conn ~upload:upload.upload
         ~parts:[ overwritten.part; second.part ]
         ())

  let bucket_cases =
    [
      test_case "bucket lifecycle" `Quick test_bucket_lifecycle;
      test_case "bucket config roundtrips" `Quick test_bucket_config_roundtrips;
    ]

  let object_cases =
    [
      test_case "object in-memory lifecycle" `Quick test_object_buffer_lifecycle;
      test_case "streaming get" `Quick test_streaming_get;
      test_case "large streaming roundtrip" `Quick
        test_large_streaming_roundtrip;
      test_case "range reads and metadata copy" `Quick
        test_range_reads_and_metadata_copy;
      test_case "object tagging" `Quick test_object_tagging;
      test_case "object versioning" `Quick test_object_versioning;
      test_case "in-memory helper limit" `Quick test_buffer_limit;
      test_case "object preconditions" `Quick test_object_preconditions;
    ]

  let listing_cases =
    [
      test_case "list copy delete objects" `Quick test_list_copy_delete_objects;
    ]

  let multipart_cases =
    [
      test_case "multipart lifecycle" `Quick test_multipart_lifecycle;
      test_case "multipart completion edges" `Quick
        test_multipart_completion_edges;
    ]

  let suites =
    [
      ("contract:bucket", bucket_cases);
      ("contract:object", object_cases);
      ("contract:listing", listing_cases);
      ("contract:multipart", multipart_cases);
    ]

  let cases = List.concat_map (fun (_name, cases) -> cases) suites
end
