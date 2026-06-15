open Awskit_s3
open Awskit_s3_test

module type SUBJECT = sig
  include S with type 'a io := 'a

  val fresh : unit -> connection

  val read_response_body :
    Reader.t -> bytes -> off:int -> len:int -> (int, Error.t) result
end

module Make (Client : SUBJECT) = struct
  let bucket = "contract-bucket"

  let is_body_error error =
    let open Awskit.Error in
    match kind error with Body _ -> true | _ -> false

  let create_bucket conn =
    ignore (Client.Bucket.create conn ~bucket () |> ok_or_fail "create bucket")

  let put_object_string conn ~bucket ~key ?options value =
    Client.Object.put conn ~bucket ~key ?options
      ~body:(Client.Body.of_string value)
      ()

  let get_object_as_string conn ~bucket ~key ?options ~max_bytes () =
    Client.Object.get conn ~bucket ~key ?options
      ~consume:(Client.Reader.to_string ~max_bytes)
      ()

  let put_string conn key value =
    ignore
      (put_object_string conn ~bucket ~key value |> ok_or_fail ("put " ^ key))

  let require_version label = function
    | Some version_id -> version_id
    | None -> Alcotest.failf "%s: expected version id" label

  let version_string = Option.map Object.Version_id.to_string

  let test_bucket_lifecycle () =
    let conn = Client.fresh () in
    Alcotest.(check bool)
      "missing bucket" false
      (Client.Bucket.exists conn ~bucket () |> ok_or_fail "exists missing");
    create_bucket conn;
    let head = Client.Bucket.head conn ~bucket () |> ok_or_fail "head bucket" in
    Alcotest.(check string) "bucket name" bucket head.name;
    Alcotest.(check bool)
      "created bucket" true
      (Client.Bucket.exists conn ~bucket () |> ok_or_fail "exists present");
    let list_result = Client.Bucket.list conn |> ok_or_fail "list buckets" in
    Alcotest.(check (list string))
      "bucket list" [ bucket ]
      (List.map (fun (info : Bucket.info) -> info.name) list_result.buckets);
    let location =
      Client.Bucket.get_location conn ~bucket () |> ok_or_fail "location"
    in
    Alcotest.(check (option string))
      "location" (Some "us-east-1")
      (Option.map Region.to_string location.region);
    ignore (Client.Bucket.delete conn ~bucket () |> ok_or_fail "delete bucket");
    Alcotest.(check bool)
      "deleted bucket" false
      (Client.Bucket.exists conn ~bucket () |> ok_or_fail "exists deleted")

  let test_object_buffer_lifecycle () =
    let conn = Client.fresh () in
    create_bucket conn;
    let options =
      {
        Put_object.default_options with
        content_type = Some "text/plain";
        metadata = [ ("origin", "contract") ];
        tags = [ tag "env" "test" ];
        checksum =
          Some
            {
              Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha256;
              value = "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=";
            };
      }
    in
    let put =
      put_object_string conn ~bucket ~key:"hello.txt" ~options "hello"
      |> ok_or_fail "put object"
    in
    Alcotest.(check bool) "put etag" true (Option.is_some put.etag);
    check_checksum "put checksum" Object.Checksum.Algorithm.Sha256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
    let info, body =
      get_object_as_string conn ~bucket ~key:"hello.txt" ~max_bytes:16L ()
      |> ok_or_fail "get object"
    in
    Alcotest.(check string) "body" "hello" body;
    Alcotest.(check (option string))
      "content type" (Some "text/plain") info.content_type;
    Alcotest.(check (option string))
      "metadata" (Some "contract")
      (List.assoc_opt "origin" info.metadata);
    check_checksum "get checksum" Object.Checksum.Algorithm.Sha256
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" info.checksum;
    let head =
      Client.Object.head conn ~bucket ~key:"hello.txt" () |> ok_or_fail "head"
    in
    Alcotest.(check (option int64))
      "content length" (Some 5L) head.content_length;
    check_checksum "head checksum" Object.Checksum.Algorithm.Sha256
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
         ~body:(Client.Body.of_string "abcdef")
         ()
      |> ok_or_fail "put streaming");
    let consume reader =
      let bytes = Bytes.create 4 in
      match Client.read_response_body reader bytes ~off:0 ~len:4 with
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
         ~body:(Client.Body.of_string body)
         ()
      |> ok_or_fail "put large stream");
    let consume reader =
      let bytes = Bytes.create 16384 in
      let rec loop total first last =
        match
          Client.read_response_body reader bytes ~off:0
            ~len:(Bytes.length bytes)
        with
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
        Put_object.default_options with
        content_type = Some "text/plain";
        metadata = [ ("origin", "source"); ("mode", "copy") ];
      }
    in
    ignore
      (put_object_string conn ~bucket ~key:"range.txt" ~options:put_options
         "abcdefghij"
      |> ok_or_fail "put range source");
    let range_options =
      {
        Get_object.default_options with
        range = Some (Range.bytes_exn ~start:2L ~finish:5L);
      }
    in
    let info, body =
      get_object_as_string conn ~bucket ~key:"range.txt" ~options:range_options
        ~max_bytes:16L ()
      |> ok_or_fail "get byte range"
    in
    Alcotest.(check string) "range body" "cdef" body;
    Alcotest.(check int)
      "range status" 206
      (Awskit.Response.status info.response);
    Alcotest.(check (option int64))
      "range content length" (Some 4L) info.content_length;
    Alcotest.(check (option string))
      "range content range" (Some "bytes 2-5/10")
      (Awskit.Response.header info.response "content-range");
    let suffix_options =
      { Get_object.default_options with range = Some (Range.suffix_exn 3L) }
    in
    let _info, suffix =
      get_object_as_string conn ~bucket ~key:"range.txt" ~options:suffix_options
        ~max_bytes:16L ()
      |> ok_or_fail "get suffix range"
    in
    Alcotest.(check string) "suffix body" "hij" suffix;
    let invalid_range_options =
      { Get_object.default_options with range = Some (Range.from_exn 99L) }
    in
    expect_status "invalid range" 416
      (get_object_as_string conn ~bucket ~key:"range.txt"
         ~options:invalid_range_options ~max_bytes:16L ());
    ignore
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:"range.txt"
         ~destination_bucket:bucket ~destination_key:"copied.txt" ()
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
        Copy_object.default_options with
        metadata_directive = Some (`Replace [ ("origin", "replacement") ]);
      }
    in
    ignore
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:"range.txt"
         ~destination_bucket:bucket ~destination_key:"replaced.txt"
         ~options:replace_options ()
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

  let test_list_copy_delete_objects () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "logs/a.txt" "a";
    put_string conn "logs/b.txt" "b";
    put_string conn "other.txt" "other";
    ignore
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:"logs/a.txt"
         ~destination_bucket:bucket ~destination_key:"copy/a.txt" ()
      |> ok_or_fail "copy");
    let list_options =
      {
        List_objects_v2.default_options with
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
      { Delete_objects.key; version_id = None; etag = None }
    in
    ignore
      (Client.Object.delete_objects conn ~bucket
         ~objects:[ delete_object "logs/a.txt"; delete_object "logs/b.txt" ]
         ()
      |> ok_or_fail "delete objects");
    let keys = Client.Object.list_keys conn ~bucket () |> ok_or_fail "keys" in
    Alcotest.(check (list string))
      "remaining keys"
      [ "copy/a.txt"; "other.txt" ]
      keys

  let test_copy_validates_source_and_destination () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "source.txt" "body";
    expect_validation "copy invalid source bucket"
      (Client.Object.copy conn ~source_bucket:"BadBucket"
         ~source_key:"source.txt" ~destination_bucket:bucket
         ~destination_key:"copy.txt" ());
    expect_validation "copy invalid source key"
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:""
         ~destination_bucket:bucket ~destination_key:"copy.txt" ());
    expect_validation "copy invalid destination bucket"
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:"source.txt"
         ~destination_bucket:"BadBucket" ~destination_key:"copy.txt" ());
    expect_validation "copy invalid destination key"
      (Client.Object.copy conn ~source_bucket:bucket ~source_key:"source.txt"
         ~destination_bucket:bucket ~destination_key:"" ())

  let test_object_tagging () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "tagged.txt" "body";
    let tags = [ tag "env" "dev"; tag "owner" "sdk" ] in
    ignore
      (Client.Object.Tagging.put conn ~bucket ~key:"tagged.txt" tags
      |> ok_or_fail "put object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:"tagged.txt" ()
      |> ok_or_fail "get object tags"
    in
    Alcotest.(check int) "tag count" 2 (List.length result.tags);
    ignore
      (Client.Object.Tagging.delete conn ~bucket ~key:"tagged.txt" ()
      |> ok_or_fail "delete object tags");
    let result =
      Client.Object.Tagging.get conn ~bucket ~key:"tagged.txt" ()
      |> ok_or_fail "get deleted tags"
    in
    Alcotest.(check int) "deleted tag count" 0 (List.length result.tags)

  let test_object_versioning () =
    let conn = Client.fresh () in
    create_bucket conn;
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         Bucket.Versioning.Status.Enabled
      |> ok_or_fail "enable versioning");
    let put1 =
      put_object_string conn ~bucket ~key:"versioned.txt" "one"
      |> ok_or_fail "put version one"
    in
    let v1 = require_version "put version one" put1.version_id in
    let put2 =
      put_object_string conn ~bucket ~key:"versioned.txt" "two"
      |> ok_or_fail "put version two"
    in
    let v2 = require_version "put version two" put2.version_id in
    let info, body =
      get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L ()
      |> ok_or_fail "get current version"
    in
    Alcotest.(check string) "current body" "two" body;
    Alcotest.(check (option string))
      "current version"
      (Some (Object.Version_id.to_string v2))
      (version_string info.version_id);
    let previous_options =
      { Get_object.default_options with version_id = Some v1 }
    in
    let info, body =
      get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L
        ~options:previous_options ()
      |> ok_or_fail "get previous version"
    in
    Alcotest.(check string) "previous body" "one" body;
    Alcotest.(check (option string))
      "previous version"
      (Some (Object.Version_id.to_string v1))
      (version_string info.version_id);
    let head_options =
      { Head_object.default_options with version_id = Some v1 }
    in
    let head =
      Client.Object.head conn ~bucket ~key:"versioned.txt" ~options:head_options
        ()
      |> ok_or_fail "head previous version"
    in
    Alcotest.(check (option int64))
      "previous length" (Some 3L) head.content_length;
    Alcotest.(check (option string))
      "previous head version"
      (Some (Object.Version_id.to_string v1))
      (version_string head.version_id);
    let copy =
      Client.Object.copy conn ~source_bucket:bucket ~source_key:"versioned.txt"
        ~destination_bucket:bucket ~destination_key:"copy.txt" ()
      |> ok_or_fail "copy versioned object"
    in
    ignore (require_version "copy destination version" copy.version_id);
    Alcotest.(check (option string))
      "copy source version"
      (Some (Object.Version_id.to_string v2))
      (version_string copy.copy_source_version_id);
    let copy_previous_options =
      { Copy_object.default_options with source_version_id = Some v1 }
    in
    let copy_previous =
      Client.Object.copy conn ~source_bucket:bucket ~source_key:"versioned.txt"
        ~destination_bucket:bucket ~destination_key:"copy-previous.txt"
        ~options:copy_previous_options ()
      |> ok_or_fail "copy previous version"
    in
    Alcotest.(check (option string))
      "copy previous source version"
      (Some (Object.Version_id.to_string v1))
      (version_string copy_previous.copy_source_version_id);
    let _info, body =
      get_object_as_string conn ~bucket ~key:"copy-previous.txt" ~max_bytes:16L
        ()
      |> ok_or_fail "get copied previous"
    in
    Alcotest.(check string) "copied previous body" "one" body;
    let upload =
      Client.Multipart.create_upload conn ~bucket ~key:"multi-versioned.txt" ()
      |> ok_or_fail "create versioned multipart"
    in
    let part =
      Client.Multipart.upload_part conn ~bucket ~key:"multi-versioned.txt"
        ~upload_id:upload.upload.upload_id ~part_number:1
        ~body:(Client.Body.of_string "multipart")
        ()
      |> ok_or_fail "upload versioned part"
    in
    let complete =
      Client.Multipart.complete_upload conn ~bucket ~key:"multi-versioned.txt"
        ~upload_id:upload.upload.upload_id [ part.part ]
      |> ok_or_fail "complete versioned multipart"
    in
    ignore (require_version "complete multipart version" complete.version_id);
    let deleted =
      Client.Object.delete conn ~bucket ~key:"versioned.txt" ()
      |> ok_or_fail "delete current version"
    in
    let marker = require_version "delete marker version" deleted.version_id in
    Alcotest.(check (option bool))
      "delete marker" (Some true) deleted.delete_marker;
    Alcotest.(check bool)
      "delete marker hides current" false
      (Client.Object.exists conn ~bucket ~key:"versioned.txt"
      |> ok_or_fail "exists after delete marker");
    let marker_get_options =
      { Get_object.default_options with version_id = Some marker }
    in
    expect_status "get delete marker version" 405
      (get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L
         ~options:marker_get_options ());
    let versions_options =
      {
        List_object_versions.default_options with
        prefix = Some "versioned.txt";
        max_keys = Some 2;
      }
    in
    let first_versions_page =
      Client.Object.list_versions conn ~bucket ~options:versions_options ()
      |> ok_or_fail "list object versions page"
    in
    Alcotest.(check bool)
      "version page truncated" true first_versions_page.is_truncated;
    Alcotest.(check int)
      "version page entries" 2
      (List.length first_versions_page.versions
      + List.length first_versions_page.delete_markers);
    let all_versions =
      Client.Object.List_object_versions.object_versions conn ~bucket
        ~options:versions_options ()
      |> ok_or_fail "paginate object versions"
    in
    Alcotest.(check (list string))
      "listed object versions"
      [ Object.Version_id.to_string v2; Object.Version_id.to_string v1 ]
      (List.filter_map
         (fun (version : List_object_versions.object_version) ->
           Option.map Object.Version_id.to_string version.version_id)
         all_versions);
    let all_markers =
      Client.Object.List_object_versions.delete_markers conn ~bucket
        ~options:versions_options ()
      |> ok_or_fail "paginate delete markers"
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
    ignore
      (Client.Object.delete conn ~bucket ~key:"versioned.txt"
         ~options:missing_version_options ()
      |> ok_or_fail "delete missing version id");
    Alcotest.(check bool)
      "missing version delete keeps marker" false
      (Client.Object.exists conn ~bucket ~key:"versioned.txt"
      |> ok_or_fail "exists after missing version delete");
    let version_two_options =
      { Get_object.default_options with version_id = Some v2 }
    in
    let _info, body =
      get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L
        ~options:version_two_options ()
      |> ok_or_fail "get hidden version"
    in
    Alcotest.(check string) "hidden body" "two" body;
    let delete_marker_options =
      { Delete_object.default_options with version_id = Some marker }
    in
    ignore
      (Client.Object.delete conn ~bucket ~key:"versioned.txt"
         ~options:delete_marker_options ()
      |> ok_or_fail "delete delete marker");
    let _info, body =
      get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L ()
      |> ok_or_fail "get restored current"
    in
    Alcotest.(check string) "restored current" "two" body;
    let delete_v2_options =
      { Delete_object.default_options with version_id = Some v2 }
    in
    ignore
      (Client.Object.delete conn ~bucket ~key:"versioned.txt"
         ~options:delete_v2_options ()
      |> ok_or_fail "delete version two");
    let _info, body =
      get_object_as_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L ()
      |> ok_or_fail "get remaining version"
    in
    Alcotest.(check string) "remaining current" "one" body;
    let wrong_delete =
      {
        Delete_objects.key = "versioned.txt";
        version_id = Some v1;
        etag = Some (Object.Etag.of_string_exn "\"wrong\"");
      }
    in
    let failed_many =
      Client.Object.delete_objects conn ~bucket ~objects:[ wrong_delete ] ()
      |> ok_or_fail "delete objects wrong etag"
    in
    Alcotest.(check int)
      "delete objects precondition errors" 1
      (List.length failed_many.errors);
    Alcotest.(check int)
      "delete objects failed deleted" 0
      (List.length failed_many.deleted);
    let correct_delete = { wrong_delete with etag = None } in
    let deleted_many =
      Client.Object.delete_objects conn ~bucket ~objects:[ correct_delete ] ()
      |> ok_or_fail "delete objects version"
    in
    Alcotest.(check int)
      "delete objects deleted version" 1
      (List.length deleted_many.deleted);
    Alcotest.(check bool)
      "all versions removed" false
      (Client.Object.exists conn ~bucket ~key:"versioned.txt"
      |> ok_or_fail "exists after delete objects version");
    let enabled_put =
      put_object_string conn ~bucket ~key:"suspended.txt" "enabled"
      |> ok_or_fail "put before suspend"
    in
    let enabled_version =
      require_version "put before suspend" enabled_put.version_id
    in
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         Bucket.Versioning.Status.Suspended
      |> ok_or_fail "suspend versioning");
    let suspended_put =
      put_object_string conn ~bucket ~key:"suspended.txt" "suspended"
      |> ok_or_fail "put while suspended"
    in
    Alcotest.(check (option string))
      "suspended put version" (Some "null")
      (version_string suspended_put.version_id);
    let _info, body =
      get_object_as_string conn ~bucket ~key:"suspended.txt" ~max_bytes:16L ()
      |> ok_or_fail "get suspended current"
    in
    Alcotest.(check string) "suspended current body" "suspended" body;
    let enabled_options =
      { Get_object.default_options with version_id = Some enabled_version }
    in
    let _info, body =
      get_object_as_string conn ~bucket ~key:"suspended.txt" ~max_bytes:16L
        ~options:enabled_options ()
      |> ok_or_fail "get enabled version after suspend"
    in
    Alcotest.(check string) "enabled version body" "enabled" body;
    let suspended_versions =
      Client.Object.List_object_versions.object_versions conn ~bucket
        ~options:
          {
            List_object_versions.default_options with
            prefix = Some "suspended.txt";
          }
        ()
      |> ok_or_fail "list suspended versions"
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
      Client.Object.delete conn ~bucket ~key:"suspended.txt" ()
      |> ok_or_fail "delete while suspended"
    in
    Alcotest.(check (option bool))
      "suspended delete marker" (Some true) suspended_delete.delete_marker;
    Alcotest.(check (option string))
      "suspended delete marker version" (Some "null")
      (version_string suspended_delete.version_id);
    Alcotest.(check bool)
      "suspended marker hides current" false
      (Client.Object.exists conn ~bucket ~key:"suspended.txt"
      |> ok_or_fail "exists after suspended delete");
    let null_version = Object.Version_id.of_string_exn "null" in
    let null_marker_options =
      { Delete_object.default_options with version_id = Some null_version }
    in
    ignore
      (Client.Object.delete conn ~bucket ~key:"suspended.txt"
         ~options:null_marker_options ()
      |> ok_or_fail "delete suspended marker");
    let _info, body =
      get_object_as_string conn ~bucket ~key:"suspended.txt" ~max_bytes:16L ()
      |> ok_or_fail "get restored enabled version"
    in
    Alcotest.(check string) "restored enabled body" "enabled" body

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
    let policy =
      Client.Bucket.Policy.get conn ~bucket () |> ok_or_fail "policy"
    in
    Alcotest.(check string) "policy" sample_policy_json (Policy.to_json policy);
    ignore
      (Client.Bucket.Policy.delete conn ~bucket () |> ok_or_fail "delete policy");
    expect_not_found "deleted policy" (Client.Bucket.Policy.get conn ~bucket ());
    ignore
      (Client.Bucket.Versioning.put conn ~bucket
         Bucket.Versioning.Status.Enabled
      |> ok_or_fail "put versioning");
    let versioning =
      Client.Bucket.Versioning.get conn ~bucket ()
      |> ok_or_fail "get versioning"
    in
    Alcotest.(check bool)
      "versioning enabled" true
      (versioning.status = Some Bucket.Versioning.Status.Enabled);
    let bucket_tags = [ tag "team" "storage" ] in
    ignore
      (Client.Bucket.Tagging.put conn ~bucket bucket_tags
      |> ok_or_fail "put bucket tags");
    let result =
      Client.Bucket.Tagging.get conn ~bucket () |> ok_or_fail "get bucket tags"
    in
    Alcotest.(check int) "bucket tag count" 1 (List.length result.tags);
    ignore
      (Client.Bucket.Tagging.delete conn ~bucket ()
      |> ok_or_fail "delete bucket tags");
    let result =
      Client.Bucket.Tagging.get conn ~bucket () |> ok_or_fail "get deleted tags"
    in
    Alcotest.(check int) "deleted bucket tag count" 0 (List.length result.tags);
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
      (Client.Bucket.Encryption.put conn ~bucket encryption
      |> ok_or_fail "put encryption");
    let result =
      Client.Bucket.Encryption.get conn ~bucket ()
      |> ok_or_fail "get encryption"
    in
    Alcotest.(check int)
      "encryption rule count" 1
      (List.length result.config.rules);
    ignore
      (Client.Bucket.Encryption.delete conn ~bucket ()
      |> ok_or_fail "delete encryption");
    expect_not_found "deleted encryption"
      (Client.Bucket.Encryption.get conn ~bucket ());
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
    let result =
      Client.Bucket.Cors.get conn ~bucket () |> ok_or_fail "get cors"
    in
    Alcotest.(check int) "cors rule count" 1 (List.length result.config.rules);
    ignore
      (Client.Bucket.Cors.delete conn ~bucket () |> ok_or_fail "delete cors");
    expect_not_found "deleted cors" (Client.Bucket.Cors.get conn ~bucket ());
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
      Client.Bucket.Public_access_block.get conn ~bucket ()
      |> ok_or_fail "get public access block"
    in
    Alcotest.(check bool)
      "block public acls" true result.config.block_public_acls;
    ignore
      (Client.Bucket.Public_access_block.delete conn ~bucket ()
      |> ok_or_fail "delete public access block");
    expect_not_found "deleted public access block"
      (Client.Bucket.Public_access_block.get conn ~bucket ());
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
      Client.Bucket.Ownership_controls.get conn ~bucket ()
      |> ok_or_fail "get ownership"
    in
    Alcotest.(check string)
      "ownership" "BucketOwnerEnforced"
      (Bucket.Ownership_controls.Object_ownership.to_string
         result.config.object_ownership);
    ignore
      (Client.Bucket.Ownership_controls.delete conn ~bucket ()
      |> ok_or_fail "delete ownership");
    expect_not_found "deleted ownership"
      (Client.Bucket.Ownership_controls.get conn ~bucket ())

  let test_buffer_limit () =
    let conn = Client.fresh () in
    create_bucket conn;
    put_string conn "large.txt" "abcdef";
    match
      get_object_as_string conn ~bucket ~key:"large.txt" ~max_bytes:3L ()
    with
    | Error error when is_body_error error -> ()
    | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
    | Ok _ -> Alcotest.fail "expected max_bytes failure"

  let test_object_preconditions () =
    let conn = Client.fresh () in
    create_bucket conn;
    let put =
      put_object_string conn ~bucket ~key:"conditional.txt" "body"
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
      (put_object_string conn ~bucket ~key:"conditional.txt"
         ~options:absent_options "new-body");
    let match_options =
      {
        Put_object.default_options with
        preconditions = Object.Preconditions.Write.if_etag bad_etag;
      }
    in
    expect_precondition_failed "put if match bad etag"
      (put_object_string conn ~bucket ~key:"conditional.txt"
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
      (get_object_as_string conn ~bucket ~key:"conditional.txt"
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
      (get_object_as_string conn ~bucket ~key:"conditional.txt"
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
    expect_not_modified "head if modified since same time"
      (Client.Object.head conn ~bucket ~key:"conditional.txt"
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
    expect_precondition_failed "head if unmodified since stale"
      (Client.Object.head conn ~bucket ~key:"conditional.txt"
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
         ~source_key:"conditional.txt" ~destination_bucket:bucket
         ~destination_key:"conditional-copy.txt" ~options:copy_options ()
      |> ok_or_fail "copy if match etag");
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
    expect_precondition_failed "copy if none match etag"
      (Client.Object.copy conn ~source_bucket:bucket
         ~source_key:"conditional.txt" ~destination_bucket:bucket
         ~destination_key:"conditional-copy-fail.txt" ~options:copy_fail_options
         ());
    let delete_key = "delete-conditional.txt" in
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
      |> ok_or_fail "delete preconditions");
    let delete_fail_key = "delete-conditional-fail.txt" in
    put_string conn delete_fail_key "abc";
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
    expect_precondition_failed "delete if match mismatch"
      (Client.Object.delete conn ~bucket ~key:delete_fail_key
         ~options:delete_fail_options ());
    expect_precondition_failed "delete missing with precondition"
      (Client.Object.delete conn ~bucket ~key:"missing-delete-conditional.txt"
         ~options:delete_options ())

  let test_multipart_lifecycle () =
    let conn = Client.fresh () in
    create_bucket conn;
    let upload_options =
      {
        Create_multipart_upload.default_options with
        checksum_algorithm = Some Object.Checksum.Algorithm.Sha256;
      }
    in
    let upload =
      Client.Multipart.create_upload conn ~bucket ~key:"multi.bin"
        ~options:upload_options ()
      |> ok_or_fail "create multipart"
    in
    let upload_id = upload.upload.upload_id in
    let part_options =
      {
        Upload_part.default_options with
        checksum =
          Some
            {
              Object.Checksum.algorithm = Object.Checksum.Algorithm.Sha1;
              value = "provided-sha1";
            };
      }
    in
    let part1 =
      Client.Multipart.upload_part conn ~bucket ~key:"multi.bin" ~upload_id
        ~part_number:1
        ~body:(Client.Body.of_string "hello-")
        ~options:part_options ()
      |> ok_or_fail "upload part 1"
    in
    let part2 =
      Client.Multipart.upload_part conn ~bucket ~key:"multi.bin" ~upload_id
        ~part_number:2
        ~body:(Client.Body.of_string "world")
        ~options:part_options ()
      |> ok_or_fail "upload part 2"
    in
    let list_options = { List_parts.default_options with max_parts = Some 1 } in
    let page =
      Client.Multipart.list_parts conn ~bucket ~key:"multi.bin" ~upload_id
        ~options:list_options ()
      |> ok_or_fail "list multipart parts"
    in
    Alcotest.(check int) "first page count" 1 (List.length page.parts);
    Alcotest.(check bool) "parts truncated" true page.is_truncated;
    (match page.parts with
    | [ part ] ->
        check_checksum "listed part checksum" Object.Checksum.Algorithm.Sha1
          "provided-sha1" part.checksum
    | _ -> Alcotest.fail "expected first multipart page");
    let parts =
      Client.Multipart.List_parts.parts conn ~bucket ~key:"multi.bin" ~upload_id
        ~options:list_options ()
      |> ok_or_fail "paginate multipart parts"
    in
    Alcotest.(check (list int))
      "part numbers" [ 1; 2 ]
      (List.map (fun (part : List_parts.part_info) -> part.part_number) parts);
    let complete =
      Client.Multipart.complete_upload conn ~bucket ~key:"multi.bin" ~upload_id
        [ part1.part; part2.part ]
      |> ok_or_fail "complete multipart"
    in
    Alcotest.(check bool) "complete etag" true (Option.is_some complete.etag);
    check_checksum "complete checksum" Object.Checksum.Algorithm.Sha256
      "r6J7RNQ7Aqn+pB0TztwuQBbPz4fF2/mQ5ZNmmqjOKG0=" complete.checksum;
    let info, body =
      get_object_as_string conn ~bucket ~key:"multi.bin" ~max_bytes:16L ()
      |> ok_or_fail "get completed multipart object"
    in
    Alcotest.(check string) "completed body" "hello-world" body;
    check_checksum "completed object checksum" Object.Checksum.Algorithm.Sha256
      "r6J7RNQ7Aqn+pB0TztwuQBbPz4fF2/mQ5ZNmmqjOKG0=" info.checksum;
    let aborted =
      Client.Multipart.create_upload conn ~bucket ~key:"abort.bin" ()
      |> ok_or_fail "create abort multipart"
    in
    let aborted_upload_id = aborted.upload.upload_id in
    ignore
      (Client.Multipart.upload_part conn ~bucket ~key:"abort.bin"
         ~upload_id:aborted_upload_id ~part_number:1
         ~body:(Client.Body.of_string "discarded")
         ()
      |> ok_or_fail "upload aborted part");
    ignore
      (Client.Multipart.abort_upload conn ~bucket ~key:"abort.bin"
         ~upload_id:aborted_upload_id ()
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
      Client.Multipart.create_upload conn ~bucket ~key:"edges.bin" ()
      |> ok_or_fail "create edge multipart"
    in
    let upload_id = upload.upload.upload_id in
    let first =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:1
        ~body:(Client.Body.of_string "first")
        ()
      |> ok_or_fail "upload first part"
    in
    let second =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:2
        ~body:(Client.Body.of_string "second")
        ()
      |> ok_or_fail "upload second part"
    in
    let overwritten =
      Client.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
        ~part_number:1
        ~body:(Client.Body.of_string "FIRST")
        ()
      |> ok_or_fail "overwrite first part"
    in
    expect_status "complete with stale part etag" 400
      (Client.Multipart.complete_upload conn ~bucket ~key:"edges.bin" ~upload_id
         [ first.part; second.part ]);
    expect_validation "complete with unsorted parts"
      (Client.Multipart.complete_upload conn ~bucket ~key:"edges.bin" ~upload_id
         [ second.part; overwritten.part ]);
    ignore
      (Client.Multipart.complete_upload conn ~bucket ~key:"edges.bin" ~upload_id
         [ overwritten.part; second.part ]
      |> ok_or_fail "complete overwritten parts");
    let _info, body =
      get_object_as_string conn ~bucket ~key:"edges.bin" ~max_bytes:16L ()
      |> ok_or_fail "get edge multipart object"
    in
    Alcotest.(check string) "completed overwritten body" "FIRSTsecond" body;
    expect_status "complete already completed upload" 404
      (Client.Multipart.complete_upload conn ~bucket ~key:"edges.bin" ~upload_id
         [ overwritten.part; second.part ])

  let cases =
    [
      Alcotest.test_case "bucket lifecycle" `Quick test_bucket_lifecycle;
      Alcotest.test_case "object in-memory lifecycle" `Quick
        test_object_buffer_lifecycle;
      Alcotest.test_case "streaming get" `Quick test_streaming_get;
      Alcotest.test_case "large streaming roundtrip" `Quick
        test_large_streaming_roundtrip;
      Alcotest.test_case "range reads and metadata copy" `Quick
        test_range_reads_and_metadata_copy;
      Alcotest.test_case "list copy delete objects" `Quick
        test_list_copy_delete_objects;
      Alcotest.test_case "copy validates source and destination" `Quick
        test_copy_validates_source_and_destination;
      Alcotest.test_case "object tagging" `Quick test_object_tagging;
      Alcotest.test_case "object versioning" `Quick test_object_versioning;
      Alcotest.test_case "bucket config roundtrips" `Quick
        test_bucket_config_roundtrips;
      Alcotest.test_case "in-memory helper limit" `Quick test_buffer_limit;
      Alcotest.test_case "object preconditions" `Quick test_object_preconditions;
      Alcotest.test_case "multipart lifecycle" `Quick test_multipart_lifecycle;
      Alcotest.test_case "multipart completion edges" `Quick
        test_multipart_completion_edges;
    ]
end
