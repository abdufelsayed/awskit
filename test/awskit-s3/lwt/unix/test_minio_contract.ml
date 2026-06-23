module S3 = Awskit_s3_lwt_unix
module Bucket = Awskit_s3.Bucket
module Object = Awskit_s3.Object
module Multipart = Awskit_s3.Multipart
module Put_object = Awskit_s3.Put_object
module Get_object = Awskit_s3.Get_object
module Delete_object = Awskit_s3.Delete_object
module Delete_objects = Awskit_s3.Delete_objects
module Copy_object = Awskit_s3.Copy_object
module List_object_versions = Awskit_s3.List_object_versions
module Object_key = Awskit_s3.Object_key
module Range = Awskit_s3.Range
module Transfer = Awskit_s3.Transfer

let content_type value = Awskit_s3.Content_type.of_string_exn value
let tag_set tags = Awskit_s3.Tag.Set.of_list_exn tags
let bucket_of_string value = Awskit_s3.Bucket_name.of_string_exn value
let object_key value = Awskit_s3.Object_key.of_string_exn value

let getenv_default name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

let endpoint = getenv_default "AWSKIT_S3_MINIO_ENDPOINT" "http://127.0.0.1:9000"
let unsafe_http = getenv_default "AWSKIT_S3_MINIO_UNSAFE_HTTP" ""
let access_key = getenv_default "AWSKIT_S3_MINIO_ACCESS_KEY_ID" "minioadmin"
let secret_key = getenv_default "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY" "minioadmin"
let region = getenv_default "AWSKIT_S3_MINIO_REGION" "us-east-1"

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:access_key
    ~secret_access_key:secret_key ()

let endpoint_config () =
  let endpoint =
    Awskit.Endpoint.of_string endpoint |> function
    | Ok endpoint -> endpoint
    | Error error ->
        Alcotest.failf "minio endpoint: %a" Awskit_s3.Error.pp error
  in
  let ok_config = function
    | Ok config -> config
    | Error error ->
        Alcotest.failf "minio endpoint policy: %a" Awskit_s3.Error.pp error
  in
  let signing_region = Awskit.Region.of_string_exn region in
  match Awskit.Endpoint.scheme endpoint with
  | `Https ->
      Awskit_s3.Endpoint_config.s3_compatible ~endpoint ~signing_region
        ~addressing_style:`Path ~tls_policy:`Https_required
        ~feature_policy:`S3_compatible ()
      |> ok_config
  | `Http when unsafe_http = "1" ->
      Awskit_s3.Endpoint_config.unsafe_plaintext ~endpoint ~signing_region
        ~addressing_style:`Path ()
  | `Http -> (
      match
        Awskit_s3.Endpoint_config.local_plaintext ~endpoint ~signing_region
          ~addressing_style:`Path ()
      with
      | Ok config -> config
      | Error _ ->
          Alcotest.fail
            "non-local HTTP MinIO endpoint requires \
             AWSKIT_S3_MINIO_UNSAFE_HTTP=1")

let connect () =
  match
    S3.create ~endpoint_config:(endpoint_config ()) ~region ~credentials
      ~clock:Ptime_clock.now ()
  with
  | Ok conn -> conn
  | Error error -> Alcotest.failf "connect: %a" Awskit_s3.Error.pp error

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let await label promise = Lwt_main.run promise |> ok_or_fail label
let await_get label promise = await label promise

let expect_status label status result =
  match result with
  | Error error when Awskit.Error.service_status error = Some status -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit_s3.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected service status %d" label status

let bucket_name_string suffix =
  Printf.sprintf "awskit-minio-%d-%s" (Unix.getpid ()) suffix

let delete_object_key key = Delete_objects.object_ ~key ()
let delete_object key = delete_object_key (object_key key)

let delete_object_version key version_id =
  match version_id with
  | Some version_id -> Delete_objects.object_ ~key ~version_id ()
  | None -> delete_object_key key

let cleanup_bucket conn ~bucket =
  let open Lwt.Syntax in
  let* versions_result =
    S3.Object.Versions.pages conn ~bucket ~max_pages:100 ()
  in
  (match versions_result with
    | Ok pages ->
        let objects =
          List.concat_map
            (fun (page : List_object_versions.page) ->
              let versions =
                List.map
                  (fun (version : List_object_versions.object_version) ->
                    delete_object_version version.key version.version_id)
                  page.versions
              in
              let markers =
                List.map
                  (fun (marker : List_object_versions.delete_marker) ->
                    delete_object_version marker.key marker.version_id)
                  page.delete_markers
              in
              versions @ markers)
            pages
        in
        if objects = [] then Lwt.return_unit
        else
          let* _ = S3.Object.delete_objects conn ~bucket ~objects () in
          Lwt.return_unit
    | Error _ -> (
        let* keys_result = S3.Object.List.keys conn ~bucket ~max_pages:100 () in
        match keys_result with
        | Ok [] -> Lwt.return_unit
        | Ok keys ->
            let objects = List.map delete_object_key keys in
            let* _ = S3.Object.delete_objects conn ~bucket ~objects () in
            Lwt.return_unit
        | Error _ -> Lwt.return_unit))
  |> fun deleted ->
  let* () = deleted in
  let* _ = S3.Bucket.delete conn ~bucket () in
  Lwt.return_unit

let write_file path body =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel body)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let put_string conn ~bucket ~key ?options body =
  S3.Object.put conn ~bucket ~key:(object_key key) ?options
    ~body:(S3.Body.of_string body) ()

let get_string conn ~bucket ~key ?options ~max_bytes () =
  S3.Object.get conn ~bucket ~key:(object_key key) ?options
    ~consume:(S3.Reader.to_string ~max_bytes)
    ()

let remove_file path = try Sys.remove path with Sys_error _ -> ()
let first = function [] -> None | value :: _ -> Some value

let require_version label = function
  | Some version_id -> version_id
  | None -> Alcotest.failf "%s: expected version id" label

let version_string = Option.map Object.Version_id.to_string

let with_bucket suffix f =
  let conn = connect () in
  let bucket = bucket_of_string (bucket_name_string suffix) in
  Lwt_main.run (cleanup_bucket conn ~bucket);
  ignore (await "create bucket" (S3.Bucket.create conn ~bucket ()));
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (cleanup_bucket conn ~bucket))
    (fun () -> f conn ~bucket)

let test_object_range_metadata_and_copy () =
  with_bucket "objects" (fun conn ~bucket ->
      let put_options =
        {
          Put_object.default_options with
          content_type = Some (content_type "text/plain");
          metadata =
            Awskit_s3.Metadata.of_list_exn
              [ ("origin", "minio"); ("mode", "copy") ];
        }
      in
      ignore
        (await "put object"
           (put_string conn ~bucket ~key:"range.txt" ~options:put_options
              "abcdefghij"));
      let range_options =
        {
          Get_object.default_options with
          range = Some (Range.bytes_exn ~start:2L ~finish:5L);
        }
      in
      let result =
        await_get "get range"
          (get_string conn ~bucket ~key:"range.txt" ~options:range_options
             ~max_bytes:16L ())
      in
      Alcotest.(check string) "range body" "cdef" result.Get_object.value;
      Alcotest.(check int)
        "range status" 206
        (Awskit.Response.status result.response);
      Alcotest.(check (option string))
        "content-range" (Some "bytes 2-5/10")
        (Awskit.Response.header result.response "content-range");
      let suffix_options =
        { Get_object.default_options with range = Some (Range.suffix_exn 3L) }
      in
      let result =
        await_get "get suffix"
          (get_string conn ~bucket ~key:"range.txt" ~options:suffix_options
             ~max_bytes:16L ())
      in
      Alcotest.(check string) "suffix body" "hij" result.Get_object.value;
      let invalid_range_options =
        { Get_object.default_options with range = Some (Range.from_exn 99L) }
      in
      expect_status "invalid range" 416
        (Lwt_main.run
           (get_string conn ~bucket ~key:"range.txt"
              ~options:invalid_range_options ~max_bytes:16L ()));
      ignore
        (await "copy object"
           (S3.Object.copy conn ~source_bucket:bucket
              ~source_key:(object_key "range.txt") ~destination_bucket:bucket
              ~destination_key:(object_key "copied.txt") ()));
      let copied =
        await "head copied"
          (S3.Object.head conn ~bucket ~key:(object_key "copied.txt") ())
      in
      Alcotest.(check (option string))
        "copied metadata" (Some "minio")
        (List.assoc_opt "origin" (Awskit_s3.Metadata.to_list copied.metadata));
      let replace_options =
        {
          Copy_object.default_options with
          metadata_directive =
            Some
              (`Replace
                 (Awskit_s3.Metadata.of_list_exn [ ("origin", "replacement") ]));
        }
      in
      ignore
        (await "copy replace"
           (S3.Object.copy conn ~source_bucket:bucket
              ~source_key:(object_key "range.txt") ~destination_bucket:bucket
              ~destination_key:(object_key "replaced.txt")
              ~options:replace_options ()));
      let replaced =
        await "head replaced"
          (S3.Object.head conn ~bucket ~key:(object_key "replaced.txt") ())
      in
      Alcotest.(check (option string))
        "replaced metadata" (Some "replacement")
        (List.assoc_opt "origin" (Awskit_s3.Metadata.to_list replaced.metadata));
      Alcotest.(check (option string))
        "old metadata removed" None
        (List.assoc_opt "mode" (Awskit_s3.Metadata.to_list replaced.metadata)))

let test_object_versioning () =
  with_bucket "versioning" (fun conn ~bucket ->
      ignore
        (await "enable versioning"
           (S3.Bucket.Versioning.put conn ~bucket
              ~status:Awskit_s3.Bucket.Versioning.Status.Enabled ()));
      let put1 =
        await "put version one"
          (put_string conn ~bucket ~key:"versioned.txt" "one")
      in
      let v1 = require_version "put version one" put1.version_id in
      let put2 =
        await "put version two"
          (put_string conn ~bucket ~key:"versioned.txt" "two")
      in
      let v2 = require_version "put version two" put2.version_id in
      let previous_options =
        { Get_object.default_options with version_id = Some v1 }
      in
      let result =
        await_get "get previous version"
          (get_string conn ~bucket ~key:"versioned.txt"
             ~options:previous_options ~max_bytes:16L ())
      in
      Alcotest.(check string)
        "previous version body" "one" result.Get_object.value;
      let copy_previous_options =
        { Copy_object.default_options with source_version_id = Some v1 }
      in
      let copied =
        await "copy previous version"
          (S3.Object.copy conn ~source_bucket:bucket
             ~source_key:(object_key "versioned.txt")
             ~destination_bucket:bucket
             ~destination_key:(object_key "copy-previous.txt")
             ~options:copy_previous_options ())
      in
      Alcotest.(check (option string))
        "copy source version"
        (Some (Object.Version_id.to_string v1))
        (version_string copied.copy_source_version_id);
      let result =
        await_get "get copied previous"
          (get_string conn ~bucket ~key:"copy-previous.txt" ~max_bytes:16L ())
      in
      Alcotest.(check string)
        "copied previous body" "one" result.Get_object.value;
      let deleted =
        await "delete current"
          (S3.Object.delete conn ~bucket ~key:(object_key "versioned.txt") ())
      in
      let marker = require_version "delete marker" deleted.version_id in
      Alcotest.(check (option bool))
        "delete marker" (Some true) deleted.delete_marker;
      Alcotest.(check bool)
        "delete marker hides current" false
        (await "exists after delete marker"
           (S3.Object.exists conn ~bucket ~key:(object_key "versioned.txt")));
      let list_options =
        List_object_versions.options_exn
          ~prefix:(Object_key.Prefix.of_string_exn "versioned.txt")
          ~max_keys:1 ()
      in
      let versions =
        await "list versions"
          (S3.Object.Versions.object_versions conn ~bucket ~options:list_options
             ~max_pages:10 ())
      in
      let listed_versions =
        List.filter_map
          (fun (version : List_object_versions.object_version) ->
            Option.map Object.Version_id.to_string version.version_id)
          versions
      in
      Alcotest.(check bool)
        "listed v1" true
        (List.mem (Object.Version_id.to_string v1) listed_versions);
      Alcotest.(check bool)
        "listed v2" true
        (List.mem (Object.Version_id.to_string v2) listed_versions);
      let markers =
        await "list delete markers"
          (S3.Object.Versions.delete_markers conn ~bucket ~options:list_options
             ~max_pages:10 ())
      in
      let listed_markers =
        List.filter_map
          (fun (marker : List_object_versions.delete_marker) ->
            Option.map Object.Version_id.to_string marker.version_id)
          markers
      in
      Alcotest.(check bool)
        "listed delete marker" true
        (List.mem (Object.Version_id.to_string marker) listed_markers);
      let version_two_options =
        { Get_object.default_options with version_id = Some v2 }
      in
      let result =
        await_get "get hidden current"
          (get_string conn ~bucket ~key:"versioned.txt"
             ~options:version_two_options ~max_bytes:16L ())
      in
      Alcotest.(check string)
        "hidden version body" "two" result.Get_object.value;
      ignore
        (await "delete marker version"
           (S3.Object.delete conn ~bucket
              ~key:(object_key "versioned.txt")
              ~options:
                { Delete_object.default_options with version_id = Some marker }
              ()));
      let result =
        await_get "get restored current"
          (get_string conn ~bucket ~key:"versioned.txt" ~max_bytes:16L ())
      in
      Alcotest.(check string)
        "restored current body" "two" result.Get_object.value)

let test_bucket_config_roundtrip () =
  with_bucket "bucket-config" (fun conn ~bucket ->
      ignore
        (await "enable versioning"
           (S3.Bucket.Versioning.put conn ~bucket
              ~status:Bucket.Versioning.Status.Enabled ()));
      let versioning =
        await "get versioning" (S3.Bucket.Versioning.get conn ~bucket ())
      in
      Alcotest.(check bool)
        "versioning enabled" true
        (versioning.status = Some Bucket.Versioning.Status.Enabled);
      let tags =
        tag_set
          [
            Awskit_s3.Tag.create_exn ~key:"env" ~value:"test";
            Awskit_s3.Tag.create_exn ~key:"suite" ~value:"minio";
          ]
      in
      ignore
        (await "put bucket tags" (S3.Bucket.Tagging.put conn ~bucket ~tags ()));
      let tagging =
        await "get bucket tags" (S3.Bucket.Tagging.get conn ~bucket ())
      in
      Alcotest.(check (list (pair string string)))
        "bucket tags"
        [ ("env", "test"); ("suite", "minio") ]
        (Awskit_s3.Tag.Set.to_list tagging.tags
        |> List.map (fun tag ->
            (Awskit_s3.Tag.key tag, Awskit_s3.Tag.value tag))
        |> List.sort compare);
      ignore
        (await "delete bucket tags" (S3.Bucket.Tagging.delete conn ~bucket ())))

let test_multipart_edges () =
  with_bucket "multipart" (fun conn ~bucket ->
      let first_body = String.make Transfer.min_part_size 'a' in
      let overwritten_body = String.make Transfer.min_part_size 'b' in
      let final_body = "second" in
      let upload =
        await "create multipart"
          (S3.Multipart.create_upload conn ~bucket ~key:(object_key "edges.bin")
             ())
      in
      let upload_id = upload.upload.upload_id in
      let first =
        await "upload first"
          (S3.Multipart.upload_part conn ~bucket ~key:(object_key "edges.bin")
             ~upload_id ~part_number:1
             ~body:(S3.Body.of_string first_body)
             ())
      in
      let second =
        await "upload second"
          (S3.Multipart.upload_part conn ~bucket ~key:(object_key "edges.bin")
             ~upload_id ~part_number:2
             ~body:(S3.Body.of_string final_body)
             ())
      in
      let overwritten =
        await "overwrite first"
          (S3.Multipart.upload_part conn ~bucket ~key:(object_key "edges.bin")
             ~upload_id ~part_number:1
             ~body:(S3.Body.of_string overwritten_body)
             ())
      in
      expect_status "complete stale etag" 400
        (Lwt_main.run
           (S3.Multipart.complete_upload conn ~bucket
              ~key:(object_key "edges.bin") ~upload_id
              [ first.part; second.part ]));
      ignore
        (await "complete overwritten"
           (S3.Multipart.complete_upload conn ~bucket
              ~key:(object_key "edges.bin") ~upload_id
              [ overwritten.part; second.part ]));
      let get_result =
        await_get "get multipart"
          (get_string conn ~bucket ~key:"edges.bin"
             ~max_bytes:
               (Int64.of_int
                  (String.length overwritten_body + String.length final_body))
             ())
      in
      let body = get_result.Get_object.value in
      Alcotest.(check int)
        "multipart body length"
        (String.length overwritten_body + String.length final_body)
        (String.length body);
      Alcotest.(check char) "multipart first byte" 'b' body.[0];
      Alcotest.(check string)
        "multipart suffix" final_body
        (String.sub body
           (String.length body - String.length final_body)
           (String.length final_body));
      expect_status "complete missing upload" 404
        (Lwt_main.run
           (S3.Multipart.complete_upload conn ~bucket
              ~key:(object_key "edges.bin") ~upload_id
              [ overwritten.part; second.part ])))

let test_path_transfer_streams () =
  with_bucket "transfer" (fun conn ~bucket ->
      let upload_path = Filename.temp_file "awskit-upload" ".bin" in
      let download_path = Filename.temp_file "awskit-download" ".bin" in
      let body =
        String.init
          ((128 * 1024 * 2) + 17)
          (fun index -> Char.chr ((index mod 26) + Char.code 'a'))
      in
      write_file upload_path body;
      remove_file download_path;
      let upload_progress = ref [] in
      let download_progress = ref [] in
      Fun.protect
        ~finally:(fun () ->
          remove_file upload_path;
          remove_file download_path)
        (fun () ->
          ignore
            (await "upload path"
               (S3.Object.Transfer.upload_file conn ~bucket
                  ~key:(object_key "transfer.bin")
                  ~path:upload_path
                  ~on_progress:(fun transferred ->
                    upload_progress := transferred :: !upload_progress)
                  ()));
          ignore
            (await "download path"
               (S3.Object.Transfer.download_file conn ~bucket
                  ~key:(object_key "transfer.bin")
                  ~path:download_path
                  ~on_progress:(fun transferred ->
                    download_progress := transferred :: !download_progress)
                  ()));
          Alcotest.(check string)
            "downloaded file body" body (read_file download_path);
          Alcotest.(check (option int64))
            "upload final progress"
            (Some (Int64.of_int (String.length body)))
            (first !upload_progress);
          Alcotest.(check (option int64))
            "download final progress"
            (Some (Int64.of_int (String.length body)))
            (first !download_progress)))

let test_multipart_path_transfer_resumes () =
  with_bucket "transfer-multipart" (fun conn ~bucket ->
      let part_size = Transfer.min_part_size in
      let path = Filename.temp_file "awskit-multipart-upload" ".bin" in
      let body =
        String.init
          ((part_size * 2) + 17)
          (fun index -> Char.chr ((index mod 10) + Char.code '0'))
      in
      write_file path body;
      let options =
        { Transfer.default_upload_options with part_size; concurrency = 2 }
      in
      let progress = ref [] in
      Fun.protect
        ~finally:(fun () -> remove_file path)
        (fun () ->
          let created =
            await "create multipart for resume"
              (S3.Multipart.create_upload conn ~bucket
                 ~key:(object_key "resume.bin") ())
          in
          let upload_id = created.upload.upload_id in
          let first_part = String.sub body 0 part_size in
          ignore
            (await "upload resume seed"
               (S3.Multipart.upload_part conn ~bucket
                  ~key:(object_key "resume.bin") ~upload_id ~part_number:1
                  ~body:(S3.Body.of_string first_part)
                  ()));
          let result =
            await "resume multipart path"
              (S3.Object.Transfer.resume_multipart_upload_file conn ~bucket
                 ~key:(object_key "resume.bin") ~upload_id ~options ~path
                 ~on_progress:(fun transferred ->
                   progress := transferred :: !progress)
                 ())
          in
          Alcotest.(check int) "completed parts" 3 (List.length result.parts);
          let get_result =
            await_get "get resumed multipart"
              (get_string conn ~bucket ~key:"resume.bin"
                 ~max_bytes:(Int64.of_int (String.length body))
                 ())
          in
          Alcotest.(check string)
            "resumed body" body get_result.Get_object.value;
          Alcotest.(check (option int64))
            "resume initial progress includes first part"
            (Some (Int64.of_int (String.length body)))
            (first !progress)))

let suite () =
  [
    ( "minio contract",
      [
        Alcotest.test_case "object range metadata copy" `Quick
          test_object_range_metadata_and_copy;
        Alcotest.test_case "object versioning" `Quick test_object_versioning;
        Alcotest.test_case "bucket config roundtrip" `Quick
          test_bucket_config_roundtrip;
        Alcotest.test_case "multipart edges" `Quick test_multipart_edges;
        Alcotest.test_case "path transfer streams" `Quick
          test_path_transfer_streams;
        Alcotest.test_case "multipart path transfer resumes" `Quick
          test_multipart_path_transfer_resumes;
      ] );
  ]

let () = Alcotest.run "awskit-s3-minio-contract" (suite ())
