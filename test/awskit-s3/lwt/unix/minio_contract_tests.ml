module S3 = Awskit_s3_lwt_unix
module Object = Awskit_s3.Object
module Multipart = Awskit_s3.Multipart
module Range = Awskit_s3.Range

let getenv_default name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

let endpoint =
  getenv_default "AWSKIT_S3_MINIO_ENDPOINT" "http://127.0.0.1:9000"
  |> Awskit.Endpoint.of_string_exn

let access_key = getenv_default "AWSKIT_S3_MINIO_ACCESS_KEY_ID" "minioadmin"
let secret_key = getenv_default "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY" "minioadmin"

let region =
  getenv_default "AWSKIT_S3_MINIO_REGION" "us-east-1"
  |> Awskit.Region.of_string_exn

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:access_key
    ~secret_access_key:secret_key ()

let connect () =
  match
    S3.create ~endpoint ~addressing_style:`Path ~region ~credentials
      ~clock:Ptime_clock.now ()
  with
  | Ok conn -> conn
  | Error error -> Alcotest.failf "connect: %a" Awskit_s3.Error.pp error

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let await label promise = Lwt_main.run promise |> ok_or_fail label

let expect_status label status result =
  match result with
  | Error (Awskit.Error.Service service) when service.status = status -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit_s3.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected service status %d" label status

let bucket_name suffix =
  Printf.sprintf "awskit-minio-%d-%s" (Unix.getpid ()) suffix

let delete_object key =
  {
    Object.Delete_many.key;
    version_id = None;
    etag = None;
    last_modified_time = None;
    size = None;
  }

let cleanup_bucket conn ~bucket =
  let open Lwt.Syntax in
  let* keys_result = S3.Object.list_keys conn ~bucket () in
  (match keys_result with
    | Ok [] -> Lwt.return_unit
    | Ok keys ->
        let objects = List.map delete_object keys in
        let* _ = S3.Object.delete_many conn ~bucket ~objects in
        Lwt.return_unit
    | Error _ -> Lwt.return_unit)
  |> fun deleted ->
  let* () = deleted in
  let* _ = S3.Bucket.delete conn ~bucket in
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

let remove_file path = try Sys.remove path with Sys_error _ -> ()
let first = function [] -> None | value :: _ -> Some value

let with_bucket suffix f =
  let conn = connect () in
  let bucket = bucket_name suffix in
  Lwt_main.run (cleanup_bucket conn ~bucket);
  ignore (await "create bucket" (S3.Bucket.create conn ~bucket ()));
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (cleanup_bucket conn ~bucket))
    (fun () -> f conn ~bucket)

let test_object_range_metadata_and_copy () =
  with_bucket "objects" (fun conn ~bucket ->
      let put_options =
        {
          Object.Put.default_options with
          content_type = Some "text/plain";
          metadata = [ ("origin", "minio"); ("mode", "copy") ];
        }
      in
      ignore
        (await "put object"
           (S3.Object.Buffer.put_string conn ~bucket ~key:"range.txt"
              ~options:put_options "abcdefghij"));
      let range_options =
        {
          Object.Get.default_options with
          range = Some (Range.bytes_exn ~start:2L ~finish:5L);
        }
      in
      let info, body =
        await "get range"
          (S3.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
             ~options:range_options ~max_size:16L ())
      in
      Alcotest.(check string) "range body" "cdef" body;
      Alcotest.(check int)
        "range status" 206
        (Awskit.Response.status info.request);
      Alcotest.(check (option string))
        "content-range" (Some "bytes 2-5/10")
        (Awskit.Response.header info.request "content-range");
      let suffix_options =
        { Object.Get.default_options with range = Some (Range.suffix_exn 3L) }
      in
      let _info, suffix =
        await "get suffix"
          (S3.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
             ~options:suffix_options ~max_size:16L ())
      in
      Alcotest.(check string) "suffix body" "hij" suffix;
      let invalid_range_options =
        { Object.Get.default_options with range = Some (Range.from_exn 99L) }
      in
      expect_status "invalid range" 416
        (Lwt_main.run
           (S3.Object.Buffer.get_string conn ~bucket ~key:"range.txt"
              ~options:invalid_range_options ~max_size:16L ()));
      ignore
        (await "copy object"
           (S3.Object.copy conn ~src_bucket:bucket ~src_key:"range.txt"
              ~dst_bucket:bucket ~dst_key:"copied.txt" ()));
      let copied =
        await "head copied" (S3.Object.head conn ~bucket ~key:"copied.txt" ())
      in
      Alcotest.(check (option string))
        "copied metadata" (Some "minio")
        (List.assoc_opt "origin" copied.metadata);
      let replace_options =
        {
          Object.Copy.default_options with
          metadata = Some (`Replace [ ("origin", "replacement") ]);
        }
      in
      ignore
        (await "copy replace"
           (S3.Object.copy conn ~src_bucket:bucket ~src_key:"range.txt"
              ~dst_bucket:bucket ~dst_key:"replaced.txt"
              ~options:replace_options ()));
      let replaced =
        await "head replaced"
          (S3.Object.head conn ~bucket ~key:"replaced.txt" ())
      in
      Alcotest.(check (option string))
        "replaced metadata" (Some "replacement")
        (List.assoc_opt "origin" replaced.metadata);
      Alcotest.(check (option string))
        "old metadata removed" None
        (List.assoc_opt "mode" replaced.metadata))

let test_multipart_edges () =
  with_bucket "multipart" (fun conn ~bucket ->
      let first_body = String.make Multipart.Managed.min_part_size 'a' in
      let overwritten_body = String.make Multipart.Managed.min_part_size 'b' in
      let final_body = "second" in
      let upload =
        await "create multipart"
          (S3.Multipart.create conn ~bucket ~key:"edges.bin" ())
      in
      let upload_id = upload.upload.upload_id in
      let first =
        await "upload first"
          (S3.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
             ~part_number:1
             ~body:(S3.Runtime.string_body first_body)
             ())
      in
      let second =
        await "upload second"
          (S3.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
             ~part_number:2
             ~body:(S3.Runtime.string_body final_body)
             ())
      in
      let overwritten =
        await "overwrite first"
          (S3.Multipart.upload_part conn ~bucket ~key:"edges.bin" ~upload_id
             ~part_number:1
             ~body:(S3.Runtime.string_body overwritten_body)
             ())
      in
      expect_status "complete stale etag" 400
        (Lwt_main.run
           (S3.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
              [ first.part; second.part ]));
      ignore
        (await "complete overwritten"
           (S3.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
              [ overwritten.part; second.part ]));
      let _info, body =
        await "get multipart"
          (S3.Object.Buffer.get_string conn ~bucket ~key:"edges.bin"
             ~max_size:
               (Int64.of_int
                  (String.length overwritten_body + String.length final_body))
             ())
      in
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
           (S3.Multipart.complete conn ~bucket ~key:"edges.bin" ~upload_id
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
               (S3.Object.Transfer.upload_from_path conn ~bucket
                  ~key:"transfer.bin" ~path:upload_path
                  ~on_progress:(fun transferred ->
                    upload_progress := transferred :: !upload_progress)
                  ()));
          ignore
            (await "download path"
               (S3.Object.Transfer.download_to_path conn ~bucket
                  ~key:"transfer.bin" ~path:download_path
                  ~on_progress:(fun transferred ->
                    download_progress := transferred :: !download_progress)
                  ()));
          Alcotest.(check string) "download body" body (read_file download_path);
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
      let part_size = Multipart.Managed.min_part_size in
      let path = Filename.temp_file "awskit-multipart-upload" ".bin" in
      let body =
        String.init
          ((part_size * 2) + 17)
          (fun index -> Char.chr ((index mod 10) + Char.code '0'))
      in
      write_file path body;
      let options = { Multipart.Managed.default_options with part_size } in
      let progress = ref [] in
      Fun.protect
        ~finally:(fun () -> remove_file path)
        (fun () ->
          let created =
            await "create multipart for resume"
              (S3.Multipart.create conn ~bucket ~key:"resume.bin" ())
          in
          let upload_id = created.upload.upload_id in
          let first_part = String.sub body 0 part_size in
          ignore
            (await "upload resume seed"
               (S3.Multipart.upload_part conn ~bucket ~key:"resume.bin"
                  ~upload_id ~part_number:1
                  ~body:(S3.Runtime.string_body first_part)
                  ()));
          let result =
            await "resume multipart path"
              (S3.Object.Transfer.resume_multipart_upload_from_path conn ~bucket
                 ~key:"resume.bin" ~upload_id ~options ~concurrency:2 ~path
                 ~on_progress:(fun transferred ->
                   progress := transferred :: !progress)
                 ())
          in
          Alcotest.(check int) "completed parts" 3 (List.length result.parts);
          let _info, downloaded =
            await "get resumed multipart"
              (S3.Object.Buffer.get_string conn ~bucket ~key:"resume.bin"
                 ~max_size:(Int64.of_int (String.length body))
                 ())
          in
          Alcotest.(check string) "resumed body" body downloaded;
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
        Alcotest.test_case "multipart edges" `Quick test_multipart_edges;
        Alcotest.test_case "path transfer streams" `Quick
          test_path_transfer_streams;
        Alcotest.test_case "multipart path transfer resumes" `Quick
          test_multipart_path_transfer_resumes;
      ] );
  ]

let () = Alcotest.run "awskit-s3-minio-contract" (suite ())
