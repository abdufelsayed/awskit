open Awskit_s3
open Awskit_s3_test

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let reader body : Recording_s3.Reader.t =
  { Recording_runtime.body; read_error_after = None; active = true; offset = 0 }

let expect_error label predicate = function
  | Error error when predicate error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected error" label

let test_reader_to_string_accepts_exact_limit () =
  match
    Recording_s3.Reader.to_string ~chunk_size:2 ~max_bytes:3L (reader "abc")
  with
  | Error error -> Alcotest.failf "read failed: %a" Error.pp error
  | Ok body -> Alcotest.(check string) "body" "abc" body

let test_reader_to_string_rejects_over_limit () =
  Recording_s3.Reader.to_string ~chunk_size:2 ~max_bytes:2L (reader "abc")
  |> expect_error "oversized body" is_body_error

let test_reader_to_string_stops_after_limit_exceeded () =
  let reader = reader "abcdef" in
  Recording_s3.Reader.to_string ~chunk_size:2 ~max_bytes:3L reader
  |> expect_error "oversized body" is_body_error;
  Alcotest.(check int) "bytes read" 4 reader.offset

let test_reader_to_bytes_preserves_binary_data () =
  let payload = "\000\255binary" in
  match
    Recording_s3.Reader.to_bytes ~chunk_size:3
      ~max_bytes:(Int64.of_int (String.length payload))
      (reader payload)
  with
  | Error error -> Alcotest.failf "read failed: %a" Error.pp error
  | Ok body -> Alcotest.(check string) "body" payload (Bytes.to_string body)

let test_reader_buffering_rejects_invalid_chunk_size () =
  let expect_chunk_size = is_validation_field "chunk_size" in
  Recording_s3.Reader.next ~chunk_size:0 (reader "abc")
  |> expect_error "next chunk_size" expect_chunk_size;
  Recording_s3.Reader.fold ~chunk_size:0 (reader "abc") ~init:() ~f:(fun () _ ->
      Ok ())
  |> expect_error "fold chunk_size" expect_chunk_size;
  Recording_s3.Reader.iter ~chunk_size:0 (reader "abc") ~f:(fun _ -> Ok ())
  |> expect_error "iter chunk_size" expect_chunk_size;
  Recording_s3.Reader.to_string ~chunk_size:0 ~max_bytes:3L (reader "abc")
  |> expect_error "to_string chunk_size" expect_chunk_size;
  Recording_s3.Reader.to_bytes ~chunk_size:0 ~max_bytes:3L (reader "abc")
  |> expect_error "to_bytes chunk_size" expect_chunk_size

let test_reader_buffering_rejects_negative_max_bytes () =
  let expect_max_bytes = is_validation_field "max_bytes" in
  Recording_s3.Reader.to_string ~max_bytes:(-1L) (reader "abc")
  |> expect_error "to_string max_bytes" expect_max_bytes;
  Recording_s3.Reader.to_bytes ~max_bytes:(-1L) (reader "abc")
  |> expect_error "to_bytes max_bytes" expect_max_bytes

let test_reader_fold_processes_chunks_incrementally () =
  let chunks = ref [] in
  match
    Recording_s3.Reader.fold ~chunk_size:2 (reader "abcde") ~init:0
      ~f:(fun total chunk ->
        chunks := Bytes.to_string chunk :: !chunks;
        Ok (total + Bytes.length chunk))
  with
  | Error error -> Alcotest.failf "fold failed: %a" Error.pp error
  | Ok total ->
      Alcotest.(check int) "total" 5 total;
      Alcotest.(check (list string))
        "chunks" [ "ab"; "cd"; "e" ] (List.rev !chunks)

let test_body_stream_exposes_replayability () =
  let body =
    Recording_s3.Body.of_stream ~content_length:5L ~replayable:true
      ~write:(fun writer ->
        Recording_s3.Body.Writer.write_string writer "hello")
  in
  match body with
  | Error error -> Alcotest.failf "body failed: %a" Error.pp error
  | Ok body ->
      Alcotest.(check (option int64))
        "content length" (Some 5L)
        (Recording_s3.Body.content_length body);
      Alcotest.(check bool)
        "replayable" true
        (Recording_s3.Body.replayable body)

let test_in_memory_bodies_are_replayable () =
  Alcotest.(check bool)
    "string body" true
    (Recording_s3.Body.replayable (Recording_s3.Body.of_string "hello"));
  Alcotest.(check bool)
    "bytes body" true
    (Recording_s3.Body.replayable
       (Recording_s3.Body.of_bytes (Bytes.of_string "hello")))

let test_body_stream_rejects_negative_content_length () =
  let write_called = ref false in
  Recording_s3.Body.of_stream ~content_length:(-1L) ~replayable:false
    ~write:(fun _ ->
      write_called := true;
      Ok ())
  |> expect_error "negative content_length" (fun error ->
      is_validation_field "content_length" error);
  Alcotest.(check bool) "write not called" false !write_called

let test_replayable_stream_body_is_retried () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 503 slow_down; response 200 "" ]
  in
  let body =
    Recording_s3.Body.of_stream ~content_length:4L ~replayable:true
      ~write:(fun writer -> Recording_s3.Body.Writer.write_string writer "body")
    |> ok_or_fail "stream body"
  in
  match
    Recording_s3.Object.put conn ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "file") ~body ()
  with
  | Error error -> Alcotest.failf "put failed: %a" Error.pp error
  | Ok _ -> Alcotest.(check int) "attempts" 2 (List.length conn.calls)

let suite =
  [
    ( "streaming",
      [
        Alcotest.test_case "reader accepts exact byte limit" `Quick
          test_reader_to_string_accepts_exact_limit;
        Alcotest.test_case "reader rejects oversized body" `Quick
          test_reader_to_string_rejects_over_limit;
        Alcotest.test_case "reader stops after limit exceeded" `Quick
          test_reader_to_string_stops_after_limit_exceeded;
        Alcotest.test_case "reader preserves binary bytes" `Quick
          test_reader_to_bytes_preserves_binary_data;
        Alcotest.test_case "reader rejects invalid chunk size" `Quick
          test_reader_buffering_rejects_invalid_chunk_size;
        Alcotest.test_case "reader rejects negative max bytes" `Quick
          test_reader_buffering_rejects_negative_max_bytes;
        Alcotest.test_case "reader folds chunks incrementally" `Quick
          test_reader_fold_processes_chunks_incrementally;
        Alcotest.test_case "body stream exposes replayability" `Quick
          test_body_stream_exposes_replayability;
        Alcotest.test_case "in-memory bodies are replayable" `Quick
          test_in_memory_bodies_are_replayable;
        Alcotest.test_case "body stream rejects negative content length" `Quick
          test_body_stream_rejects_negative_content_length;
        Alcotest.test_case "replayable stream body is retried" `Quick
          test_replayable_stream_body_is_retried;
      ] );
  ]
