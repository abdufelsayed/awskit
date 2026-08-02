module R = Recording_runtime
module Runtime = R.Runtime

let error_text error = Awskit.Error.to_string_hum error

let is_body_error error =
  match Awskit.Error.kind error with
  | Body _ -> true
  | Validation _ | Credentials _ | Signing _ | Endpoint _ | Transport _
  | Timeout _ | Cancelled _ | Service _ | Decode _ | Retry_exhausted _
  | Not_supported _ | Multiple _ ->
      false

let expect_body_error label = function
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body error" label

let is_transport_error error =
  match Awskit.Error.kind error with
  | Transport _ -> true
  | Validation _ | Credentials _ | Signing _ | Endpoint _ | Timeout _
  | Cancelled _ | Service _ | Body _ | Decode _ | Retry_exhausted _
  | Not_supported _ | Multiple _ ->
      false

let expect_transport_error label = function
  | Error error when is_transport_error error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected transport error" label

let contains ~substring text =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.sub text index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Https
      ~host:"service.us-east-1.amazonaws.com" ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:`PUT ~target ()

let bytes_to_string bytes = Bytes.sub_string bytes 0 (Bytes.length bytes)

let response_body ?read_error_after body : R.response_body =
  { body; read_error_after; consumed_bytes = ref 0L; draining = ref false }

let read_response_body body =
  R.Response_body.with_reader body ~consume:(fun reader ->
      let rec loop chunks =
        match R.Response_body.next ~chunk_size:4 reader with
        | Error _ as error -> error
        | Ok None -> Ok (String.concat "" (List.rev chunks))
        | Ok (Some chunk) -> loop (bytes_to_string chunk :: chunks)
      in
      loop [])

let test_io_bind_law () =
  let result =
    R.IO.bind (R.IO.return 1) (fun value -> R.IO.return (value + 1))
  in
  Alcotest.(check int) "bind result" 2 result

let test_request_body_descriptor_is_authoritative () =
  let body = R.Request_body.of_string "hello" in
  let descriptor = R.Request_body.descriptor body in
  Alcotest.(check (option int64))
    "content length" (Some 5L) descriptor.content_length;
  Alcotest.(check string)
    "payload hash"
    (Awskit.Body.Payload_hash.to_header_value
       (Awskit.Body.Payload_hash.sha256_of_string "hello"))
    (Awskit.Body.Payload_hash.to_header_value descriptor.payload_hash);
  Alcotest.(check bool) "replayable" true descriptor.replayable;
  Alcotest.(check (option int64))
    "content_length helper" (Some 5L)
    (R.Request_body.content_length body)

let test_request_body_of_bytes_owns_input () =
  let bytes = Bytes.of_string "hello" in
  let body = R.Request_body.of_bytes bytes in
  Bytes.fill bytes 0 (Bytes.length bytes) 'x';
  let conn = R.connect [ R.response 200 "" ] in
  match
    R.Transport.with_response conn request ~body
      ~consume:(fun _ response_body -> R.Response_body.discard response_body)
  with
  | Error error ->
      Alcotest.failf "unexpected transport error: %a" Awskit.Error.pp error
  | Ok () ->
      Alcotest.(check string) "request body" "hello" (R.last_call conn).body

let test_stream_request_body_preserves_descriptor () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        R.Request_body.write_string writer "body")
  in
  let actual = R.Request_body.descriptor body in
  Alcotest.(check (option int64))
    "content length" descriptor.content_length actual.content_length;
  Alcotest.(check string)
    "payload hash"
    (Awskit.Body.Payload_hash.to_header_value descriptor.payload_hash)
    (Awskit.Body.Payload_hash.to_header_value actual.payload_hash);
  Alcotest.(check bool) "replayable" false actual.replayable;
  Alcotest.(check (option int64))
    "content_length helper" descriptor.content_length
    (R.Request_body.content_length body)

let test_stream_request_body_length_mismatch_prevents_transport () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        R.Request_body.write_string writer "abcde")
  in
  let conn = R.connect [ R.response 200 "" ] in
  R.Transport.with_response conn request ~body ~consume:(fun _ response_body ->
      R.Response_body.discard response_body)
  |> expect_body_error "length mismatch";
  Alcotest.(check int) "no transport call" 0 (List.length conn.calls)

let test_stream_request_body_writer_error_prevents_transport () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let writer_error = Awskit.Error.Producer.body "writer failed" in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        match R.Request_body.write_string writer "body" with
        | Error _ as error -> error
        | Ok () -> Error writer_error)
  in
  let conn = R.connect [ R.response 200 "" ] in
  R.Transport.with_response conn request ~body ~consume:(fun _ response_body ->
      R.Response_body.discard response_body)
  |> expect_body_error "writer error";
  Alcotest.(check int) "no transport call" 0 (List.length conn.calls)

let test_stream_request_body_writer_cannot_escape_scope () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:4L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false
      ()
  in
  let escaped = ref None in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        escaped := Some writer;
        R.Request_body.write_string writer "body")
  in
  Alcotest.(check (option int64))
    "body was constructed" (Some 4L)
    (R.Request_body.content_length body);
  let writer =
    match !escaped with
    | Some writer -> writer
    | None -> Alcotest.fail "expected escaped writer"
  in
  R.Request_body.write_string writer "again"
  |> expect_body_error "writer escaped scope"

let test_write_subbytes_copies_selected_slice () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:true
      ()
  in
  let source = Bytes.of_string "abcdef" in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        R.Request_body.write_subbytes writer source ~off:1 ~len:3)
  in
  Bytes.fill source 0 (Bytes.length source) 'x';
  let conn = R.connect [ R.response 200 "" ] in
  match
    R.Transport.with_response conn request ~body
      ~consume:(fun _ response_body -> R.Response_body.discard response_body)
  with
  | Error error ->
      Alcotest.failf "unexpected transport error: %a" Awskit.Error.pp error
  | Ok () ->
      Alcotest.(check string) "request body slice" "bcd" (R.last_call conn).body

let test_write_subbytes_rejects_invalid_bounds () =
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:1L
      ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:true
      ()
  in
  let body =
    R.Request_body.of_stream descriptor ~write:(fun writer ->
        R.Request_body.write_subbytes writer (Bytes.of_string "abc") ~off:2
          ~len:2)
  in
  let conn = R.connect [ R.response 200 "" ] in
  R.Transport.with_response conn request ~body ~consume:(fun _ response_body ->
      R.Response_body.discard response_body)
  |> expect_body_error "invalid subbytes bounds";
  Alcotest.(check int) "no transport call" 0 (List.length conn.calls)

let test_response_next_reads_bounded_chunks () =
  match
    R.Response_body.with_reader (response_body "abcde") ~consume:(fun reader ->
        match R.Response_body.next ~chunk_size:2 reader with
        | Error _ as error -> error
        | Ok None -> Error (Awskit.Error.Producer.body "missing first chunk")
        | Ok (Some first) -> (
            match R.Response_body.next ~chunk_size:3 reader with
            | Error _ as error -> error
            | Ok None ->
                Error (Awskit.Error.Producer.body "missing second chunk")
            | Ok (Some second) -> (
                match R.Response_body.next ~chunk_size:3 reader with
                | Error _ as error -> error
                | Ok None -> Ok (bytes_to_string first, bytes_to_string second)
                | Ok (Some _) ->
                    Error (Awskit.Error.Producer.body "unexpected chunk"))))
  with
  | Error error ->
      Alcotest.failf "unexpected response body error: %a" Awskit.Error.pp error
  | Ok (first, second) ->
      Alcotest.(check string) "first chunk" "ab" first;
      Alcotest.(check string) "second chunk" "cde" second

let test_response_next_rejects_nonpositive_chunk_size () =
  R.Response_body.with_reader (response_body "abcdef") ~consume:(fun reader ->
      R.Response_body.next ~chunk_size:0 reader)
  |> expect_body_error "chunk size"

let test_response_read_validates_bounds_and_zero_length () =
  (match
     R.Response_body.with_reader (response_body "abcdef")
       ~consume:(fun reader ->
         let bytes = Bytes.of_string "xxx" in
         match R.Response_body.read reader bytes ~off:1 ~len:0 with
         | Error _ as error -> error
         | Ok count -> Ok (count, Bytes.to_string bytes))
   with
  | Error error ->
      Alcotest.failf "unexpected zero-length read error: %a" Awskit.Error.pp
        error
  | Ok (count, bytes) ->
      Alcotest.(check int) "zero-length read count" 0 count;
      Alcotest.(check string) "zero-length read leaves buffer" "xxx" bytes);
  R.Response_body.with_reader (response_body "abcdef") ~consume:(fun reader ->
      let bytes = Bytes.create 3 in
      R.Response_body.read reader bytes ~off:2 ~len:2)
  |> expect_body_error "invalid response read bounds"

let test_response_read_error_is_sticky_inside_scope () =
  let observed = ref None in
  R.Response_body.with_reader (response_body ~read_error_after:2 "abcdef")
    ~consume:(fun reader ->
      let bytes = Bytes.create 4 in
      match R.Response_body.read reader bytes ~off:0 ~len:2 with
      | Error _ as error -> error
      | Ok first_count -> (
          match R.Response_body.read reader bytes ~off:0 ~len:1 with
          | Ok _ -> Error (Awskit.Error.Producer.body "expected read error")
          | Error first_error -> (
              match R.Response_body.read reader bytes ~off:0 ~len:1 with
              | Ok _ ->
                  Error
                    (Awskit.Error.Producer.body "expected sticky read error")
              | Error second_error ->
                  observed :=
                    Some
                      ( first_count,
                        is_body_error first_error,
                        is_body_error second_error );
                  Error first_error)))
  |> expect_body_error "sticky read error";
  match !observed with
  | Some (first_count, first_is_body, second_is_body) ->
      Alcotest.(check int) "first read count" 2 first_count;
      Alcotest.(check bool) "first read error is body" true first_is_body;
      Alcotest.(check bool) "second read error is body" true second_is_body
  | None -> Alcotest.fail "expected read error observations"

let test_discard_drains_successful_body () =
  match R.Response_body.discard (response_body "abcdef") with
  | Error error ->
      Alcotest.failf "unexpected discard error: %a" Awskit.Error.pp error
  | Ok () -> ()

let test_response_reader_cannot_escape_scope () =
  let escaped = ref None in
  match
    R.Response_body.with_reader (response_body "abcdef") ~consume:(fun reader ->
        escaped := Some reader;
        Ok ())
  with
  | Error error ->
      Alcotest.failf "unexpected with_reader error: %a" Awskit.Error.pp error
  | Ok () -> (
      let reader =
        match !escaped with
        | Some reader -> reader
        | None -> Alcotest.fail "expected escaped reader"
      in
      let bytes = Bytes.create 1 in
      match R.Response_body.read reader bytes ~off:0 ~len:1 with
      | Error error ->
          Alcotest.(check bool)
            "use-after-scope is body error" true (is_body_error error)
      | Ok _ -> Alcotest.fail "escaped reader read succeeded")

let test_response_consumer_error_wins_over_drain_error () =
  let consumer_error = Awskit.Error.Producer.body "consumer failed" in
  match
    R.Response_body.with_reader (response_body ~read_error_after:0 "abcdef")
      ~consume:(fun _ -> Error consumer_error)
  with
  | Error error ->
      Alcotest.(check bool)
        "consumer error wins" true
        (contains ~substring:"consumer failed" (error_text error))
  | Ok _ -> Alcotest.fail "expected consumer error"

let test_response_success_reports_drain_error () =
  match
    R.Response_body.with_reader (response_body ~read_error_after:0 "abcdef")
      ~consume:(fun _ -> Ok ())
  with
  | Error error ->
      Alcotest.(check bool) "drain error" true (is_body_error error)
  | Ok () -> Alcotest.fail "expected drain error"

let test_discard_reports_drain_error () =
  match
    R.Response_body.discard (response_body ~read_error_after:0 "abcdef")
  with
  | Error error ->
      Alcotest.(check bool) "discard error" true (is_body_error error)
  | Ok () -> Alcotest.fail "expected discard drain error"

let test_transport_callback_exception_is_not_wrapped () =
  let callback_exn = Failure "callback exploded" in
  let conn = R.connect [ R.response 200 "" ] in
  try
    ignore
      (R.Transport.with_response conn request ~body:R.Request_body.empty
         ~consume:(fun _ _ -> raise callback_exn)
        : (unit, Awskit.Error.t) result);
    Alcotest.fail "expected callback exception"
  with
  | exn when exn == callback_exn -> ()
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)

let test_response_reader_callback_exception_is_not_wrapped () =
  let callback_exn = Failure "response callback exploded" in
  try
    ignore
      (R.Response_body.with_reader (response_body "abcdef") ~consume:(fun _ ->
           raise callback_exn)
        : (unit, Awskit.Error.t) result);
    Alcotest.fail "expected callback exception"
  with
  | exn when exn == callback_exn -> ()
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)

let test_transport_uses_fifo_responses_and_records_calls () =
  let conn = R.connect [ R.response 201 "one"; R.response 202 "two" ] in
  let call body =
    R.Transport.with_response conn request ~body:(R.Request_body.of_string body)
      ~consume:(fun response response_body ->
        match read_response_body response_body with
        | Error _ as error -> error
        | Ok response_body -> Ok (Awskit.Response.status response, response_body))
  in
  let first = call "first" in
  let second = call "second" in
  let check_result label expected = function
    | Error error ->
        Alcotest.failf "%s returned unexpected error: %a" label Awskit.Error.pp
          error
    | Ok actual -> Alcotest.(check (pair int string)) label expected actual
  in
  check_result "first response" (201, "one") first;
  check_result "second response" (202, "two") second;
  Alcotest.(check (list string))
    "recorded request bodies" [ "first"; "second" ]
    (List.map (fun (call : R.call) -> call.body) (List.rev conn.calls));
  Alcotest.(check int) "responses consumed" 0 (List.length conn.responses)

let test_transport_no_response_is_transport_error_and_records_attempt () =
  let conn = R.connect [] in
  R.Transport.with_response conn request ~body:(R.Request_body.of_string "body")
    ~consume:(fun _ response_body -> R.Response_body.discard response_body)
  |> expect_transport_error "no canned response";
  Alcotest.(check int) "attempt recorded" 1 (List.length conn.calls);
  Alcotest.(check string) "recorded body" "body" (R.last_call conn).body

let test_runtime_witness_uses_concrete_capabilities () =
  let region = Awskit.Region.of_string_exn "eu-west-1" in
  let endpoint = Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 () in
  let conn =
    R.connect ~region ~endpoint
      [ R.response ~headers:[ ("x-amz-request-id", "request-1") ] 204 "" ]
  in
  Alcotest.(check string)
    "runtime region" "eu-west-1"
    (Awskit.Region.to_string (Runtime.Endpoint.region conn));
  Alcotest.(check (option bool))
    "runtime endpoint" (Some true)
    (Option.map
       (Awskit.Endpoint.equal endpoint)
       (Runtime.Endpoint.endpoint conn));
  (match Runtime.Credentials.resolve conn with
  | Error error ->
      Alcotest.failf "unexpected credentials error: %a" Awskit.Error.pp error
  | Ok credentials ->
      Alcotest.(check string)
        "runtime credentials" "AKIDEXAMPLE"
        (Awskit.Credentials.access_key_id credentials));
  let body = Runtime.Request_body.of_string "payload" in
  Alcotest.(check (option int64))
    "runtime body length" (Some 7L)
    (Runtime.Request_body.content_length body);
  (match
     Runtime.Transport.with_response conn request ~body
       ~consume:(fun response response_body ->
         match Runtime.Response_body.discard response_body with
         | Error _ as error -> error
         | Ok () -> Ok response)
   with
  | Error error ->
      Alcotest.failf "unexpected runtime transport error: %a" Awskit.Error.pp
        error
  | Ok response ->
      Alcotest.(check int)
        "runtime status" 204
        (Awskit.Response.status response);
      Alcotest.(check (option string))
        "runtime request id" (Some "request-1")
        (Awskit.Response.request_id response));
  Alcotest.(check string)
    "runtime request body" "payload" (R.last_call conn).body

let test_retry_timeout_random_and_sleep_capabilities () =
  let retry_policy =
    Awskit.Retry.create_exn ~max_attempts:2 ~jitter:1.0
      ~base_delay:(Ptime.Span.of_float_s 1.0 |> Option.get)
      ~max_delay:(Ptime.Span.of_float_s 1.0 |> Option.get)
      ()
  in
  let conn = R.connect ~retry_policy [] in
  Alcotest.(check int)
    "max attempts" 2
    (Awskit.Retry.max_attempts (R.Retry.policy conn));
  let delay =
    Awskit.Retry.delay (R.Retry.policy conn) ~attempt:1
      ~error:(Awskit.Error.Producer.transport ~retryable:true "temporary")
      ~random_float:(R.Random.float conn)
    |> Option.get
  in
  Alcotest.(check (float 0.0001))
    "runtime random controls delay" 0.5
    (Ptime.Span.to_float_s delay);
  R.Sleeper.sleep conn delay;
  Alcotest.(check int) "sleep recorded" 1 (List.length conn.sleeps);
  Alcotest.(check bool)
    "timeout policy visible" true
    (Option.is_some (Awskit.Timeout.span (R.Timeout.policy conn) `Connect))

let suite =
  [
    ( "contract:awskit:runtime-core",
      [
        Alcotest.test_case "io bind law" `Quick test_io_bind_law;
        Alcotest.test_case "request body descriptor" `Quick
          test_request_body_descriptor_is_authoritative;
        Alcotest.test_case "request body of_bytes owns input" `Quick
          test_request_body_of_bytes_owns_input;
        Alcotest.test_case "stream request body preserves descriptor" `Quick
          test_stream_request_body_preserves_descriptor;
        Alcotest.test_case
          "stream request body length mismatch prevents transport" `Quick
          test_stream_request_body_length_mismatch_prevents_transport;
        Alcotest.test_case "stream request body writer error prevents transport"
          `Quick test_stream_request_body_writer_error_prevents_transport;
        Alcotest.test_case "stream request body writer cannot escape scope"
          `Quick test_stream_request_body_writer_cannot_escape_scope;
        Alcotest.test_case "write_subbytes copies selected slice" `Quick
          test_write_subbytes_copies_selected_slice;
        Alcotest.test_case "write_subbytes rejects invalid bounds" `Quick
          test_write_subbytes_rejects_invalid_bounds;
        Alcotest.test_case "response next reads bounded chunks" `Quick
          test_response_next_reads_bounded_chunks;
        Alcotest.test_case "response next rejects nonpositive chunk size" `Quick
          test_response_next_rejects_nonpositive_chunk_size;
        Alcotest.test_case "response read validates bounds and zero length"
          `Quick test_response_read_validates_bounds_and_zero_length;
        Alcotest.test_case "response read error is sticky inside scope" `Quick
          test_response_read_error_is_sticky_inside_scope;
        Alcotest.test_case "discard drains successful body" `Quick
          test_discard_drains_successful_body;
        Alcotest.test_case "response reader cannot escape scope" `Quick
          test_response_reader_cannot_escape_scope;
        Alcotest.test_case "consumer error wins over drain error" `Quick
          test_response_consumer_error_wins_over_drain_error;
        Alcotest.test_case "success reports drain error" `Quick
          test_response_success_reports_drain_error;
        Alcotest.test_case "discard reports drain error" `Quick
          test_discard_reports_drain_error;
        Alcotest.test_case "transport callback exception is not wrapped" `Quick
          test_transport_callback_exception_is_not_wrapped;
        Alcotest.test_case "response reader callback exception is not wrapped"
          `Quick test_response_reader_callback_exception_is_not_wrapped;
        Alcotest.test_case "transport uses FIFO responses and records calls"
          `Quick test_transport_uses_fifo_responses_and_records_calls;
        Alcotest.test_case "transport no response records failed attempt" `Quick
          test_transport_no_response_is_transport_error_and_records_attempt;
        Alcotest.test_case "runtime witness uses concrete capabilities" `Quick
          test_runtime_witness_uses_concrete_capabilities;
        Alcotest.test_case "retry timeout random and sleep" `Quick
          test_retry_timeout_random_and_sleep_capabilities;
      ] );
  ]

let () = Alcotest.run "awskit-runtime-contracts" suite
