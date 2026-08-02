open Base
module O = Awskit.Observability

let with_loopback_or_skip f =
  try f ()
  with Unix.Unix_error (Unix.EPERM, "bind", _) ->
    Loopback_policy.handle_bind_denied ()

let unlink_if_present path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let bucket = Awskit_s3.Bucket_name.of_string_exn "observability-bucket"
let key = Awskit_s3.Object_key.of_string_exn "transfer-object"

let transfer_name completion =
  completion
  |> O.For_projection.Operation.Completion.info
  |> O.For_projection.Operation.Info.name

let measurement name completion =
  completion
  |> O.For_projection.Operation.Completion.measurements
  |> List.find_map ~f:(fun value ->
      if String.equal name (O.For_projection.Measurement.name value) then
        match O.For_projection.Measurement.value value with
        | Int value -> Some (Int64.of_int value)
        | Int64 value -> Some value
        | Float _ -> None
      else None)

let dimension name completion =
  completion
  |> O.For_projection.Operation.Completion.dimensions
  |> List.find_map ~f:(fun value ->
      if String.equal name (O.For_projection.Dimension.name value) then
        Some (O.For_projection.Dimension.value value)
      else None)

let metric_observer ?trace_sinks () =
  let observations = ref [] in
  let sink =
    O.Metric_sink.create ~name:"transfer-capture" ~needs_clock:true
      ~enabled:(fun family ->
        List.mem
          [
            "awskit.s3.transfers";
            "awskit.s3.transfer.logical_bytes";
            "awskit.s3.transfer.parts";
            "awskit.s3.transfers_in_flight";
          ]
          (O.For_projection.Metric.Family.name family)
          ~equal:String.equal)
      ~observe:(fun observation -> observations := observation :: !observations)
  in
  let ticks = ref 0L in
  let observer =
    Awskit_eio.Observability.create ~logs:false ~metric_sinks:[ sink ]
      ?trace_sinks
      ~clock:(fun () ->
        let value = !ticks in
        ticks := Int64.succ value;
        value)
      ()
  in
  (observer, observations)

let gauge_value observer direction =
  let module Metric = O.For_projection.Metric in
  Awskit_eio.Observability.instrument_snapshot observer
  |> List.find_map ~f:(fun observation ->
      let family = Metric.Observation.family observation in
      let labels = Metric.Observation.labels observation in
      if
        String.equal "awskit.s3.transfers_in_flight" (Metric.Family.name family)
        && List.equal String.equal
             (List.map labels ~f:Metric.Label.encoded)
             [ direction ]
      then
        match Metric.Observation.value observation with
        | Int64 value -> Some value
        | Int _ | Float _ -> None
      else None)
  |> Option.value ~default:0L

let transfer_completions completions =
  List.filter !completions ~f:(fun completion ->
      String.equal "awskit.s3.transfer" (transfer_name completion))

let metric_values name observations =
  let module Metric = O.For_projection.Metric in
  List.filter_map !observations ~f:(fun observation ->
      let family = Metric.Observation.family observation in
      if String.equal name (Metric.Family.name family) then
        Some
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
      else None)

let all_transfer_and_operation_sink completions =
  Awskit_eio.Observability.Trace_sink.create ~name:"transfer-topology"
    ~needs_clock:false
    ~enabled:(fun info ->
      let name = O.For_projection.Operation.Info.name info in
      String.equal "awskit.s3.transfer" name
      || String.equal "awskit.s3.operation" name)
    ~start:(fun _ ->
      {
        Awskit_eio.Observability.Trace_sink.within =
          (fun callback -> callback ());
        correlation = [];
        finish = (fun completion -> completions := completion :: !completions);
      })
    ~event_enabled:(fun _ -> false)
    ~event:(fun _ -> ())

let operation_completions completions =
  List.filter !completions ~f:(fun completion ->
      String.equal "awskit.s3.operation" (transfer_name completion))

let sorted_operation_names completions =
  operation_completions completions
  |> List.filter_map ~f:(dimension "aws.operation")
  |> List.sort ~compare:String.compare

let multipart_part_size = Awskit_s3.Transfer.min_part_size
let multipart_length = multipart_part_size + 1

let test_successful_upload_and_download env () =
  let source_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".source"
  in
  let target_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".target"
  in
  Unix.unlink target_native;
  let source = Eio.Path.(Eio.Stdenv.fs env / source_native) in
  let target = Eio.Path.(Eio.Stdenv.fs env / target_native) in
  Eio.Path.save ~create:(`Or_truncate 0o600) source "upload!";
  let requests = ref [] in
  let completions = ref [] in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"transfer-trace"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.transfer"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer, metric_observations =
    metric_observer ~trace_sinks:[ sink ] ()
  in
  let upload_progress_gauges = ref [] in
  let download_progress_gauges = ref [] in
  let result =
    Observability_eio_fixture.with_connection env ~body:"payload"
      ~on_request:(fun method_ _headers body ->
        requests := (method_, body) :: !requests)
      ~observability:observer
    @@ fun connection ~calls:_ ->
    let upload =
      Awskit_s3_eio.Object.Transfer.upload_file connection ~bucket ~key
        ~on_progress:(fun _ ->
          upload_progress_gauges :=
            gauge_value observer "upload" :: !upload_progress_gauges)
        ~path:source ()
    in
    match upload with
    | Error _ as error -> error
    | Ok upload -> (
        match
          Awskit_s3_eio.Object.Transfer.download_file connection ~bucket ~key
            ~on_progress:(fun _ ->
              download_progress_gauges :=
                gauge_value observer "download" :: !download_progress_gauges)
            ~path:target ()
        with
        | Ok download -> Ok (upload, download)
        | Error _ as error -> error)
  in
  Exn.protect
    ~f:(fun () ->
      let upload, download =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok result -> result
      in
      ignore upload;
      ignore download;
      Alcotest.(check (list (pair string string)))
        "PUT, HEAD, and GET request bodies"
        [ ("PUT", "upload!"); ("HEAD", ""); ("GET", "") ]
        (List.rev !requests);
      Alcotest.(check string) "download body" "payload" (Eio.Path.load target);
      let transfers = transfer_completions completions in
      Alcotest.(check int) "two transfer completions" 2 (List.length transfers);
      let uploads, downloads =
        List.partition_tf transfers ~f:(fun completion ->
            String.equal "upload"
              (Option.value_exn (dimension "transfer.direction" completion)))
      in
      Alcotest.(check int) "one upload completion" 1 (List.length uploads);
      Alcotest.(check int) "one download completion" 1 (List.length downloads);
      let check_success direction completion =
        Alcotest.(check string)
          (direction ^ " outcome") "ok"
          (O.For_projection.Operation.Completion.outcome completion
          |> O.Outcome.to_string);
        Alcotest.(check (option int64))
          (direction ^ " logical bytes")
          (Some 7L)
          (measurement "transfer.logical_bytes" completion);
        Alcotest.(check (option int64))
          (direction ^ " parts") (Some 1L)
          (measurement "transfer.parts" completion)
      in
      check_success "upload" (List.hd_exn uploads);
      check_success "download" (List.hd_exn downloads);
      Alcotest.(check int64)
        "upload gauge released" 0L
        (gauge_value observer "upload");
      Alcotest.(check int64)
        "download gauge released" 0L
        (gauge_value observer "download");
      Alcotest.(check bool)
        "upload gauge was held" true
        (List.exists !upload_progress_gauges ~f:(Int64.equal 1L));
      Alcotest.(check bool)
        "download gauge was held" true
        (List.exists !download_progress_gauges ~f:(Int64.equal 1L));
      let transfer_samples =
        metric_values "awskit.s3.transfers" metric_observations
      in
      Alcotest.(check int)
        "transfer counter samples" 2
        (List.length transfer_samples);
      List.iter transfer_samples ~f:(fun (labels, value) ->
          Alcotest.(check int) "counter labels are exact" 2 (List.length labels);
          Alcotest.(check bool)
            "counter direction is bounded" true
            (List.equal String.equal labels [ "upload"; "ok" ]
            || List.equal String.equal labels [ "download"; "ok" ]);
          match value with
          | O.For_projection.Metric.Value.Int64 1L -> ()
          | _ -> Alcotest.fail "transfer counter value was not one");
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "logical byte samples" 2
        (List.length logical_samples);
      List.iter logical_samples ~f:(fun (labels, value) ->
          Alcotest.(check bool)
            "logical byte labels are bounded" true
            (List.equal String.equal labels [ "upload" ]
            || List.equal String.equal labels [ "download" ]);
          match value with
          | O.For_projection.Metric.Value.Int64 7L -> ()
          | _ -> Alcotest.fail "logical byte value was not seven");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "part samples" 2 (List.length part_samples);
      List.iter part_samples ~f:(fun (labels, value) ->
          Alcotest.(check bool)
            "part labels are bounded" true
            (List.equal String.equal labels [ "upload" ]
            || List.equal String.equal labels [ "download" ]);
          match value with
          | O.For_projection.Metric.Value.Int64 1L -> ()
          | _ -> Alcotest.fail "part value was not one"))
    ~finally:(fun () ->
      Unix.unlink source_native;
      Unix.unlink target_native)

let test_multipart_upload_topology env () =
  let source_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".source"
  in
  let source = Eio.Path.(Eio.Stdenv.fs env / source_native) in
  Eio.Path.save ~create:(`Or_truncate 0o600) source
    (String.make multipart_length 'u');
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let create_body =
        "<InitiateMultipartUploadResult><UploadId>upload-id</UploadId>"
        ^ "</InitiateMultipartUploadResult>"
      in
      let complete_body =
        "<CompleteMultipartUploadResult><ETag>\"complete\"</ETag>"
        ^ "</CompleteMultipartUploadResult>"
      in
      let responses =
        [
          (200, [], create_body);
          (200, [ ("etag", "\"part-1\"") ], "");
          (200, [ ("etag", "\"part-2\"") ], "");
          (200, [], complete_body);
        ]
      in
      let requests = ref [] in
      let result =
        Observability_eio_fixture.with_connection env ~responses
          ~on_request:(fun method_ _headers body ->
            requests := (method_, String.length body) :: !requests)
          ~observability:observer
        @@ fun connection ~calls:_ ->
        let options =
          Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1L
            ~part_size:multipart_part_size ~concurrency:1 ()
        in
        Awskit_s3_eio.Object.Transfer.upload_file connection ~bucket ~key
          ~options ~path:source ()
      in
      let upload =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok upload -> upload
      in
      (match Awskit_s3.Transfer.upload_strategy upload with
      | `Multipart -> ()
      | `Put -> Alcotest.fail "multipart upload selected PutObject");
      Alcotest.(check int64)
        "multipart upload logical bytes"
        (Int64.of_int multipart_length)
        (Awskit_s3.Transfer.upload_bytes_transferred upload);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "multipart request methods"
        [ "POST"; "PUT"; "PUT"; "POST" ]
        (List.map requests ~f:fst);
      let request_body_lengths = List.map requests ~f:snd in
      Alcotest.(check int)
        "create request body length" 0
        (List.nth_exn request_body_lengths 0);
      Alcotest.(check int)
        "first upload part body length" multipart_part_size
        (List.nth_exn request_body_lengths 1);
      Alcotest.(check int)
        "final upload part body length" 1
        (List.nth_exn request_body_lengths 2);
      Alcotest.(check bool)
        "complete request has an XML body" true
        (Int.compare (List.nth_exn request_body_lengths 3) 0 > 0);
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one multipart transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "multipart transfer direction" "upload"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "multipart transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "multipart transfer logical bytes"
        (Some (Int64.of_int multipart_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "multipart transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "multipart logical child operations"
        [
          "CompleteMultipartUpload";
          "CreateMultipartUpload";
          "UploadPart";
          "UploadPart";
        ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "multipart child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "multipart child transfer direction" "upload"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "multipart logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "multipart logical byte metric value"
            (Int64.of_int multipart_length)
            value
      | _ -> Alcotest.fail "multipart logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "multipart part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "multipart part metric shape")
    ~finally:(fun () -> Unix.unlink source_native)

let test_resume_multipart_upload_topology env () =
  let source_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".source"
  in
  let source = Eio.Path.(Eio.Stdenv.fs env / source_native) in
  Eio.Path.save ~create:(`Or_truncate 0o600) source
    (String.make multipart_length 'u');
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let list_parts_body =
        Fmt.str
          "<ListPartsResult><Part><PartNumber>1</PartNumber><ETag>\"existing\"</ETag><Size>%d</Size></Part></ListPartsResult>"
          multipart_part_size
      in
      let complete_response_body =
        "<CompleteMultipartUploadResult><ETag>\"complete\"</ETag>"
        ^ "</CompleteMultipartUploadResult>"
      in
      let upload =
        Awskit_s3.Multipart.Upload.resume ~bucket ~key
          ~upload_id:(Awskit_s3.Multipart.Upload_id.of_string_exn "upload-id")
      in
      let responses =
        [
          (200, [], list_parts_body);
          (200, [ ("etag", "\"fresh-1\"") ], "");
          (200, [ ("etag", "\"fresh-2\"") ], "");
          (200, [], complete_response_body);
        ]
      in
      let requests = ref [] in
      let complete_requests = ref [] in
      let result =
        Observability_eio_fixture.with_connection env ~responses
          ~on_request:(fun method_ _headers body ->
            requests := (method_, String.length body) :: !requests;
            if String.equal method_ "POST" then
              complete_requests := body :: !complete_requests)
          ~observability:observer
        @@ fun connection ~calls:_ ->
        let options =
          Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1L
            ~part_size:multipart_part_size ~concurrency:1 ()
        in
        Awskit_s3_eio.Object.Transfer.resume_multipart_upload_file connection
          ~upload ~options ~path:source ()
      in
      let result =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok result -> result
      in
      Alcotest.(check int64)
        "resumed upload logical bytes"
        (Int64.of_int multipart_length)
        result.bytes_transferred;
      Alcotest.(check int) "resumed upload parts" 2 (List.length result.parts);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "resume request methods"
        [ "GET"; "PUT"; "PUT"; "POST" ]
        (List.map requests ~f:fst);
      let request_body_lengths = List.map requests ~f:snd in
      Alcotest.(check int)
        "resume ListParts request body length" 0
        (List.nth_exn request_body_lengths 0);
      Alcotest.(check int)
        "resume first upload part body length" multipart_part_size
        (List.nth_exn request_body_lengths 1);
      Alcotest.(check int)
        "resume final upload part body length" 1
        (List.nth_exn request_body_lengths 2);
      let complete_request =
        match List.rev !complete_requests with
        | [ body ] -> body
        | _ -> Alcotest.fail "resume complete request count"
      in
      Alcotest.(check bool)
        "resume completion uses fresh part one" true
        (Base.String.is_substring complete_request ~substring:"fresh-1");
      Alcotest.(check bool)
        "resume completion uses fresh part two" true
        (Base.String.is_substring complete_request ~substring:"fresh-2");
      Alcotest.(check bool)
        "resume completion omits existing part etag" false
        (Base.String.is_substring complete_request ~substring:"existing");
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one resumed transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "resumed transfer direction" "upload"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "resumed transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "resumed transfer logical bytes"
        (Some (Int64.of_int multipart_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "resumed transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "resumed logical child operations"
        [ "CompleteMultipartUpload"; "ListParts"; "UploadPart"; "UploadPart" ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "resumed child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "resumed child transfer direction" "upload"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "resumed logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "resumed logical byte metric value"
            (Int64.of_int multipart_length)
            value
      | _ -> Alcotest.fail "resumed logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "resumed part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "resumed part metric shape")
    ~finally:(fun () -> Unix.unlink source_native)

let test_ranged_download_topology env () =
  let target_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".target"
  in
  Unix.unlink target_native;
  let target = Eio.Path.(Eio.Stdenv.fs env / target_native) in
  let first_part = String.make multipart_part_size 'a' in
  let final_part = "b" in
  let object_length = multipart_length in
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let responses =
        [
          ( 200,
            [
              ("content-length", Int.to_string object_length);
              ("etag", "\"object\"");
            ],
            "" );
          ( 206,
            [
              ( "content-range",
                Fmt.str "bytes 0-%d/%d" (multipart_part_size - 1) object_length
              );
            ],
            first_part );
          ( 206,
            [
              ( "content-range",
                Fmt.str "bytes %d-%d/%d" multipart_part_size (object_length - 1)
                  object_length );
            ],
            final_part );
        ]
      in
      let requests = ref [] in
      let result =
        Observability_eio_fixture.with_connection env ~responses
          ~on_request:(fun method_ headers _body ->
            requests := (method_, headers) :: !requests)
          ~observability:observer
        @@ fun connection ~calls:_ ->
        let options =
          Awskit_s3.Transfer.download_options_exn ~multipart_threshold:1L
            ~part_size:multipart_part_size ~concurrency:1 ()
        in
        Awskit_s3_eio.Object.Transfer.download_file connection ~bucket ~key
          ~options ~path:target ()
      in
      let download =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok download -> download
      in
      (match Awskit_s3.Transfer.download_strategy download with
      | `Ranged -> ()
      | `Get -> Alcotest.fail "ranged download selected GetObject");
      Alcotest.(check int64)
        "ranged download logical bytes"
        (Int64.of_int object_length)
        (Awskit_s3.Transfer.download_bytes_transferred download);
      (match download with
      | Awskit_s3.Transfer.Ranged result ->
          Alcotest.(check int) "ranged download parts" 2 result.parts
      | Awskit_s3.Transfer.Get _ -> Alcotest.fail "ranged download result shape");
      Alcotest.(check string)
        "ranged download bytes" (first_part ^ final_part) (Eio.Path.load target);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "ranged request methods" [ "HEAD"; "GET"; "GET" ]
        (List.map requests ~f:fst);
      let ranges =
        List.filter_map requests ~f:(fun (_method_, headers) ->
            List.find_map headers ~f:(fun (name, value) ->
                if String.equal "range" name then Some value else None))
      in
      Alcotest.(check (list string))
        "ranged GetObject byte ranges"
        [
          Fmt.str "bytes=0-%d" (multipart_part_size - 1);
          Fmt.str "bytes=%d-%d" multipart_part_size (object_length - 1);
        ]
        ranges;
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one ranged transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "ranged transfer direction" "download"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "ranged transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "ranged transfer logical bytes"
        (Some (Int64.of_int object_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "ranged transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "ranged logical child operations"
        [ "GetObject"; "GetObject"; "HeadObject" ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "ranged child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "ranged child transfer direction" "download"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "ranged logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "download" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "ranged logical byte metric value"
            (Int64.of_int object_length)
            value
      | _ -> Alcotest.fail "ranged logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "ranged part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "download" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "ranged part metric shape")
    ~finally:(fun () -> unlink_if_present target_native)

let test_failed_upload_completes_once env () =
  let missing_native =
    Stdlib.Filename.temp_file "awskit-transfer-missing" ".source"
  in
  Unix.unlink missing_native;
  let missing = Eio.Path.(Eio.Stdenv.fs env / missing_native) in
  let completions = ref [] in
  let sink =
    Awskit_eio.Observability.Trace_sink.create ~name:"transfer-failure"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.transfer"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_eio.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer =
    Awskit_eio.Observability.create ~logs:false ~trace_sinks:[ sink ] ()
  in
  Observability_eio_fixture.with_connection_without_server env
    ~observability:observer
  @@ fun connection ~calls:_ ->
  let result =
    Awskit_s3_eio.Object.Transfer.upload_file connection ~bucket ~key
      ~path:missing ()
  in
  Alcotest.(check bool) "missing source fails" true (Result.is_error result);
  let transfers = transfer_completions completions in
  Alcotest.(check int)
    "failed transfer completes once" 1 (List.length transfers);
  Alcotest.(check bool)
    "failure is not success" true
    (not
       (String.equal "ok"
          (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
          |> O.Outcome.to_string)));
  Alcotest.(check int64)
    "failed transfer gauge released" 0L
    (gauge_value observer "upload")

let test_progress_callback_exception_completes_once env () =
  let source_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".source"
  in
  let source = Eio.Path.(Eio.Stdenv.fs env / source_native) in
  Eio.Path.save ~create:(`Or_truncate 0o600) source "upload!";
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink =
        Awskit_eio.Observability.Trace_sink.create ~name:"transfer-callback-exn"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_eio.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let result =
        try
          Observability_eio_fixture.with_connection env ~observability:observer
          @@ fun connection ~calls:_ ->
          ignore
            (Awskit_s3_eio.Object.Transfer.upload_file connection ~bucket ~key
               ~on_progress:(fun _ ->
                 raise (Failure "progress callback failure"))
               ~path:source ());
          `Returned
        with
        | Failure message -> `Raised message
        | Unix.Unix_error (Unix.EPERM, "bind", _) as exn -> raise exn
        | exn -> `Other exn
      in
      (match result with
      | `Raised message ->
          Alcotest.(check string)
            "progress exception is preserved" "progress callback failure"
            message
      | `Other exn ->
          Alcotest.failf "progress callback exception changed to %s"
            (Exn.to_string exn)
      | `Returned -> Alcotest.fail "progress callback unexpectedly returned");
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "callback failure completes once" 1 (List.length transfers);
      Alcotest.(check string)
        "callback failure outcome" "exception"
        (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "callback failure has no logical bytes" None
        (measurement "transfer.logical_bytes" (List.hd_exn transfers));
      Alcotest.(check int64)
        "callback failure gauge released" 0L
        (gauge_value observer "upload");
      let samples = metric_values "awskit.s3.transfers" metric_observations in
      Alcotest.(check int)
        "callback failure transfer metric" 1 (List.length samples);
      match samples with
      | [ (labels, O.For_projection.Metric.Value.Int64 1L) ] ->
          Alcotest.(check (list string))
            "callback failure metric labels" [ "upload"; "exception" ] labels
      | [ (_, _) ] -> Alcotest.fail "callback failure metric value was not one"
      | _ -> Alcotest.fail "callback failure metric sample missing")
    ~finally:(fun () -> Unix.unlink source_native)

let test_native_cancellation_completes_once env () =
  let source_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".source"
  in
  let source = Eio.Path.(Eio.Stdenv.fs env / source_native) in
  Eio.Path.save ~create:(`Or_truncate 0o600) source "upload!";
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink =
        Awskit_eio.Observability.Trace_sink.create ~name:"transfer-cancel"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_eio.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      Observability_eio_fixture.with_connection env ~response_delay:0.25
        ~observability:observer
      @@ fun connection ~calls:_ ->
      let cancelled =
        try
          Eio.Cancel.sub (fun context ->
              Eio.Switch.run @@ fun sw ->
              Eio.Fiber.fork ~sw (fun () ->
                  Eio.Fiber.yield ();
                  Eio.Cancel.cancel context (Failure "transfer cancellation"));
              ignore
                (Awskit_s3_eio.Object.Transfer.upload_file connection ~bucket
                   ~key ~path:source ());
              false)
        with Eio.Cancel.Cancelled _ -> true
      in
      Alcotest.(check bool) "native cancellation is preserved" true cancelled;
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "cancelled transfer completes once" 1 (List.length transfers);
      Alcotest.(check string)
        "cancelled transfer outcome" "cancelled"
        (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "cancelled transfer has no logical bytes" None
        (measurement "transfer.logical_bytes" (List.hd_exn transfers));
      Alcotest.(check int64)
        "cancelled transfer gauge released" 0L
        (gauge_value observer "upload");
      let samples = metric_values "awskit.s3.transfers" metric_observations in
      Alcotest.(check int) "cancelled transfer metric" 1 (List.length samples);
      match samples with
      | [ (labels, O.For_projection.Metric.Value.Int64 1L) ] ->
          Alcotest.(check (list string))
            "cancelled transfer metric labels" [ "upload"; "cancelled" ] labels
      | [ (_, _) ] ->
          Alcotest.fail "cancelled transfer metric value was not one"
      | _ -> Alcotest.fail "cancelled transfer metric sample missing")
    ~finally:(fun () -> Unix.unlink source_native)

let test_download_callback_exception_cleans_target env () =
  let target_native =
    Stdlib.Filename.temp_file "awskit-transfer-observation" ".target"
  in
  Unix.unlink target_native;
  let target = Eio.Path.(Eio.Stdenv.fs env / target_native) in
  Exn.protect
    ~f:(fun () ->
      let completions = ref [] in
      let sink =
        Awskit_eio.Observability.Trace_sink.create ~name:"download-callback-exn"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_eio.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, _metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let result =
        try
          Observability_eio_fixture.with_connection env ~observability:observer
          @@ fun connection ~calls:_ ->
          ignore
            (Awskit_s3_eio.Object.Transfer.download_file connection ~bucket ~key
               ~on_progress:(fun _ ->
                 raise (Failure "download progress callback failure"))
               ~path:target ());
          `Returned
        with
        | Failure message -> `Raised message
        | Unix.Unix_error (Unix.EPERM, "bind", _) as exn -> raise exn
        | exn -> `Other exn
      in
      (match result with
      | `Raised message ->
          Alcotest.(check string)
            "download callback exception is preserved"
            "download progress callback failure" message
      | `Other exn ->
          Alcotest.failf "download callback exception changed to %s"
            (Exn.to_string exn)
      | `Returned -> Alcotest.fail "download callback unexpectedly returned");
      let target_exists =
        try
          ignore (Eio.Path.stat ~follow:true target);
          true
        with _ -> false
      in
      Alcotest.(check bool)
        "failed download target was cleaned" false target_exists;
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "download callback completion once" 1 (List.length transfers);
      let completion = List.hd_exn transfers in
      Alcotest.(check string)
        "download callback outcome" "exception"
        (O.For_projection.Operation.Completion.outcome completion
        |> O.Outcome.to_string);
      Alcotest.(check string)
        "download direction" "download"
        (Option.value_exn (dimension "transfer.direction" completion));
      Alcotest.(check (option int64))
        "failed download has no logical bytes" None
        (measurement "transfer.logical_bytes" completion);
      Alcotest.(check int64)
        "failed download gauge released" 0L
        (gauge_value observer "download"))
    ~finally:(fun () -> unlink_if_present target_native)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "awskit-transfer-observation-eio"
    [
      ( "behavior:transfer-observation-eio",
        [
          Alcotest.test_case "success topology and metrics" `Quick (fun () ->
              with_loopback_or_skip (fun () ->
                  test_successful_upload_and_download env ()));
          Alcotest.test_case "multipart upload topology" `Quick (fun () ->
              with_loopback_or_skip (fun () ->
                  test_multipart_upload_topology env ()));
          Alcotest.test_case "resume multipart upload topology" `Quick
            (fun () ->
              with_loopback_or_skip (fun () ->
                  test_resume_multipart_upload_topology env ()));
          Alcotest.test_case "ranged download topology" `Quick (fun () ->
              with_loopback_or_skip (fun () ->
                  test_ranged_download_topology env ()));
          Alcotest.test_case "failure completion" `Quick (fun () ->
              test_failed_upload_completes_once env ());
          Alcotest.test_case "progress callback exception" `Quick (fun () ->
              with_loopback_or_skip (fun () ->
                  test_progress_callback_exception_completes_once env ()));
          Alcotest.test_case "native cancellation" `Quick (fun () ->
              with_loopback_or_skip (fun () ->
                  test_native_cancellation_completes_once env ()));
          Alcotest.test_case "download cleanup on callback exception" `Quick
            (fun () ->
              with_loopback_or_skip (fun () ->
                  test_download_callback_exception_cleans_target env ()));
        ] );
    ]
