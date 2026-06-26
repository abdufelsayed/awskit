module Model = Transfer_model

module type TARGET = sig
  val name : string
  val run_upload_case : Model.upload_case -> bool
end

let count_from_env ~var ~default =
  match Sys.getenv_opt var with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let max_extra_part_bytes = 4096

let body_char length index =
  match index mod 17 with
  | 0 -> '\000'
  | 1 -> '\255'
  | _ -> Char.chr (32 + (((index * 17) + length) mod 91))

let body_of_length length = String.init length (body_char length)

let part_count_for_length ~length ~part_size =
  Model.part_count_for_length ~length ~part_size

let unique_offsets length part_size =
  [
    0;
    length - 1;
    Model.chunk_size - 1;
    Model.chunk_size;
    Model.chunk_size + 1;
    part_size - 1;
    part_size;
    part_size + 1;
    (2 * part_size) - 1;
    2 * part_size;
  ]
  |> List.filter (fun offset -> offset >= 0 && offset < length)
  |> List.sort_uniq Int.compare

let part_size_gen =
  QCheck.Gen.int_range Model.min_part_size
    (Model.min_part_size + max_extra_part_bytes)

let boundary_lengths part_size =
  [
    0;
    1;
    Model.chunk_size - 1;
    Model.chunk_size;
    Model.chunk_size + 1;
    part_size - 1;
    part_size;
    part_size + 1;
    2 * part_size;
  ]
  |> List.filter (fun length -> length >= 0)
  |> List.sort_uniq Int.compare

let length_gen part_size =
  let open QCheck.Gen in
  oneof_weighted
    [
      (9, oneof_list (boundary_lengths part_size));
      (2, int_range 2 4096);
      (2, int_range 4097 (128 * 1024));
      (3, int_range part_size ((2 * part_size) + 257));
      (1, int_range ((2 * part_size) + 1) ((3 * part_size) + 257));
    ]

let fault_offset_gen ~length ~part_size =
  let offsets = unique_offsets length part_size in
  let open QCheck.Gen in
  match offsets with
  | [] -> int_range 0 (length - 1)
  | offsets ->
      oneof_weighted [ (2, oneof_list offsets); (3, int_range 0 (length - 1)) ]

let fault_gen ~length ~part_size =
  let open QCheck.Gen in
  let read_write_faults =
    if length = 0 then []
    else
      [
        ( 2,
          map
            (fun n -> Some (Model.Read_fails_after n))
            (fault_offset_gen ~length ~part_size) );
        ( 2,
          map
            (fun n -> Some (Model.Write_fails_after n))
            (fault_offset_gen ~length ~part_size) );
        ( 2,
          map
            (fun n -> Some (Model.Progress_callback_raises_after n))
            (fault_offset_gen ~length ~part_size) );
        ( 2,
          map
            (fun n -> Some (Model.Cancellation_after_bytes n))
            (fault_offset_gen ~length ~part_size) );
        (1, return (Some Model.Cleanup_delete_fails));
      ]
  in
  let multipart_faults =
    if length < part_size then []
    else
      let part_count = part_count_for_length ~length ~part_size in
      [
        (2, return (Some Model.Multipart_create_fails));
        ( 3,
          map
            (fun n -> Some (Model.Multipart_part_fails n))
            (int_range 1 part_count) );
        (2, return (Some Model.Multipart_complete_fails));
      ]
  in
  oneof_weighted ((3, return None) :: (read_write_faults @ multipart_faults))

let upload_case_gen =
  let open QCheck.Gen in
  part_size_gen >>= fun part_size ->
  length_gen part_size >>= fun length ->
  fault_gen ~length ~part_size >>= fun fault ->
  return { Model.local_body = body_of_length length; part_size; fault }

let size_bin case =
  let length = String.length case.Model.local_body in
  if length = 0 then "transfer.size.zero"
  else if length < case.part_size then "transfer.size.single"
  else if length = case.part_size then "transfer.size.exact-part"
  else "transfer.size.multipart"

let fault_bin = function
  | None -> "transfer.fault.none"
  | Some (Model.Read_fails_after _) -> "transfer.fault.read"
  | Some (Model.Write_fails_after _) -> "transfer.fault.write"
  | Some (Model.Progress_callback_raises_after _) ->
      "transfer.fault.progress-callback"
  | Some (Model.Cancellation_after_bytes _) -> "transfer.fault.cancellation"
  | Some Model.Multipart_create_fails -> "transfer.fault.multipart-create"
  | Some (Model.Multipart_part_fails _) -> "transfer.fault.multipart-part"
  | Some Model.Multipart_complete_fails -> "transfer.fault.multipart-complete"
  | Some Model.Cleanup_delete_fails -> "transfer.fault.cleanup-delete"

let boundary_bins case =
  let length = String.length case.Model.local_body in
  let boundary =
    if length = 0 then [ "transfer.boundary.zero" ]
    else if length = 1 then [ "transfer.boundary.one-byte" ]
    else if length = Model.chunk_size - 1 then
      [ "transfer.boundary.chunk-minus-one" ]
    else if length = Model.chunk_size then [ "transfer.boundary.chunk-exact" ]
    else if length = Model.chunk_size + 1 then
      [ "transfer.boundary.chunk-plus-one" ]
    else if length = case.part_size - 1 then
      [ "transfer.boundary.part-minus-one" ]
    else if length = case.part_size then [ "transfer.boundary.part-exact" ]
    else if length = case.part_size + 1 then
      [ "transfer.boundary.part-plus-one" ]
    else []
  in
  if Model.uses_multipart case && Model.part_count case >= 2 then
    "transfer.boundary.multipart-two-or-more-parts" :: boundary
  else boundary

let coverage_bins case =
  size_bin case :: fault_bin case.Model.fault :: boundary_bins case

let normalize_fault ~part_size ~length = function
  | None -> None
  | Some (Model.Read_fails_after offset) when length > 0 ->
      Some (Model.Read_fails_after (min offset (length - 1)))
  | Some (Model.Write_fails_after offset) when length > 0 ->
      Some (Model.Write_fails_after (min offset (length - 1)))
  | Some (Model.Progress_callback_raises_after offset) when length > 0 ->
      Some (Model.Progress_callback_raises_after (min offset (length - 1)))
  | Some (Model.Cancellation_after_bytes offset) when length > 0 ->
      Some (Model.Cancellation_after_bytes (min offset (length - 1)))
  | Some Model.Multipart_create_fails when length >= part_size ->
      Some Model.Multipart_create_fails
  | Some Model.Multipart_complete_fails when length >= part_size ->
      Some Model.Multipart_complete_fails
  | Some (Model.Multipart_part_fails part_number) when length >= part_size ->
      let part_count = part_count_for_length ~length ~part_size in
      Some (Model.Multipart_part_fails (min part_number part_count))
  | Some Model.Cleanup_delete_fails when length >= part_size ->
      Some Model.Cleanup_delete_fails
  | Some _ -> None

let fault_equal left right =
  match (left, right) with
  | Model.Read_fails_after left, Model.Read_fails_after right ->
      Int.equal left right
  | Model.Write_fails_after left, Model.Write_fails_after right ->
      Int.equal left right
  | ( Model.Progress_callback_raises_after left,
      Model.Progress_callback_raises_after right ) ->
      Int.equal left right
  | Model.Cancellation_after_bytes left, Model.Cancellation_after_bytes right ->
      Int.equal left right
  | Model.Multipart_create_fails, Model.Multipart_create_fails -> true
  | Model.Multipart_part_fails left, Model.Multipart_part_fails right ->
      Int.equal left right
  | Model.Multipart_complete_fails, Model.Multipart_complete_fails -> true
  | Model.Cleanup_delete_fails, Model.Cleanup_delete_fails -> true
  | _ -> false

let fault_option_equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> fault_equal left right
  | None, Some _ | Some _, None -> false

let distinct_strict_fault_shrinks current candidates =
  candidates
  |> List.fold_left
       (fun acc candidate ->
         if
           fault_option_equal current candidate
           || List.exists (fault_option_equal candidate) acc
         then acc
         else candidate :: acc)
       []
  |> List.rev

let shrink_fault fault =
  let candidates =
    match fault with
    | None -> []
    | Some (Model.Read_fails_after offset) ->
        [
          None;
          Some (Model.Read_fails_after 0);
          Some (Model.Read_fails_after (offset / 2));
        ]
    | Some (Model.Write_fails_after offset) ->
        [
          None;
          Some (Model.Write_fails_after 0);
          Some (Model.Write_fails_after (offset / 2));
        ]
    | Some (Model.Progress_callback_raises_after offset) ->
        [
          None;
          Some (Model.Progress_callback_raises_after 0);
          Some (Model.Progress_callback_raises_after (offset / 2));
        ]
    | Some (Model.Cancellation_after_bytes offset) ->
        [
          None;
          Some (Model.Cancellation_after_bytes 0);
          Some (Model.Cancellation_after_bytes (offset / 2));
        ]
    | Some (Model.Multipart_part_fails part_number) ->
        [
          None;
          Some (Model.Multipart_part_fails 1);
          Some (Model.Multipart_part_fails (max 1 (part_number / 2)));
        ]
    | Some Model.Multipart_create_fails
    | Some Model.Multipart_complete_fails
    | Some Model.Cleanup_delete_fails ->
        [ None ]
  in
  QCheck.Iter.of_list (distinct_strict_fault_shrinks fault candidates)

let iter_to_list iter =
  let values = ref [] in
  iter (fun value -> values := value :: !values);
  List.rev !values

let fault_option_to_string = function
  | None -> "none"
  | Some fault -> Model.fault_to_string fault

let test_shrink_fault_excludes_original () =
  let examples =
    [
      None;
      Some (Model.Read_fails_after 0);
      Some (Model.Read_fails_after 1);
      Some (Model.Write_fails_after 0);
      Some (Model.Write_fails_after 1);
      Some (Model.Progress_callback_raises_after 0);
      Some (Model.Progress_callback_raises_after 1);
      Some (Model.Cancellation_after_bytes 0);
      Some (Model.Cancellation_after_bytes 1);
      Some Model.Multipart_create_fails;
      Some (Model.Multipart_part_fails 1);
      Some (Model.Multipart_part_fails 2);
      Some Model.Multipart_complete_fails;
      Some Model.Cleanup_delete_fails;
    ]
  in
  List.iter
    (fun fault ->
      Alcotest.(check bool)
        (fault_option_to_string fault)
        false
        (List.exists
           (fun candidate -> fault_option_equal fault candidate)
           (iter_to_list (shrink_fault fault))))
    examples

let case ?fault ~length ~part_size () =
  { Model.local_body = body_of_length length; part_size; fault }

let deterministic_cases =
  let multipart_length = Model.min_part_size + 32 in
  [
    ( "single read fault reports partial movement",
      case ~length:4096 ~part_size:Model.min_part_size
        ~fault:(Model.Read_fails_after 17) () );
    ( "multipart create fault moves no bytes",
      case ~length:multipart_length ~part_size:Model.min_part_size
        ~fault:Model.Multipart_create_fails () );
    ( "multipart write fault reports partial failed part",
      case ~length:multipart_length ~part_size:Model.min_part_size
        ~fault:(Model.Write_fails_after (Model.min_part_size + 3))
        () );
    ( "multipart part fault skips failed body",
      case ~length:multipart_length ~part_size:Model.min_part_size
        ~fault:(Model.Multipart_part_fails 2) () );
    ( "multipart complete fault reports all bytes before cleanup",
      case ~length:multipart_length ~part_size:Model.min_part_size
        ~fault:Model.Multipart_complete_fails () );
    ( "progress callback exception is preserved",
      case ~length:4096 ~part_size:Model.min_part_size
        ~fault:(Model.Progress_callback_raises_after 17) () );
    ( "cancellation leaves no completed object",
      case ~length:4096 ~part_size:Model.min_part_size
        ~fault:(Model.Cancellation_after_bytes 17) () );
    ( "cleanup failure is reported without hiding primary failure",
      case ~length:multipart_length ~part_size:Model.min_part_size
        ~fault:Model.Cleanup_delete_fails () );
  ]

let unique_lengths lengths current =
  lengths
  |> List.filter (fun length -> length >= 0 && length <> current)
  |> List.sort_uniq Int.compare

let shrink_lengths case =
  let length = String.length case.Model.local_body in
  let common = if length = 0 then [] else [ 0; 1; min 32 length; length / 2 ] in
  let multipart =
    if Model.uses_multipart case then
      [
        case.part_size;
        case.part_size + 1;
        ((Model.part_count case - 1) * case.part_size) + 1;
      ]
    else []
  in
  unique_lengths (boundary_lengths case.part_size @ common @ multipart) length
  |> QCheck.Iter.of_list

let shrink_case case =
  let shrink_faults =
    QCheck.Iter.map
      (fun fault -> { case with Model.fault })
      (shrink_fault case.Model.fault)
  in
  let shrink_body =
    QCheck.Iter.map
      (fun length ->
        {
          case with
          Model.local_body = body_of_length length;
          fault =
            normalize_fault ~part_size:case.part_size ~length case.Model.fault;
        })
      (shrink_lengths case)
  in
  QCheck.Iter.append shrink_faults shrink_body

let expected_upload_to_string = function
  | Model.Upload_succeeds { remote_body; progress_final } ->
      Printf.sprintf
        "expected_result=success expected_byte_movement={read=%d; written=%d; \
         progress_final=%d} remote_body={length=%d; digest=%s} \
         cleanup_ownership={owned_upload_aborted=false; \
         cleanup_error_secondary=false}"
        progress_final progress_final progress_final
        (String.length remote_body)
        (Digest.to_hex (Digest.string remote_body))
  | Upload_fails
      {
        remote_absent;
        owned_upload_aborted;
        read_bytes;
        written_bytes;
        progress_bytes;
        progress_report_max;
        callback_exception_preserved;
        cleanup_error_secondary;
        cancelled;
      } ->
      Printf.sprintf
        "expected_result=failure expected_byte_movement={read=%d; written=%d; \
         progress=%d; progress_report_max=%d} \
         cleanup_ownership={remote_absent=%b; owned_upload_aborted=%b; \
         cleanup_error_secondary=%b} callback_exception_preserved=%b \
         cancelled=%b"
        read_bytes written_bytes progress_bytes progress_report_max
        remote_absent owned_upload_aborted cleanup_error_secondary
        callback_exception_preserved cancelled

let upload_case_to_string case =
  let length = String.length case.Model.local_body in
  Printf.sprintf
    "length=%d part_size=%d part_count=%d fault_schedule=%s body_digest=%s %s \
     observed_result=<reported by target failure>"
    length case.part_size (Model.part_count case)
    (fault_option_to_string case.fault)
    (Digest.to_hex (Digest.string case.local_body))
    (expected_upload_to_string (Model.expected_upload case))

let arbitrary =
  QCheck.make ~print:upload_case_to_string ~shrink:shrink_case upload_case_gen

let required_transfer_bins =
  [
    "transfer.size.zero";
    "transfer.size.single";
    "transfer.size.exact-part";
    "transfer.size.multipart";
    "transfer.fault.none";
    "transfer.fault.read";
    "transfer.fault.write";
    "transfer.fault.progress-callback";
    "transfer.fault.cancellation";
    "transfer.fault.multipart-create";
    "transfer.fault.multipart-part";
    "transfer.fault.multipart-complete";
    "transfer.fault.cleanup-delete";
  ]

let required_transfer_boundary_bins =
  [
    "transfer.boundary.zero";
    "transfer.boundary.one-byte";
    "transfer.boundary.chunk-minus-one";
    "transfer.boundary.chunk-exact";
    "transfer.boundary.chunk-plus-one";
    "transfer.boundary.part-minus-one";
    "transfer.boundary.part-exact";
    "transfer.boundary.part-plus-one";
    "transfer.boundary.multipart-two-or-more-parts";
  ]

let transfer_boundary_thresholds =
  List.map
    (fun bin -> Workload_coverage.threshold ~bin ~minimum:10)
    required_transfer_boundary_bins

let qcheck_seed_from_env () =
  match Sys.getenv_opt "QCHECK_SEED" with
  | Some value -> int_of_string_opt value
  | None -> None

let fresh_sample_seed () =
  Random.self_init ();
  Random.int 1_000_000_000

let coverage_sample_seed () =
  match qcheck_seed_from_env () with
  | Some seed -> seed
  | None -> fresh_sample_seed ()

let generated_coverage ~seed sample_count =
  let rand = Random.State.make [| seed |] in
  let rec loop remaining coverage =
    if remaining = 0 then coverage
    else
      let case = QCheck.Gen.generate1 ~rand upload_case_gen in
      loop (remaining - 1)
        (Workload_coverage.add_many (coverage_bins case) coverage)
  in
  loop sample_count Workload_coverage.empty

let test_generator_coverage () =
  let sample_count = 1_000 in
  let seed = coverage_sample_seed () in
  let coverage = generated_coverage ~seed sample_count in
  let label =
    Printf.sprintf
      "S3 transfer fault generator (seed=%d; reproduce with QCHECK_SEED=%d \
       opam exec -- dune build @s3-transfer)"
      seed seed
  in
  Workload_coverage.require_all ~label
    ~required:(required_transfer_bins @ required_transfer_boundary_bins)
    coverage;
  Workload_coverage.require_thresholds ~label ~sample_count
    ~thresholds:transfer_boundary_thresholds coverage

module Make (Target : TARGET) = struct
  let property =
    let count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:50 in
    QCheck.Test.make ~count
      ~name:(Target.name ^ " transfer faults preserve model")
      arbitrary Target.run_upload_case
    |> QCheck_alcotest.to_alcotest ~speed_level:`Quick

  let suite =
    [
      ( "workload:" ^ Target.name ^ ":transfer-faults",
        Alcotest.test_case "fault shrink excludes original" `Quick
          test_shrink_fault_excludes_original
        :: Alcotest.test_case "generator semantic coverage" `Quick
             test_generator_coverage
        :: List.map
             (fun (name, case) ->
               Alcotest.test_case name `Quick (fun () ->
                   Alcotest.(check bool) name true (Target.run_upload_case case)))
             deterministic_cases
        @ [ property ] );
    ]
end
