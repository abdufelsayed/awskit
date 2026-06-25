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

let length_gen part_size =
  let open QCheck.Gen in
  oneof_weighted
    [
      (1, return 0);
      (2, int_range 1 4096);
      (2, int_range 4097 (128 * 1024));
      (2, return part_size);
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
  | Some Model.Multipart_create_fails -> "transfer.fault.multipart-create"
  | Some (Model.Multipart_part_fails _) -> "transfer.fault.multipart-part"
  | Some Model.Multipart_complete_fails -> "transfer.fault.multipart-complete"

let coverage_bins case = [ size_bin case; fault_bin case.Model.fault ]

let normalize_fault ~part_size ~length = function
  | None -> None
  | Some (Model.Read_fails_after offset) when length > 0 ->
      Some (Model.Read_fails_after (min offset (length - 1)))
  | Some (Model.Write_fails_after offset) when length > 0 ->
      Some (Model.Write_fails_after (min offset (length - 1)))
  | Some Model.Multipart_create_fails when length >= part_size ->
      Some Model.Multipart_create_fails
  | Some Model.Multipart_complete_fails when length >= part_size ->
      Some Model.Multipart_complete_fails
  | Some (Model.Multipart_part_fails part_number) when length >= part_size ->
      let part_count = part_count_for_length ~length ~part_size in
      Some (Model.Multipart_part_fails (min part_number part_count))
  | Some _ -> None

let fault_equal left right =
  match (left, right) with
  | Model.Read_fails_after left, Model.Read_fails_after right ->
      Int.equal left right
  | Model.Write_fails_after left, Model.Write_fails_after right ->
      Int.equal left right
  | Model.Multipart_create_fails, Model.Multipart_create_fails -> true
  | Model.Multipart_part_fails left, Model.Multipart_part_fails right ->
      Int.equal left right
  | Model.Multipart_complete_fails, Model.Multipart_complete_fails -> true
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
    | Some (Model.Multipart_part_fails part_number) ->
        [
          None;
          Some (Model.Multipart_part_fails 1);
          Some (Model.Multipart_part_fails (max 1 (part_number / 2)));
        ]
    | Some Model.Multipart_create_fails | Some Model.Multipart_complete_fails ->
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
      Some Model.Multipart_create_fails;
      Some (Model.Multipart_part_fails 1);
      Some (Model.Multipart_part_fails 2);
      Some Model.Multipart_complete_fails;
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
  unique_lengths (common @ multipart) length |> QCheck.Iter.of_list

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

let arbitrary =
  QCheck.make ~print:Model.upload_case_to_string ~shrink:shrink_case
    upload_case_gen

let required_transfer_bins =
  [
    "transfer.size.zero";
    "transfer.size.single";
    "transfer.size.exact-part";
    "transfer.size.multipart";
    "transfer.fault.none";
    "transfer.fault.read";
    "transfer.fault.write";
    "transfer.fault.multipart-create";
    "transfer.fault.multipart-part";
    "transfer.fault.multipart-complete";
  ]

let test_generator_coverage () =
  let cases = QCheck.Gen.generate ~n:1_000 upload_case_gen in
  let coverage = Workload_coverage.of_lists cases ~bins:coverage_bins in
  Workload_coverage.require_all ~label:"S3 transfer fault generator"
    ~required:required_transfer_bins coverage

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
