type fault =
  | Read_fails_after of int
  | Write_fails_after of int
  | Progress_callback_raises_after of int
  | Cancellation_after_bytes of int
  | Multipart_create_fails
  | Multipart_part_fails of int
  | Multipart_complete_fails
  | Cleanup_delete_fails

type upload_case = {
  local_body : string;
  part_size : int;
  fault : fault option;
}

type expected_upload =
  | Upload_succeeds of { remote_body : string; progress_final : int }
  | Upload_fails of {
      remote_absent : bool;
      owned_upload_aborted : bool;
      read_bytes : int;
      written_bytes : int;
      progress_bytes : int;
      progress_report_max : int;
      callback_exception_preserved : bool;
      cleanup_error_secondary : bool;
      cancelled : bool;
    }

let min_part_size = 5 * 1024 * 1024
let chunk_size = 64 * 1024

let part_count_for_length ~length ~part_size =
  if length = 0 then 0 else ((length - 1) / part_size) + 1

let part_count case =
  part_count_for_length
    ~length:(String.length case.local_body)
    ~part_size:case.part_size

let uses_multipart case =
  String.length case.local_body > 0
  && String.length case.local_body >= case.part_size

let byte_fault_reaches length offset = offset >= 0 && offset < length

let fault_reaches case = function
  | Read_fails_after offset
  | Write_fails_after offset
  | Progress_callback_raises_after offset
  | Cancellation_after_bytes offset ->
      byte_fault_reaches (String.length case.local_body) offset
  | Multipart_create_fails | Multipart_complete_fails -> uses_multipart case
  | Multipart_part_fails part_number ->
      uses_multipart case && part_number >= 1 && part_number <= part_count case
  | Cleanup_delete_fails -> uses_multipart case

let owned_upload_aborted case = function
  | Multipart_create_fails -> false
  | Cleanup_delete_fails -> false
  | Read_fails_after _ | Write_fails_after _ | Progress_callback_raises_after _
  | Cancellation_after_bytes _ | Multipart_part_fails _
  | Multipart_complete_fails ->
      uses_multipart case

let part_lengths case =
  let length = String.length case.local_body in
  if not (uses_multipart case) then [ length ]
  else
    let rec loop offset acc =
      if offset >= length then List.rev acc
      else
        let part_length = min case.part_size (length - offset) in
        loop (offset + part_length) (part_length :: acc)
    in
    loop 0 []

let completed_part_bytes_before_offset case offset =
  if not (uses_multipart case) then 0
  else
    min
      (String.length case.local_body)
      (offset / case.part_size * case.part_size)

let bytes_before_part case part_number =
  if part_number <= 1 then 0
  else min (String.length case.local_body) ((part_number - 1) * case.part_size)

let completed_part_bytes_after_offset case offset =
  if not (uses_multipart case) then
    min
      (String.length case.local_body)
      (((offset / chunk_size) + 1) * chunk_size)
  else
    min
      (String.length case.local_body)
      (((offset / case.part_size) + 1) * case.part_size)

let completed_progress_bytes_before_offset case offset =
  if uses_multipart case then completed_part_bytes_before_offset case offset
  else min (String.length case.local_body) (offset / chunk_size * chunk_size)

let write_fault_counts case limit =
  let rec chunks read_bytes written_bytes progress_bytes part_remaining =
    if part_remaining = 0 then
      `Part_complete (read_bytes, written_bytes, progress_bytes)
    else
      let length = min chunk_size part_remaining in
      let read_bytes = read_bytes + length in
      if written_bytes >= limit then
        `Failed (read_bytes, written_bytes, progress_bytes)
      else
        let allowed = min length (limit - written_bytes) in
        let written_bytes = written_bytes + allowed in
        if allowed < length then
          `Failed (read_bytes, written_bytes, progress_bytes)
        else
          chunks read_bytes written_bytes progress_bytes
            (part_remaining - length)
  in
  let rec parts read_bytes written_bytes progress_bytes = function
    | [] -> (read_bytes, written_bytes, progress_bytes)
    | part_length :: rest -> (
        match chunks read_bytes written_bytes progress_bytes part_length with
        | `Failed counts -> counts
        | `Part_complete (read_bytes, written_bytes, progress_bytes) ->
            let progress_bytes =
              if uses_multipart case then progress_bytes + part_length
              else progress_bytes
            in
            parts read_bytes written_bytes progress_bytes rest)
  in
  parts 0 0 0 (part_lengths case)

let failure_counts case = function
  | Read_fails_after offset ->
      let progress_bytes = completed_part_bytes_before_offset case offset in
      (offset, offset, progress_bytes)
  | Cancellation_after_bytes offset ->
      let progress_bytes = completed_part_bytes_before_offset case offset in
      (offset, offset, progress_bytes)
  | Write_fails_after offset -> write_fault_counts case offset
  | Progress_callback_raises_after offset ->
      let bytes = completed_part_bytes_after_offset case offset in
      let progress_bytes = completed_progress_bytes_before_offset case offset in
      (bytes, bytes, progress_bytes)
  | Multipart_create_fails -> (0, 0, 0)
  | Multipart_part_fails part_number ->
      let bytes = bytes_before_part case part_number in
      (bytes, bytes, bytes)
  | Multipart_complete_fails | Cleanup_delete_fails ->
      let bytes = String.length case.local_body in
      (bytes, bytes, bytes)

let failure_progress_report_max case = function
  | Progress_callback_raises_after offset ->
      completed_part_bytes_after_offset case offset
  | ( Read_fails_after _ | Write_fails_after _ | Cancellation_after_bytes _
    | Multipart_create_fails | Multipart_part_fails _ | Multipart_complete_fails
    | Cleanup_delete_fails ) as fault ->
      let _read_bytes, _written_bytes, progress_bytes =
        failure_counts case fault
      in
      progress_bytes

let callback_exception_preserved = function
  | Progress_callback_raises_after _ -> true
  | Read_fails_after _ | Write_fails_after _ | Cancellation_after_bytes _
  | Multipart_create_fails | Multipart_part_fails _ | Multipart_complete_fails
  | Cleanup_delete_fails ->
      false

let cleanup_error_secondary = function
  | Cleanup_delete_fails -> true
  | Read_fails_after _ | Write_fails_after _ | Progress_callback_raises_after _
  | Cancellation_after_bytes _ | Multipart_create_fails | Multipart_part_fails _
  | Multipart_complete_fails ->
      false

let cancelled = function
  | Cancellation_after_bytes _ -> true
  | Read_fails_after _ | Write_fails_after _ | Progress_callback_raises_after _
  | Multipart_create_fails | Multipart_part_fails _ | Multipart_complete_fails
  | Cleanup_delete_fails ->
      false

let expected_upload case =
  match case.fault with
  | Some fault when fault_reaches case fault ->
      let read_bytes, written_bytes, progress_bytes =
        failure_counts case fault
      in
      Upload_fails
        {
          remote_absent = true;
          owned_upload_aborted = owned_upload_aborted case fault;
          read_bytes;
          written_bytes;
          progress_bytes;
          progress_report_max = failure_progress_report_max case fault;
          callback_exception_preserved = callback_exception_preserved fault;
          cleanup_error_secondary = cleanup_error_secondary fault;
          cancelled = cancelled fault;
        }
  | None | Some _ ->
      Upload_succeeds
        {
          remote_body = case.local_body;
          progress_final = String.length case.local_body;
        }

let fault_to_string = function
  | Read_fails_after n -> Printf.sprintf "read-fails-after:%d" n
  | Write_fails_after n -> Printf.sprintf "write-fails-after:%d" n
  | Progress_callback_raises_after n ->
      Printf.sprintf "progress-callback-raises-after:%d" n
  | Cancellation_after_bytes n -> Printf.sprintf "cancellation-after-bytes:%d" n
  | Multipart_create_fails -> "multipart-create-fails"
  | Multipart_part_fails n -> Printf.sprintf "multipart-part-fails:%d" n
  | Multipart_complete_fails -> "multipart-complete-fails"
  | Cleanup_delete_fails -> "cleanup-delete-fails"

let upload_case_to_string case =
  let body_len = String.length case.local_body in
  Printf.sprintf "len=%d part_size=%d parts=%d body_digest=%s fault=%s" body_len
    case.part_size (part_count case)
    (Digest.to_hex (Digest.string case.local_body))
    (Option.fold ~none:"none" ~some:fault_to_string case.fault)
