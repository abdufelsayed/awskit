type fault =
  | Read_fails_after of int
  | Write_fails_after of int
  | Multipart_create_fails
  | Multipart_part_fails of int
  | Multipart_complete_fails

type upload_case = {
  local_body : string;
  part_size : int;
  fault : fault option;
}

type expected_upload =
  | Upload_succeeds of { remote_body : string; progress_final : int }
  | Upload_fails of { remote_absent : bool; owned_upload_aborted : bool }

let min_part_size = 5 * 1024 * 1024

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
  | Read_fails_after offset | Write_fails_after offset ->
      byte_fault_reaches (String.length case.local_body) offset
  | Multipart_create_fails | Multipart_complete_fails -> uses_multipart case
  | Multipart_part_fails part_number ->
      uses_multipart case && part_number >= 1 && part_number <= part_count case

let owned_upload_aborted case = function
  | Multipart_create_fails -> false
  | Read_fails_after _ | Write_fails_after _ | Multipart_part_fails _
  | Multipart_complete_fails ->
      uses_multipart case

let expected_upload case =
  match case.fault with
  | Some fault when fault_reaches case fault ->
      Upload_fails
        {
          remote_absent = true;
          owned_upload_aborted = owned_upload_aborted case fault;
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
  | Multipart_create_fails -> "multipart-create-fails"
  | Multipart_part_fails n -> Printf.sprintf "multipart-part-fails:%d" n
  | Multipart_complete_fails -> "multipart-complete-fails"

let upload_case_to_string case =
  let body_len = String.length case.local_body in
  Printf.sprintf "len=%d part_size=%d parts=%d body_digest=%s fault=%s" body_len
    case.part_size (part_count case)
    (Digest.to_hex (Digest.string case.local_body))
    (Option.fold ~none:"none" ~some:fault_to_string case.fault)
