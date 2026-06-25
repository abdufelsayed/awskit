module Model = Transfer_model
module Simulator = Awskit_s3_sim
module Bucket_name = Awskit_s3.Bucket_name
module Multipart = Awskit_s3.Multipart
module Object_key = Awskit_s3.Object_key
module Transfer = Awskit_s3.Transfer

let test_time = Ptime.epoch

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let bucket = Bucket_name.of_string_exn "transfer-fault-workload-bucket"
let key = Object_key.of_string_exn "transfer.bin"
let key_string = Object_key.to_string key
let chunk_size = Model.chunk_size

type counters = {
  mutable read_bytes : int;
  mutable written_bytes : int;
  mutable progress_bytes : int;
  mutable created_upload : Multipart.Upload.created Multipart.Upload.t option;
}

let counters () =
  {
    read_bytes = 0;
    written_bytes = 0;
    progress_bytes = 0;
    created_upload = None;
  }

let fail case message =
  QCheck.Test.fail_reportf "%s: %s" (Model.upload_case_to_string case) message

let failf case format =
  Printf.ksprintf (fun message -> fail case message) format

let error_to_string error = Awskit.Error.to_string_hum error

let expect_ok case label = function
  | Ok value -> value
  | Error error -> failf case "%s failed: %s" label (error_to_string error)

let expect_error case label = function
  | Error error -> error
  | Ok _ -> failf case "%s unexpectedly succeeded" label

let check_bool case label expected actual =
  if Bool.equal expected actual then ()
  else failf case "%s expected %b, got %b" label expected actual

let check_int case label expected actual =
  if Int.equal expected actual then ()
  else failf case "%s expected %d, got %d" label expected actual

let check_string case label expected actual =
  if String.equal expected actual then ()
  else
    failf case "%s expected digest %s, got digest %s" label
      (Digest.to_hex (Digest.string expected))
      (Digest.to_hex (Digest.string actual))

let operation_is op record =
  match (op, record.Simulator.op) with
  | `Create_multipart_upload, `Create_multipart_upload
  | `Upload_part, `Upload_part
  | `Complete_multipart_upload, `Complete_multipart_upload
  | `Abort_multipart_upload, `Abort_multipart_upload ->
      true
  | _ -> false

let operation_name = function
  | `Create_multipart_upload -> "CreateMultipartUpload"
  | `Upload_part -> "UploadPart"
  | `Complete_multipart_upload -> "CompleteMultipartUpload"
  | `Abort_multipart_upload -> "AbortMultipartUpload"

let count_operations op history =
  List.fold_left
    (fun count record -> if operation_is op record then count + 1 else count)
    0 history

let has_faulted_operation op history =
  List.exists
    (fun record -> record.Simulator.faulted && operation_is op record)
    history

let check_fault_history case store =
  let history = Simulator.history store in
  let expect_faulted op =
    check_bool case
      (operation_name op ^ " consumed injected fault")
      true
      (has_faulted_operation op history)
  in
  match case.Model.fault with
  | Some Model.Multipart_create_fails -> expect_faulted `Create_multipart_upload
  | Some (Model.Multipart_part_fails _) -> expect_faulted `Upload_part
  | Some Model.Multipart_complete_fails ->
      expect_faulted `Complete_multipart_upload
  | None | Some (Read_fails_after _ | Write_fails_after _) -> ()

let body_error message = Awskit.Error.Producer.body message

let write_chunk _case counts writer chunk =
  match Simulator.Body.Writer.write_string writer chunk with
  | Error _ as error -> error
  | Ok () ->
      counts.written_bytes <- counts.written_bytes + String.length chunk;
      Ok ()

let write_with_read_fault case counts writer ~offset ~local_offset ~length limit
    =
  if counts.read_bytes >= limit then
    Error (body_error "simulated transfer source read failure")
  else
    let allowed = min length (limit - counts.read_bytes) in
    let chunk =
      String.sub case.Model.local_body (offset + local_offset) allowed
    in
    counts.read_bytes <- counts.read_bytes + allowed;
    match write_chunk case counts writer chunk with
    | Error _ as error -> error
    | Ok () ->
        if allowed < length then
          Error (body_error "simulated transfer source read failure")
        else Ok ()

let write_with_write_fault case counts writer ~offset ~local_offset ~length
    limit =
  let chunk = String.sub case.Model.local_body (offset + local_offset) length in
  counts.read_bytes <- counts.read_bytes + length;
  if counts.written_bytes >= limit then
    Error (body_error "simulated transfer request write failure")
  else
    let allowed = min length (limit - counts.written_bytes) in
    let chunk = String.sub chunk 0 allowed in
    match write_chunk case counts writer chunk with
    | Error _ as error -> error
    | Ok () ->
        if allowed < length then
          Error (body_error "simulated transfer request write failure")
        else Ok ()

let write_without_fault case counts writer ~offset ~local_offset ~length =
  let chunk = String.sub case.Model.local_body (offset + local_offset) length in
  counts.read_bytes <- counts.read_bytes + length;
  write_chunk case counts writer chunk

let request_body case counts ~offset ~length =
  let write writer =
    let rec loop local_offset =
      if local_offset >= length then Ok ()
      else
        let chunk_length = min chunk_size (length - local_offset) in
        let result =
          match case.Model.fault with
          | Some (Model.Read_fails_after limit) ->
              write_with_read_fault case counts writer ~offset ~local_offset
                ~length:chunk_length limit
          | Some (Model.Write_fails_after limit) ->
              write_with_write_fault case counts writer ~offset ~local_offset
                ~length:chunk_length limit
          | None
          | Some
              ( Model.Multipart_create_fails | Multipart_part_fails _
              | Multipart_complete_fails ) ->
              write_without_fault case counts writer ~offset ~local_offset
                ~length:chunk_length
        in
        match result with
        | Error _ as error -> error
        | Ok () -> loop (local_offset + chunk_length)
    in
    loop 0
  in
  expect_ok case "build request body"
    (Simulator.Body.of_stream ~content_length:(Int64.of_int length)
       ~replayable:true ~write)

let create_connection case =
  let clock = Simulator.Clock.create ~now:test_time () in
  let store = Simulator.create_store ~clock () in
  let conn = Simulator.connect store ~credentials in
  ignore
    (expect_ok case "create bucket" (Simulator.Bucket.create conn ~bucket ())
      : Awskit_s3.Bucket.Create.result);
  (store, conn)

let upload_single case counts conn =
  let length = String.length case.Model.local_body in
  let body = request_body case counts ~offset:0 ~length in
  match Simulator.Object.put conn ~bucket ~key ~body () with
  | Error _ as error -> error
  | Ok _ ->
      counts.progress_bytes <- length;
      Ok ()

let inject_fault_for_create conn = function
  | Some Model.Multipart_create_fails ->
      Simulator.inject_fault conn Simulator.Internal_error
  | _ -> ()

let inject_fault_for_part conn fault part_number =
  match fault with
  | Some (Model.Multipart_part_fails failed_part)
    when Int.equal failed_part part_number ->
      Simulator.inject_fault conn Simulator.Internal_error
  | _ -> ()

let inject_fault_for_complete conn = function
  | Some Model.Multipart_complete_fails ->
      Simulator.inject_fault conn Simulator.Internal_error
  | _ -> ()

let abort_upload case conn upload =
  ignore
    (expect_ok case "abort multipart upload"
       (Simulator.Multipart.abort_upload conn ~upload ())
      : Multipart.Abort.result)

let upload_multipart case counts conn =
  let content_length = String.length case.Model.local_body in
  let parts =
    expect_ok case "plan multipart upload"
      (Transfer.Plan.upload_parts
         ~content_length:(Int64.of_int content_length)
         ~part_size:case.Model.part_size)
  in
  inject_fault_for_create conn case.Model.fault;
  match Simulator.Multipart.create_upload conn ~bucket ~key () with
  | Error _ as error -> error
  | Ok created -> (
      counts.created_upload <- Some created.upload;
      let rec upload_parts completed = function
        | [] -> Ok (List.rev completed)
        | (spec : Transfer.Plan.upload_part) :: rest -> (
            let part_number = Multipart.Part_number.to_int spec.part_number in
            inject_fault_for_part conn case.Model.fault part_number;
            let body =
              request_body case counts ~offset:(Int64.to_int spec.offset)
                ~length:spec.length
            in
            match
              Simulator.Multipart.upload_part conn ~upload:created.upload
                ~part_number:spec.part_number ~body ()
            with
            | Error error ->
                abort_upload case conn created.upload;
                Error error
            | Ok uploaded ->
                counts.progress_bytes <- counts.progress_bytes + spec.length;
                upload_parts (uploaded.part :: completed) rest)
      in
      match upload_parts [] parts with
      | Error _ as error -> error
      | Ok completed -> (
          inject_fault_for_complete conn case.Model.fault;
          match
            Simulator.Multipart.complete_upload conn ~upload:created.upload
              ~parts:completed ()
          with
          | Ok _ -> Ok ()
          | Error error ->
              abort_upload case conn created.upload;
              Error error))

let run_transfer case counts conn =
  if Model.uses_multipart case then upload_multipart case counts conn
  else upload_single case counts conn

let stored_body case store =
  match Simulator.objects_as_strings store ~bucket with
  | [] -> None
  | [ (stored_key, body) ] when String.equal stored_key key_string -> Some body
  | objects ->
      failf case "unexpected stored objects count=%d" (List.length objects)

let check_upload_cleaned_up case conn counts =
  match counts.created_upload with
  | None -> fail case "expected created upload handle for cleanup check"
  | Some upload -> (
      match
        Simulator.Multipart.List_parts.parts conn ~upload ~max_pages:1 ()
      with
      | Error error when Awskit.Error.service_code error = Some "NoSuchUpload"
        ->
          ()
      | Error error ->
          failf case "list parts after abort returned %s"
            (error_to_string error)
      | Ok _ -> fail case "aborted multipart upload still accepts ListParts")

let verify_success case store counts ~remote_body ~progress_final =
  check_string case "remote body" remote_body
    (Option.value ~default:"<absent>" (stored_body case store));
  check_int case "read bytes" progress_final counts.read_bytes;
  check_int case "written bytes" progress_final counts.written_bytes;
  check_int case "progress final" progress_final counts.progress_bytes

let verify_failure case store conn counts ~remote_absent ~owned_upload_aborted
    ~read_bytes ~written_bytes ~progress_bytes =
  if remote_absent then
    check_bool case "remote object absent" true
      (Option.is_none (stored_body case store));
  check_int case "read bytes after failure" read_bytes counts.read_bytes;
  check_int case "written bytes after failure" written_bytes
    counts.written_bytes;
  check_int case "progress bytes after failure" progress_bytes
    counts.progress_bytes;
  let history = Simulator.history store in
  check_fault_history case store;
  if owned_upload_aborted then (
    check_int case "abort count" 1
      (count_operations `Abort_multipart_upload history);
    check_upload_cleaned_up case conn counts)
  else
    check_int case "abort count" 0
      (count_operations `Abort_multipart_upload history)

module Target = struct
  let name = "awskit-s3-sim"

  let run_upload_case case =
    let store, conn = create_connection case in
    let counts = counters () in
    let result = run_transfer case counts conn in
    match Model.expected_upload case with
    | Model.Upload_succeeds { remote_body; progress_final } ->
        ignore (expect_ok case "transfer" result : unit);
        verify_success case store counts ~remote_body ~progress_final;
        true
    | Model.Upload_fails
        {
          remote_absent;
          owned_upload_aborted;
          read_bytes;
          written_bytes;
          progress_bytes;
        } ->
        ignore (expect_error case "transfer" result : Awskit.Error.t);
        verify_failure case store conn counts ~remote_absent
          ~owned_upload_aborted ~read_bytes ~written_bytes ~progress_bytes;
        true
end

module Workload = Transfer_fault_workload.Make (Target)

let suite = Workload.suite
