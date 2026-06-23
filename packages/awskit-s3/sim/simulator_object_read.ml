open Awskit_s3
open Simulator_support
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
open Simulator_inspect
open Simulator_object_body

let object_read_info (obj : stored_object) ~status ~content_length
    ~range_headers =
  let response =
    response status
      ~headers:
        ([
           ("etag", Object.Etag.to_string obj.etag);
           ("content-length", string_of_int content_length);
         ]
        @ range_headers
        @ version_headers obj.version_id
        @ checksum_response_headers obj.checksum)
  in
  info_of_object ~content_length response obj

let get_result (info : Get_object.info) value : _ Get_object.result =
  {
    Get_object.value;
    etag = info.etag;
    content_type = info.content_type;
    content_length = info.content_length;
    content_range = info.content_range;
    last_modified = info.last_modified;
    metadata = info.metadata;
    storage_class = info.storage_class;
    version_id = info.version_id;
    checksum = info.checksum;
    server_side_encryption = info.server_side_encryption;
    response = info.response;
  }

let read_object ?read_fault obj options ~consume =
  let* () = ensure_read_preconditions obj options.Get_object.preconditions in
  let* body, status, range_headers = ranged_body obj.body options.range in
  let info =
    object_read_info obj ~status ~content_length:(String.length body)
      ~range_headers
  in
  Result.map (get_result info)
    (Runtime.Response_body.with_reader
       (Runtime.response_body ?read_fault body)
       ~consume)

let get conn ~bucket ~key ?options ~consume () =
  let options = Option.value ~default:Get_object.default_options options in
  match validate_bucket_key bucket key with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok bucket_state -> (
          match current_or_version bucket_state key options.version_id with
          | None -> Error (no_such_key ())
          | Some (Stored_delete_marker marker) ->
              Error
                (delete_marker_error
                   ~current:(Option.is_none options.version_id)
                   marker)
          | Some (Stored_object obj) -> (
              match take_fault conn with
              | Some Response_lost ->
                  record ~faulted:true conn `Get_object bucket (Some key);
                  read_object obj options
                    ~read_fault:(fault_error Response_lost)
                    ~consume
              | Some fault ->
                  record ~faulted:true conn `Get_object bucket (Some key);
                  Error (fault_error fault)
              | None ->
                  record conn `Get_object bucket (Some key);
                  read_object obj options ~consume)))

let find conn ~bucket ~key ?options ~consume () =
  let options = Option.value ~default:Get_object.default_options options in
  let lookup_error error =
    if Error.is_no_such_key error then Ok None else Error error
  in
  match validate_bucket_key bucket key with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> lookup_error error
      | Ok bucket_state -> (
          match current_or_version bucket_state key options.version_id with
          | None -> Ok None
          | Some (Stored_delete_marker marker) ->
              lookup_error
                (delete_marker_error
                   ~current:(Option.is_none options.version_id)
                   marker)
          | Some (Stored_object obj) -> (
              match take_fault conn with
              | Some Response_lost ->
                  record ~faulted:true conn `Get_object bucket (Some key);
                  Result.map Option.some
                    (read_object obj options
                       ~read_fault:(fault_error Response_lost)
                       ~consume)
              | Some fault ->
                  record ~faulted:true conn `Get_object bucket (Some key);
                  Error (fault_error fault)
              | None ->
                  record conn `Get_object bucket (Some key);
                  Result.map Option.some (read_object obj options ~consume))))

let head conn ~bucket ~key ?options () =
  let options = Option.value ~default:Head_object.default_options options in
  match validate_bucket_key bucket key with
  | Error error -> Error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok bucket_state -> (
          match current_or_version bucket_state key options.version_id with
          | None -> Error (no_such_key ())
          | Some (Stored_delete_marker marker) ->
              Error
                (delete_marker_error
                   ~current:(Option.is_none options.version_id)
                   marker)
          | Some (Stored_object obj) -> (
              match operation_fault conn `Head_object bucket (Some key) with
              | Some error -> Error error
              | None -> (
                  match ensure_read_preconditions obj options.preconditions with
                  | Error error -> Error error
                  | Ok () ->
                      let response =
                        response 200
                          ~headers:
                            ([
                               ("etag", Object.Etag.to_string obj.etag);
                               ( "content-length",
                                 string_of_int (String.length obj.body) );
                             ]
                            @ version_headers obj.version_id
                            @ checksum_response_headers obj.checksum)
                      in
                      Ok (info_of_object response obj)))))

let exists conn ~bucket ~key =
  match head conn ~bucket ~key () with
  | Ok _ -> Ok true
  | Error error when Error.is_no_such_key error -> Ok false
  | Error error -> Error error

let get_as_string conn ~bucket ~key ~max_bytes ?options () =
  get conn ~bucket ~key ?options ~consume:(consume_string ~max_bytes) ()

let get_as_bytes conn ~bucket ~key ~max_bytes ?options () =
  Result.map
    (fun result ->
      {
        Get_object.value = Bytes.of_string result.Get_object.value;
        etag = result.etag;
        content_type = result.content_type;
        content_length = result.content_length;
        content_range = result.content_range;
        last_modified = result.last_modified;
        metadata = result.metadata;
        storage_class = result.storage_class;
        version_id = result.version_id;
        checksum = result.checksum;
        server_side_encryption = result.server_side_encryption;
        response = result.response;
      })
    (get_as_string conn ~bucket ~key ~max_bytes ?options ())
