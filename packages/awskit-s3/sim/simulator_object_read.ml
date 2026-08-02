open Simulator_support
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
open Simulator_inspect
open Simulator_object_body
module Error = Awskit_s3.Error
module Object = Awskit_s3.Object
module Operation = Awskit_s3.Operation

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

let get_result (info : Object.Get.info) value : _ Object.Get.result =
  {
    Object.Get.value;
    etag = info.etag;
    content_type = info.content_type;
    content_length = info.content_length;
    content_range = info.content_range;
    last_modified = info.last_modified;
    metadata = info.metadata;
    storage_class = info.storage_class;
    version_id = info.version_id;
    checksum = info.checksum;
    encryption = info.encryption;
    response = info.response;
  }

let read_object ?read_fault obj options ~consume =
  let* () = ensure_read_preconditions obj options.Object.Get.preconditions in
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
  let options = Option.value ~default:Object.Get.default_options options in
  let return_error error =
    Error (with_operation Operation.Get_object ~bucket ~key error)
  in
  match validate_bucket_key bucket key with
  | Error error -> return_error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> return_error error
      | Ok bucket_state -> (
          match current_or_version bucket_state key options.version_id with
          | None -> return_error (no_such_key ())
          | Some (Stored_delete_marker marker) ->
              return_error
                (delete_marker_error
                   ~current:(Option.is_none options.version_id)
                   marker)
          | Some (Stored_object obj) -> (
              match take_fault conn with
              | Some Response_lost -> (
                  record ~faulted:true conn Operation.Get_object bucket
                    (Some key);
                  match
                    read_object obj options
                      ~read_fault:(fault_error Response_lost)
                      ~consume
                  with
                  | Error error -> return_error error
                  | Ok _ as ok -> ok)
              | Some fault ->
                  record ~faulted:true conn Operation.Get_object bucket
                    (Some key);
                  return_error (fault_error fault)
              | None -> (
                  record conn Operation.Get_object bucket (Some key);
                  match read_object obj options ~consume with
                  | Error error -> return_error error
                  | Ok _ as ok -> ok))))

let find conn ~bucket ~key ?options ~consume () =
  let options = Option.value ~default:Object.Get.default_options options in
  let return_error error =
    Error (with_operation Operation.Get_object ~bucket ~key error)
  in
  let lookup_error error =
    if Error.is_no_such_key error then Ok None else return_error error
  in
  match validate_bucket_key bucket key with
  | Error error -> return_error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> return_error error
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
              | Some Response_lost -> (
                  record ~faulted:true conn Operation.Get_object bucket
                    (Some key);
                  match
                    read_object obj options
                      ~read_fault:(fault_error Response_lost)
                      ~consume
                  with
                  | Error error -> return_error error
                  | Ok result -> Ok (Some result))
              | Some fault ->
                  record ~faulted:true conn Operation.Get_object bucket
                    (Some key);
                  return_error (fault_error fault)
              | None -> (
                  record conn Operation.Get_object bucket (Some key);
                  match read_object obj options ~consume with
                  | Error error -> return_error error
                  | Ok result -> Ok (Some result)))))

let head conn ~bucket ~key ?options () =
  let options = Option.value ~default:Object.Head.default_options options in
  let return_error error =
    Error (with_operation Operation.Head_object ~bucket ~key error)
  in
  match validate_bucket_key bucket key with
  | Error error -> return_error error
  | Ok () -> (
      match require_bucket conn bucket with
      | Error error -> return_error error
      | Ok bucket_state -> (
          match current_or_version bucket_state key options.version_id with
          | None -> return_error (no_such_key ())
          | Some (Stored_delete_marker marker) ->
              return_error
                (delete_marker_error
                   ~current:(Option.is_none options.version_id)
                   marker)
          | Some (Stored_object obj) -> (
              match
                operation_fault conn Operation.Head_object bucket (Some key)
              with
              | Some error -> return_error error
              | None -> (
                  match ensure_read_preconditions obj options.preconditions with
                  | Error error -> return_error error
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

let exists conn ~bucket ~key ?options () =
  match head conn ~bucket ~key ?options () with
  | Ok _ -> Ok true
  | Error error when Error.is_no_such_key error -> Ok false
  | Error error -> Error error
