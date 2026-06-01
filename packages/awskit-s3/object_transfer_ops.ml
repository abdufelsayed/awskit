open Core

module Make
    (C : Operation_context.S)
    (Multipart :
      MULTIPART
        with type connection := C.connection
         and type 'a io := 'a C.R.t
         and type request_body := C.request_body) =
struct
  open C

  let ( let* ) = bind

  let ensure_part_count ~part_size ~length =
    if length = 0 then
      Error
        (Awskit.Error.validation ~field:"body"
           "object transfer upload requires a non-empty body")
    else
      let count = (length + part_size - 1) / part_size in
      if count > Transfer.max_parts then
        Error
          (Awskit.Error.validation ~field:"part_count"
             "object transfer upload would exceed 10000 parts")
      else Ok count

  let upload_string conn ~bucket ~key ?options body =
    let options = Option.value ~default:Transfer.default_options options in
    match Transfer.validate_options options with
    | Error error -> return_error error
    | Ok () -> (
        match
          ensure_part_count ~part_size:options.part_size
            ~length:(String.length body)
        with
        | Error error -> return_error error
        | Ok _ -> (
            let* created =
              Multipart.create_upload conn ~bucket ~key
                ~options:options.create_options ()
            in
            match created with
            | Error error -> return_error error
            | Ok created -> (
                let upload_id = created.upload.upload_id in
                let abort_and_return error =
                  let* _ =
                    Multipart.abort_upload conn ~bucket ~key ~upload_id
                  in
                  return_error error
                in
                let rec upload_parts offset part_number parts =
                  if offset >= String.length body then
                    return_ok (List.rev parts)
                  else
                    let length =
                      min options.part_size (String.length body - offset)
                    in
                    let part_body = String.sub body offset length in
                    let* uploaded =
                      Multipart.upload_part conn ~bucket ~key ~upload_id
                        ~part_number
                        ~body:(R.Request_body.of_string part_body)
                        ~options:options.upload_part_options ()
                    in
                    match uploaded with
                    | Error error -> abort_and_return error
                    | Ok uploaded ->
                        upload_parts (offset + length) (part_number + 1)
                          (uploaded.part :: parts)
                in
                let* parts = upload_parts 0 1 [] in
                match parts with
                | Error error -> return_error error
                | Ok parts -> (
                    let* completed =
                      Multipart.complete_upload conn ~bucket ~key ~upload_id
                        parts
                    in
                    match completed with
                    | Error error -> abort_and_return error
                    | Ok complete ->
                        return_ok
                          { Transfer.upload = created.upload; parts; complete })
                )))

  let upload_bytes conn ~bucket ~key ?options body =
    upload_string conn ~bucket ~key ?options (Bytes.to_string body)
end
