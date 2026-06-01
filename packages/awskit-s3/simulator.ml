module Public = struct
  include Sim_support
  module Multipart = Sim_multipart.Multipart

  module Object = struct
    include Sim_object.Object

    module Transfer = struct
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
        | Error error -> Error error
        | Ok () -> (
            match
              ensure_part_count ~part_size:options.part_size
                ~length:(String.length body)
            with
            | Error error -> Error error
            | Ok _ -> (
                match
                  Multipart.create_upload conn ~bucket ~key
                    ~options:options.create_options ()
                with
                | Error error -> Error error
                | Ok created -> (
                    let upload_id = created.upload.upload_id in
                    let abort_and_return error =
                      ignore
                        (Multipart.abort_upload conn ~bucket ~key ~upload_id);
                      Error error
                    in
                    let rec upload_parts offset part_number parts =
                      if offset >= String.length body then Ok (List.rev parts)
                      else
                        let length =
                          min options.part_size (String.length body - offset)
                        in
                        let part_body = String.sub body offset length in
                        match
                          Multipart.upload_part conn ~bucket ~key ~upload_id
                            ~part_number
                            ~body:(Runtime.Request_body.of_string part_body)
                            ~options:options.upload_part_options ()
                        with
                        | Error error -> abort_and_return error
                        | Ok uploaded ->
                            upload_parts (offset + length) (part_number + 1)
                              (uploaded.part :: parts)
                    in
                    match upload_parts 0 1 [] with
                    | Error error -> Error error
                    | Ok parts -> (
                        match
                          Multipart.complete_upload conn ~bucket ~key ~upload_id
                            parts
                        with
                        | Error error -> abort_and_return error
                        | Ok complete ->
                            Ok
                              {
                                Transfer.upload = created.upload;
                                parts;
                                complete;
                              }))))

      let upload_bytes conn ~bucket ~key ?options body =
        upload_string conn ~bucket ~key ?options (Bytes.to_string body)
    end
  end

  module Bucket = Sim_bucket.Bucket
  module Presigned = Sim_presigned.Presigned
end
