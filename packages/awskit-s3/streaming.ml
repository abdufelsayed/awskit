module Make (R : Awskit_s3_intf.RUNTIME) = struct
  let ( let* ) = R.IO.bind

  module Body = struct
    type 'a io = 'a R.t
    type t = R.request_body
    type writer = R.request_body_writer

    let empty = R.Request_body.empty
    let of_string = R.Request_body.of_string
    let of_bytes = R.Request_body.of_bytes
    let content_length body = (R.Request_body.descriptor body).content_length

    module Writer = struct
      type t = writer

      let write_string = R.Request_body.write_string

      let write_bytes writer bytes =
        R.Request_body.write_string writer (Bytes.to_string bytes)
    end

    let of_stream ~content_length ~write =
      let descriptor =
        {
          Awskit.Body.Request.content_length = Some content_length;
          payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
          replayable = false;
        }
      in
      R.Request_body.of_stream descriptor ~write
  end

  module Reader = struct
    type 'a io = 'a R.t
    type t = R.response_body_reader

    let default_chunk_size = 128 * 1024
    let read = R.Response_body.read

    let invalid_chunk_size chunk_size =
      Awskit.Error.Internal.validation ~field:"chunk_size"
        (Fmt.str "chunk_size must be positive, got %d" chunk_size)

    let next ?(chunk_size = default_chunk_size) reader =
      if chunk_size <= 0 then
        R.IO.return (Error (invalid_chunk_size chunk_size))
      else
        let bytes = Bytes.create chunk_size in
        let* read = R.Response_body.read reader bytes ~off:0 ~len:chunk_size in
        match read with
        | Error _ as error -> R.IO.return error
        | Ok 0 -> R.IO.return (Ok None)
        | Ok n ->
            let chunk = if n = chunk_size then bytes else Bytes.sub bytes 0 n in
            R.IO.return (Ok (Some chunk))

    let fold ?chunk_size reader ~init ~f =
      let rec loop acc =
        let* chunk = next ?chunk_size reader in
        match chunk with
        | Error _ as error -> R.IO.return error
        | Ok None -> R.IO.return (Ok acc)
        | Ok (Some chunk) -> (
            let* folded = f acc chunk in
            match folded with
            | Error _ as error -> R.IO.return error
            | Ok acc -> loop acc)
      in
      loop init

    let iter ?chunk_size reader ~f =
      fold ?chunk_size reader ~init:() ~f:(fun () chunk -> f chunk)

    let check_limit ?max_bytes total =
      match max_bytes with
      | Some limit when Int64.compare total limit > 0 ->
          Error
            (Awskit.Error.Internal.body ~limit
               "response body exceeded max_bytes")
      | _ -> Ok ()

    let drain_to_buffer ?chunk_size ?max_bytes reader =
      let buffer = Buffer.create 4096 in
      let* result =
        fold ?chunk_size reader ~init:0L ~f:(fun total chunk ->
            let total = Int64.add total (Int64.of_int (Bytes.length chunk)) in
            match check_limit ?max_bytes total with
            | Error _ as error -> R.IO.return error
            | Ok () ->
                Buffer.add_bytes buffer chunk;
                R.IO.return (Ok total))
      in
      match result with
      | Error _ as error -> R.IO.return error
      | Ok _ -> R.IO.return (Ok buffer)

    let to_bytes ?chunk_size ?max_bytes reader =
      let* result = drain_to_buffer ?chunk_size ?max_bytes reader in
      match result with
      | Error _ as error -> R.IO.return error
      | Ok buffer -> R.IO.return (Ok (Buffer.to_bytes buffer))

    let to_string ?chunk_size ?max_bytes reader =
      let* result = drain_to_buffer ?chunk_size ?max_bytes reader in
      match result with
      | Error _ as error -> R.IO.return error
      | Ok buffer -> R.IO.return (Ok (Buffer.contents buffer))
  end
end
