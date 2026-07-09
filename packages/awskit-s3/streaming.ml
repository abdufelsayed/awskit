module Make (R : Awskit.Runtime.S) = struct
  let ( let* ) = R.IO.bind

  module Body = struct
    type 'a io = 'a R.t
    type t = R.request_body
    type writer = R.request_body_writer

    let empty = R.Request_body.empty
    let of_string = R.Request_body.of_string
    let of_bytes = R.Request_body.of_bytes
    let content_length body = (R.Request_body.descriptor body).content_length
    let replayable body = (R.Request_body.descriptor body).replayable

    module Writer = struct
      type t = writer

      let write_string = R.Request_body.write_string
      let write_bytes = R.Request_body.write_bytes
      let write_subbytes = R.Request_body.write_subbytes
    end

    let of_stream ~content_length ~replayable ~write =
      match
        Awskit.Body.Request.descriptor ~content_length
          ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable ()
      with
      | Error _ as error -> error
      | Ok descriptor -> Ok (R.Request_body.of_stream descriptor ~write)
  end

  module Reader = struct
    type 'a io = 'a R.t
    type t = R.response_body_reader

    let default_chunk_size = 128 * 1024
    let read = R.Response_body.read

    let invalid_chunk_size chunk_size =
      Awskit.Error.Producer.validation ~field:"chunk_size"
        (Fmt.str "chunk_size must be positive, got %d" chunk_size)

    let invalid_max_bytes max_bytes =
      Awskit.Error.Producer.validation ~field:"max_bytes"
        (Fmt.str "max_bytes must be non-negative, got %Ld" max_bytes)

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

    let check_limit ~max_bytes total =
      if Int64.compare total max_bytes > 0 then
        Error
          (Awskit.Error.Producer.body ~limit:max_bytes
             "response body exceeded max_bytes")
      else Ok ()

    let check_bytes_allocation total =
      let limit = Int64.of_int Sys.max_string_length in
      if Int64.compare total limit > 0 then
        Error
          (Awskit.Error.Producer.body ~limit
             "response body exceeded maximum in-memory bytes allocation")
      else Ok ()

    let drain_to_buffer ?chunk_size ~max_bytes reader =
      if Int64.compare max_bytes 0L < 0 then
        R.IO.return (Error (invalid_max_bytes max_bytes))
      else
        let buffer = Buffer.create 4096 in
        let* result =
          fold ?chunk_size reader ~init:0L ~f:(fun total chunk ->
              let total = Int64.add total (Int64.of_int (Bytes.length chunk)) in
              match check_limit ~max_bytes total with
              | Error _ as error -> R.IO.return error
              | Ok () ->
                  Buffer.add_bytes buffer chunk;
                  R.IO.return (Ok total))
        in
        match result with
        | Error _ as error -> R.IO.return error
        | Ok _ -> R.IO.return (Ok buffer)

    let drain_to_chunks ?chunk_size ~max_bytes reader =
      if Int64.compare max_bytes 0L < 0 then
        R.IO.return (Error (invalid_max_bytes max_bytes))
      else
        fold ?chunk_size reader ~init:(0L, []) ~f:(fun total_chunks chunk ->
            let total, chunks = total_chunks in
            let total = Int64.add total (Int64.of_int (Bytes.length chunk)) in
            match check_limit ~max_bytes total with
            | Error _ as error -> R.IO.return error
            | Ok () -> (
                match check_bytes_allocation total with
                | Error _ as error -> R.IO.return error
                | Ok () -> R.IO.return (Ok (total, chunk :: chunks))))

    let chunks_to_bytes total chunks =
      let bytes = Bytes.create (Int64.to_int total) in
      let rec copy offset = function
        | [] -> bytes
        | chunk :: chunks ->
            let len = Bytes.length chunk in
            Bytes.blit chunk 0 bytes offset len;
            copy (offset + len) chunks
      in
      copy 0 (List.rev chunks)

    let to_bytes ?chunk_size ~max_bytes reader =
      let* result = drain_to_chunks ?chunk_size ~max_bytes reader in
      match result with
      | Error _ as error -> R.IO.return error
      | Ok (total, chunks) -> R.IO.return (Ok (chunks_to_bytes total chunks))

    let to_string ?chunk_size ~max_bytes reader =
      let* result = drain_to_buffer ?chunk_size ~max_bytes reader in
      match result with
      | Error _ as error -> R.IO.return error
      | Ok buffer -> R.IO.return (Ok (Buffer.contents buffer))
  end
end
