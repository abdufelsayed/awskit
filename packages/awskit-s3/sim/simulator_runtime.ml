open Simulator_support
open Simulator_state

module Runtime = struct
  type connection = Simulator_state.t
  type 'a t = 'a

  type request_body = {
    descriptor : Awskit.Body.Request.descriptor;
    body : (string, Awskit.Error.t) result;
  }

  type response_body = { body : string; read_fault : Awskit.Error.t option }

  type request_body_writer = {
    buffer : Buffer.t;
    remaining : int64 option ref;
    mutable write_error : Awskit.Error.t option;
  }

  type response_body_reader = {
    body : string;
    mutable offset : int;
    mutable read_fault : Awskit.Error.t option;
    mutable active : bool;
  }

  let descriptor_for_string body =
    Awskit.Body.Request.descriptor_exn
      ~content_length:(Int64.of_int (String.length body))
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
      ~replayable:true ()

  let empty_request_body =
    { descriptor = descriptor_for_string ""; body = Ok "" }

  let string_request_body value =
    { descriptor = descriptor_for_string value; body = Ok value }

  let bytes_request_body value = string_request_body (Bytes.to_string value)
  let body_error message = Awskit.Error.Producer.body message

  let writer_for descriptor =
    {
      buffer = Buffer.create 1024;
      remaining = ref descriptor.Awskit.Body.Request.content_length;
      write_error = None;
    }

  let check_write_length writer value =
    match !(writer.remaining) with
    | None -> Ok ()
    | Some remaining ->
        let length = Int64.of_int (String.length value) in
        if Stdlib.Int64.compare length remaining > 0 then
          Error (body_error "request body exceeded declared content_length")
        else (
          writer.remaining := Some (Stdlib.Int64.sub remaining length);
          Ok ())

  let check_finished_length writer =
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match !(writer.remaining) with
        | None | Some 0L -> Ok (Buffer.contents writer.buffer)
        | Some _ ->
            Error
              (body_error "request body ended before declared content_length"))

  let stream_request_body descriptor ~write =
    let writer = writer_for descriptor in
    let body =
      match write writer with
      | Ok () -> check_finished_length writer
      | Error _ as error -> error
    in
    { descriptor; body }

  let request_body_descriptor (body : request_body) = body.descriptor
  let request_body_result (body : request_body) = body.body

  let write_request_body_string writer value =
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match check_write_length writer value with
        | Error error ->
            writer.write_error <- Some error;
            Error error
        | Ok () ->
            Buffer.add_string writer.buffer value;
            Ok ())

  let read_response_body (reader : response_body_reader) bytes ~off ~len =
    let bytes_length = Bytes.length bytes in
    if not reader.active then
      Error (body_error "response body reader used outside its scope")
    else if off < 0 || len < 0 || off > bytes_length - len then
      Error (body_error "response body read bounds are invalid")
    else
      match reader.read_fault with
      | Some error ->
          reader.read_fault <- None;
          reader.active <- false;
          Error error
      | None ->
          if len = 0 then Ok 0
          else
            let remaining = String.length reader.body - reader.offset in
            if remaining <= 0 then Ok 0
            else
              let copied = min len remaining in
              String.blit reader.body reader.offset bytes off copied;
              reader.offset <- reader.offset + copied;
              Ok copied

  let response_body ?read_fault body : response_body = { body; read_fault }
  let close_response_body_reader reader = reader.active <- false

  let discard_reader reader =
    let buffer = Bytes.create 8192 in
    let rec loop () =
      match
        read_response_body reader buffer ~off:0 ~len:(Bytes.length buffer)
      with
      | Error _ as error -> error
      | Ok 0 -> Ok ()
      | Ok _ -> loop ()
    in
    loop ()

  let with_response_body (body : response_body) ~consume =
    let reader =
      {
        body = body.body;
        offset = 0;
        read_fault = body.read_fault;
        active = true;
      }
    in
    match consume reader with
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        ignore (discard_reader reader : (unit, Awskit.Error.t) result);
        close_response_body_reader reader;
        Printexc.raise_with_backtrace exn backtrace
    | Ok _ as result -> (
        match discard_reader reader with
        | Ok () ->
            close_response_body_reader reader;
            result
        | Error _ as error ->
            close_response_body_reader reader;
            error)
    | Error _ as error -> (
        match discard_reader reader with
        | Ok () | Error _ ->
            close_response_body_reader reader;
            error)

  let discard_response_body (body : response_body) =
    let reader =
      {
        body = body.body;
        offset = 0;
        read_fault = body.read_fault;
        active = true;
      }
    in
    let result = discard_reader reader in
    close_response_body_reader reader;
    result

  module Request_body = struct
    type 'a io = 'a
    type t = request_body
    type writer = request_body_writer

    let empty = empty_request_body
    let of_string = string_request_body
    let of_bytes = bytes_request_body
    let of_stream = stream_request_body
    let descriptor = request_body_descriptor
    let content_length body = (request_body_descriptor body).content_length
    let write_string = write_request_body_string

    let write_bytes writer bytes =
      write_request_body_string writer (Bytes.to_string bytes)

    let write_subbytes writer bytes ~off ~len =
      let bytes_length = Bytes.length bytes in
      if off < 0 || len < 0 || off > bytes_length - len then
        Error (body_error "request body write bounds are invalid")
      else write_request_body_string writer (Bytes.sub_string bytes off len)
  end

  module Response_body = struct
    type 'a io = 'a
    type t = response_body
    type reader = response_body_reader

    let read = read_response_body

    let next ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Error (Awskit.Error.Producer.body "chunk_size must be positive")
      else
        let bytes = Bytes.create chunk_size in
        match read_response_body reader bytes ~off:0 ~len:chunk_size with
        | Error _ as error -> error
        | Ok 0 -> Ok None
        | Ok n -> Ok (Some (Bytes.sub bytes 0 n))

    let with_reader = with_response_body
    let discard = discard_response_body
  end

  module IO = struct
    type 'a t = 'a

    let return x = x
    let bind x f = f x
  end

  module Transport = struct
    type 'a io = 'a
    type nonrec connection = connection
    type nonrec request_body = request_body
    type nonrec response_body = response_body

    let with_response _ _ ~body:_ ~consume:_ =
      Error
        (Awskit.Error.Producer.transport ~retryable:false
           "Simulator.Runtime.with_response is not an HTTP transport")
  end

  module Clock = struct
    type nonrec connection = connection

    let now = now
  end

  module Sleeper = struct
    type 'a io = 'a
    type nonrec connection = connection

    let sleep t span =
      Simulator_state.Clock.advance (Simulator_state.clock (store t)) span
  end

  module Random = struct
    type nonrec connection = connection

    let float _ ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a
    type nonrec connection = connection

    let resolve t = Ok (Simulator_state.credentials t)
  end

  module Endpoint = struct
    type nonrec connection = connection

    let region _ = Awskit.Region.of_string_exn "us-east-1"
    let endpoint _ = None
  end

  module Retry = struct
    type nonrec connection = connection

    let policy _ = Awskit.Retry.default
  end

  module Timeout = struct
    type nonrec connection = connection

    let policy _ = Awskit.Timeout.default
  end

  module S3_endpoint = struct
    type nonrec connection = connection

    let s3_endpoint_config _ = Awskit_s3.default_endpoint_config
  end
end
