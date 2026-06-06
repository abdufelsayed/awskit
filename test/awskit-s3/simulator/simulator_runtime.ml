open Core
open Simulator_state

module Runtime = struct
  type connection = t
  type 'a t = 'a

  let return x = x
  let bind x f = f x

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
  }

  let now = now
  let region _ = Awskit.Region.of_string_exn "us-east-1"
  let credentials t = Ok (Simulator_state.credentials t)
  let endpoint _ = None
  let retry_policy _ = Awskit.Retry.default
  let sleep t span = Clock.advance (Simulator_state.clock (store t)) span
  let s3_endpoint_config _ = default_endpoint_config

  let descriptor_for_string body =
    {
      Awskit.Body.Request.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let empty_request_body =
    { descriptor = descriptor_for_string ""; body = Ok "" }

  let string_request_body value =
    { descriptor = descriptor_for_string value; body = Ok value }

  let bytes_request_body value = string_request_body (Bytes.to_string value)
  let body_error message = Awskit.Error.body message

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
    match reader.read_fault with
    | Some error ->
        reader.read_fault <- None;
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

  let rec discard_reader reader =
    let buffer = Bytes.create 8192 in
    match
      read_response_body reader buffer ~off:0 ~len:(Bytes.length buffer)
    with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> discard_reader reader

  let with_response_body (body : response_body) ~consume =
    let reader =
      { body = body.body; offset = 0; read_fault = body.read_fault }
    in
    match consume reader with
    | Ok _ as result -> (
        match discard_reader reader with
        | Ok () -> result
        | Error _ as error -> error)
    | Error _ as error -> (
        match discard_reader reader with
        | Ok () -> error
        | Error _ as drain_error -> drain_error)

  let discard_response_body body =
    with_response_body body ~consume:(fun reader -> discard_reader reader)

  module Request_body = struct
    let empty = empty_request_body
    let of_string = string_request_body
    let of_bytes = bytes_request_body
    let of_stream = stream_request_body
    let descriptor = request_body_descriptor
    let write_string = write_request_body_string
  end

  module Response_body = struct
    let read = read_response_body
    let with_reader = with_response_body
    let discard = discard_response_body
  end

  let with_response _ _ _ ~f:_ =
    Error
      (Awskit.Error.transport ~retryable:false
         "Simulator.Runtime.with_response is not an HTTP transport")
end
