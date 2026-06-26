open Awskit_s3

module Runtime = struct
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;
    read_error_after : int option;
  }

  type call = { request : Awskit.Request.t; body : string }

  type request_body = {
    body : (string, Awskit.Error.t) result;
    descriptor : Awskit.Body.Request.descriptor;
  }

  type connection = {
    region : Region.t;
    credentials : Credentials.t;
    endpoint_config : Awskit_s3.endpoint_config;
    retry_policy : Awskit.Retry.t;
    mutable calls : call list;
    mutable responses : response list;
    mutable sleeps : Ptime.Span.t list;
  }

  type 'a t = 'a
  type response_body = { body : string; read_error_after : int option }
  type request_body_writer = Buffer.t

  type response_body_reader = {
    body : string;
    read_error_after : int option;
    mutable active : bool;
    mutable offset : int;
  }

  let connect ?(endpoint_config = Awskit_s3.default_endpoint_config)
      ?(region = Region.of_string_exn "us-east-1")
      ?(credentials = Protocol_support.credentials)
      ?(retry_policy = Awskit.Retry.default) responses =
    {
      region;
      credentials;
      endpoint_config;
      retry_policy;
      calls = [];
      responses;
      sleeps = [];
    }

  let last_call conn =
    match conn.calls with
    | call :: _ -> call
    | [] -> Alcotest.fail "expected recorded request"

  let descriptor ?(replayable = true) body =
    Awskit.Body.Request.descriptor_exn
      ~content_length:(Int64.of_int (String.length body))
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
      ~replayable ()

  let request_body ?replayable body =
    { body = Ok body; descriptor = descriptor ?replayable body }

  let empty_request_body = request_body ""
  let string_request_body body = request_body body
  let bytes_request_body body = request_body (Bytes.to_string body)

  let stream_request_body descriptor ~write =
    let buffer = Buffer.create 128 in
    let body =
      match write buffer with
      | Ok () -> (
          let body = Buffer.contents buffer in
          let length = Int64.of_int (String.length body) in
          match descriptor.Awskit.Body.Request.content_length with
          | Some declared when not (Int64.equal declared length) ->
              Error (Awskit.Error.Producer.body "request body length mismatch")
          | _ -> Ok body)
      | Error _ as error -> error
    in
    { body; descriptor }

  let request_body_descriptor body = body.descriptor

  let write_request_body_string writer body =
    Buffer.add_string writer body;
    Ok ()

  let read_response_body reader bytes ~off ~len =
    if not reader.active then
      Error
        (Awskit.Error.Producer.body
           "response body reader used outside its scope")
    else if len = 0 then Ok 0
    else if
      match reader.read_error_after with
      | Some limit -> reader.offset >= limit
      | None -> false
    then Error (Awskit.Error.Producer.body "simulated download read failure")
    else
      let remaining = String.length reader.body - reader.offset in
      if remaining <= 0 then Ok 0
      else
        let copied = min len remaining in
        String.blit reader.body reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        Ok copied

  let rec drain reader =
    let bytes = Bytes.create 8 in
    match read_response_body reader bytes ~off:0 ~len:(Bytes.length bytes) with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> drain reader

  let with_response_body (body : response_body) ~consume =
    let reader =
      {
        body = body.body;
        read_error_after = body.read_error_after;
        active = true;
        offset = 0;
      }
    in
    match consume reader with
    | Ok _ as result -> (
        match drain reader with
        | Ok () ->
            reader.active <- false;
            result
        | Error _ as error ->
            reader.active <- false;
            error)
    | Error _ as error -> (
        match drain reader with
        | Ok () | Error _ ->
            reader.active <- false;
            error)

  let discard_response_body (body : response_body) =
    let reader =
      {
        body = body.body;
        read_error_after = body.read_error_after;
        active = true;
        offset = 0;
      }
    in
    drain reader

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
      write_request_body_string writer (Bytes.sub_string bytes off len)
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

  let do_with_response conn request (body : request_body) ~f =
    match body.body with
    | Error _ as error -> error
    | Ok body -> (
        conn.calls <- { request; body } :: conn.calls;
        match conn.responses with
        | [] ->
            Error
              (Awskit.Error.Producer.transport ~retryable:false
                 "no canned response")
        | response :: rest ->
            conn.responses <- rest;
            f
              (Awskit.Response.create_exn ~status:response.status
                 ~headers:response.headers ())
              {
                body = response.body;
                read_error_after = response.read_error_after;
              })

  module IO = struct
    type 'a t = 'a

    let return value = value
    let bind value f = f value
  end

  module Transport = struct
    type 'a io = 'a
    type nonrec connection = connection
    type nonrec request_body = request_body
    type nonrec response_body = response_body

    let with_response conn request ~body ~consume =
      do_with_response conn request body ~f:consume
  end

  module Clock = struct
    type nonrec connection = connection

    let now _ = Protocol_support.test_time
  end

  module Sleeper = struct
    type 'a io = 'a
    type nonrec connection = connection

    let sleep conn span = conn.sleeps <- span :: conn.sleeps
  end

  module Random = struct
    type nonrec connection = connection

    let float _ ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a
    type nonrec connection = connection

    let resolve conn = Ok conn.credentials
  end

  module Endpoint = struct
    type nonrec connection = connection

    let region conn = conn.region
    let endpoint _ = None
  end

  module Retry = struct
    type nonrec connection = connection

    let policy conn = conn.retry_policy
  end

  module Timeout = struct
    type nonrec connection = connection

    let policy _ = Awskit.Timeout.default
  end

  module S3_endpoint = struct
    type nonrec connection = connection

    let s3_endpoint_config conn = conn.endpoint_config
  end
end

module S3 = Awskit_s3.Make (Runtime)

type response = Runtime.response
type call = Runtime.call
type connection = Runtime.connection

let connect = Runtime.connect
let last_call = Runtime.last_call

let response ?(headers = []) ?read_error_after status body =
  { Runtime.status; headers; body; read_error_after }
