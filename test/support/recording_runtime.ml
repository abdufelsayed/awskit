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
  region : Awskit.Region.t;
  credentials : Awskit.Credentials.t;
  endpoint : Awskit.Endpoint.t option;
  retry_policy : Awskit.Retry.t;
  timeout_policy : Awskit.Timeout.policy;
  mutable calls : call list;
  mutable responses : response list;
  mutable sleeps : Ptime.Span.t list;
}

type 'a t = 'a
type response_body = { body : string; read_error_after : int option }
type request_body_writer = { buffer : Buffer.t; mutable active : bool }

type response_body_reader = {
  body : string;
  read_error_after : int option;
  mutable active : bool;
  mutable offset : int;
}

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKIDEXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY" ()

let connect ?(region = Awskit.Region.of_string_exn "us-east-1")
    ?(credentials = credentials) ?endpoint
    ?(retry_policy = Awskit.Retry.default)
    ?(timeout_policy = Awskit.Timeout.default) responses =
  {
    region;
    credentials;
    endpoint;
    retry_policy;
    timeout_policy;
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

let stream_request_body descriptor ~write =
  let writer = { buffer = Buffer.create 128; active = true } in
  let body =
    try
      match write writer with
      | Error _ as error -> error
      | Ok () -> (
          writer.active <- false;
          let body = Buffer.contents writer.buffer in
          let actual = Int64.of_int (String.length body) in
          match descriptor.Awskit.Body.Request.content_length with
          | Some declared when not (Int64.equal declared actual) ->
              Error (Awskit.Error.Producer.body "request body length mismatch")
          | Some _ | None -> Ok body)
    with exn ->
      writer.active <- false;
      raise exn
  in
  writer.active <- false;
  { body; descriptor }

let write_request_body_string (writer : request_body_writer) body =
  if not writer.active then
    Error
      (Awskit.Error.Producer.body "request body writer used outside its scope")
  else (
    Buffer.add_string writer.buffer body;
    Ok ())

let valid_bounds bytes ~off ~len =
  off >= 0 && len >= 0 && off <= Bytes.length bytes - len

let write_request_body_subbytes (writer : request_body_writer) bytes ~off ~len =
  if not (valid_bounds bytes ~off ~len) then
    Error (Awskit.Error.Producer.body "invalid request body slice")
  else write_request_body_string writer (Bytes.sub_string bytes off len)

let read_response_body reader bytes ~off ~len =
  if not reader.active then
    Error
      (Awskit.Error.Producer.body "response body reader used outside its scope")
  else if not (valid_bounds bytes ~off ~len) then
    Error (Awskit.Error.Producer.body "invalid response body read bounds")
  else if len = 0 then Ok 0
  else if
    match reader.read_error_after with
    | Some limit -> reader.offset >= limit
    | None -> false
  then Error (Awskit.Error.Producer.body "simulated response body read failure")
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
  | Error _ as error ->
      ignore (drain reader : (unit, Awskit.Error.t) result);
      reader.active <- false;
      error
  | exception exn ->
      ignore (drain reader : (unit, Awskit.Error.t) result);
      reader.active <- false;
      raise exn

let discard_response_body (body : response_body) =
  let reader =
    {
      body = body.body;
      read_error_after = body.read_error_after;
      active = true;
      offset = 0;
    }
  in
  let result = drain reader in
  reader.active <- false;
  result

module IO = struct
  type 'a t = 'a

  let return value = value
  let bind value f = f value
end

module Request_body = struct
  type 'a io = 'a
  type t = request_body
  type writer = request_body_writer

  let empty = request_body ""
  let of_string body = request_body body
  let of_bytes bytes = request_body (Bytes.to_string bytes)
  let of_stream = stream_request_body
  let descriptor body = body.descriptor
  let content_length body = body.descriptor.content_length
  let write_string = write_request_body_string

  let write_bytes writer bytes =
    write_request_body_string writer (Bytes.to_string bytes)

  let write_subbytes = write_request_body_subbytes
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
      | Ok len -> Ok (Some (Bytes.sub bytes 0 len))

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

  let now _ =
    match Ptime.of_float_s 1_704_153_600. with
    | Some time -> time
    | None -> assert false
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
  let endpoint conn = conn.endpoint
end

module Retry = struct
  type nonrec connection = connection

  let policy conn = conn.retry_policy
end

module Timeout = struct
  type nonrec connection = connection

  let policy conn = conn.timeout_policy
end

module Runtime :
  Awskit.Runtime.S
    with type 'a t = 'a
     and type connection = connection
     and type request_body = request_body
     and type response_body = response_body
     and type request_body_writer = request_body_writer
     and type response_body_reader = response_body_reader = struct
  type 'a t = 'a
  type nonrec connection = connection
  type nonrec request_body = request_body
  type nonrec response_body = response_body
  type nonrec request_body_writer = request_body_writer
  type nonrec response_body_reader = response_body_reader

  module IO = IO
  module Request_body = Request_body
  module Response_body = Response_body
  module Transport = Transport
  module Clock = Clock
  module Sleeper = Sleeper
  module Random = Random
  module Credentials = Credentials
  module Endpoint = Endpoint
  module Retry = Retry
  module Timeout = Timeout
end

let response ?(headers = []) ?read_error_after status body =
  { status; headers; body; read_error_after }
