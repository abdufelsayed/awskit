open Base

let src = Logs.Src.create "awskit-eio" ~doc:"AWS Eio HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

type net = Net : _ Eio.Net.t -> net
type time_clock = Time_clock : _ Eio.Time.clock -> time_clock
type http_connection = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r

type 'flow https = (Uri.t -> http_connection -> 'flow) option
  constraint 'flow = [> Eio.Resource.close_ty ] Eio.Flow.two_way

let http_only = None

type conn = {
  net : net;
  time_clock : time_clock;
  sw : Eio.Switch.t;
  https : (Uri.t -> http_connection -> http_connection) option;
  region : Awskit.Region.t;
  credentials : Awskit.Credentials.t;
  clock : unit -> Ptime.t;
  retry_policy : Awskit.Retry.t;
  random_float : upper_bound:float -> float;
  timeout_policy : Awskit.Timeout.policy;
  endpoint : Awskit.Endpoint.t option;
  max_response_drain_bytes : int;
}

type stream_item = Chunk of string | End | Failed of string

module Stream_source = struct
  type t = {
    stream : stream_item Eio.Stream.t;
    mutable chunk : string;
    mutable offset : int;
  }

  let rec next_chunk t =
    match Eio.Stream.take t.stream with
    | End -> raise End_of_file
    | Failed message -> failwith message
    | Chunk chunk when String.is_empty chunk -> next_chunk t
    | Chunk chunk ->
        t.chunk <- chunk;
        t.offset <- 0

  let single_read t dst =
    if t.offset >= String.length t.chunk then next_chunk t;
    let len = min (Cstruct.length dst) (String.length t.chunk - t.offset) in
    Cstruct.blit_from_string t.chunk t.offset dst 0 len;
    t.offset <- t.offset + len;
    len

  let read_methods = []
end

let stream_source =
  let ops = Eio.Flow.Pi.source (module Stream_source) in
  fun stream ->
    Eio.Resource.T ({ Stream_source.stream; chunk = ""; offset = 0 }, ops)

type request_body_writer = {
  stream : stream_item Eio.Stream.t;
  remaining : int64 option ref;
  mutable write_error : Awskit.Error.t option;
}

type request_body =
  | Source of Awskit.Body.Request.descriptor * Cohttp_eio.Body.t
  | Stream of
      Awskit.Body.Request.descriptor
      * (request_body_writer -> (unit, Awskit.Error.t) Result.t)

type response_body = {
  body : Cohttp_eio.Body.t;
  time_clock : time_clock;
  timeout_policy : Awskit.Timeout.policy;
  max_response_drain_bytes : int;
}

type response_body_reader = {
  body : Cohttp_eio.Body.t;
  time_clock : time_clock;
  timeout_policy : Awskit.Timeout.policy;
  mutable active : bool;
  mutable chunk : string;
  mutable offset : int;
}

type request_body_bridge = {
  body : Cohttp_eio.Body.t option;
  finished : (unit, Awskit.Error.t) Result.t Eio.Promise.t;
}

let default_max_response_drain_bytes = 64 * 1024 * 1024

let now_from_eio_clock (Time_clock clock) =
  Eio.Time.now clock |> Ptime.of_float_s |> Option.value ~default:Ptime.epoch

let close_https (https : 'flow https) =
  Option.map https ~f:(fun connector uri flow ->
      (connector uri flow :> http_connection))

let parse_endpoint = function
  | None -> Ok None
  | Some endpoint ->
      Result.map (Awskit.Endpoint.of_string endpoint) ~f:Option.some

let create ~env ~sw ~https ~region ~credentials ?clock
    ?(retry_policy = Awskit.Retry.default) ?random_float
    ?(timeout_policy = Awskit.Timeout.default) ?endpoint
    ?(max_response_drain_bytes = default_max_response_drain_bytes) () =
  let validate_max_response_drain_bytes () =
    if max_response_drain_bytes <= 0 then
      Error
        (Awskit.Error.Producer.validation ~field:"max_response_drain_bytes"
           "max_response_drain_bytes must be positive")
    else Ok ()
  in
  match
    ( validate_max_response_drain_bytes (),
      Awskit.Region.of_string region,
      parse_endpoint endpoint )
  with
  | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
  | Ok (), Ok region, Ok endpoint ->
      let net = Net (env :> < net : _ Eio.Net.t ; .. >)#net in
      let time_clock =
        Time_clock (env :> < clock : _ Eio.Time.clock ; .. >)#clock
      in
      let clock =
        Option.value clock ~default:(fun () -> now_from_eio_clock time_clock)
      in
      let random_float =
        match random_float with
        | Some random_float -> random_float
        | None ->
            let state = Random.State.make_self_init () in
            fun ~upper_bound -> Random.State.float state upper_bound
      in
      Ok
        {
          net;
          time_clock;
          sw;
          https = close_https https;
          region;
          credentials;
          clock;
          retry_policy;
          random_float;
          timeout_policy;
          endpoint;
          max_response_drain_bytes;
        }

let to_cohttp_meth = function
  | `GET -> `GET
  | `PUT -> `PUT
  | `POST -> `POST
  | `DELETE -> `DELETE
  | `HEAD -> `HEAD
  | `PATCH -> `PATCH

let to_aws_response http_response =
  Awskit.Response.create_exn
    ~status:(Http.Response.status http_response |> Http.Status.to_int)
    ~headers:(Http.Response.headers http_response |> Http.Header.to_list)
    ()

let make_uri (request : Awskit.Request.t) =
  let target = request.target in
  let scheme_str = Awskit.Endpoint.Scheme.to_string target.scheme in
  let host_port =
    match target.port with
    | Some port -> Fmt.str "%s:%d" target.host port
    | None -> target.host
  in
  Uri.of_string
    (Fmt.str "%s://%s%s" scheme_str host_port
       (Awskit.Request.Target.path_and_query target))

let missing_https_connector_error =
  Awskit.Error.Producer.transport ~retryable:false
    "HTTPS endpoint requires an HTTPS connector. Pass ~https with a connector \
     compatible with Cohttp_eio.Client.make. For local HTTP endpoints, pass \
     ~https:Awskit_eio.http_only and an explicit http:// endpoint."

let descriptor_for_string body =
  {
    Awskit.Body.Request.content_length =
      Some (String.length body |> Int64.of_int);
    payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
    replayable = true;
  }

let empty_request_body =
  Source (descriptor_for_string "", Cohttp_eio.Body.of_string "")

let string_request_body body =
  Source (descriptor_for_string body, Cohttp_eio.Body.of_string body)

let bytes_request_body body =
  let body = Bytes.to_string body in
  string_request_body body

let stream_request_body descriptor ~write = Stream (descriptor, write)

let request_body_descriptor = function
  | Source (descriptor, _) -> descriptor
  | Stream (descriptor, _) -> descriptor

let body_error message = Awskit.Error.Producer.body message

let timeout_phase_name = function
  | `Connect -> "connect"
  | `Attempt -> "attempt"
  | `Operation -> "operation"
  | `Request_body -> "request body"
  | `Response_body -> "response body"
  | `Drain -> "response drain"

let timeout_error phase span =
  let phase_name = timeout_phase_name phase in
  Awskit.Error.Producer.timeout ~operation:phase_name
    (Fmt.str "%s timed out after %.3fs" phase_name (Ptime.Span.to_float_s span))

let with_timeout_result (Time_clock clock) timeout_policy phase f =
  match Awskit.Timeout.span timeout_policy phase with
  | None -> f ()
  | Some span -> (
      try Eio.Time.with_timeout_exn clock (Ptime.Span.to_float_s span) f
      with Eio.Time.Timeout -> Error (timeout_error phase span))

let drain_limit_error max_response_drain_bytes =
  Awskit.Error.Producer.body
    ~limit:(Int64.of_int max_response_drain_bytes)
    "response body exceeded max_response_drain_bytes"

let writer_for descriptor stream =
  {
    stream;
    remaining = ref descriptor.Awskit.Body.Request.content_length;
    write_error = None;
  }

let check_write_length writer length =
  match !(writer.remaining) with
  | None -> Ok ()
  | Some remaining ->
      let length64 = Int64.of_int length in
      if Stdlib.Int64.compare length64 remaining > 0 then
        Error (body_error "request body exceeded declared content_length")
      else (
        writer.remaining := Some (Stdlib.Int64.sub remaining length64);
        Ok ())

let invalid_write_bounds bytes ~off ~len =
  off < 0 || len < 0 || len > Bytes.length bytes - off

let check_finished_length writer =
  match writer.write_error with
  | Some error -> Error error
  | None -> (
      match !(writer.remaining) with
      | None | Some 0L -> Ok ()
      | Some _ ->
          Error (body_error "request body ended before declared content_length")
      )

let write_request_body_string writer string =
  match writer.write_error with
  | Some error -> Error error
  | None -> (
      match check_write_length writer (String.length string) with
      | Error error ->
          writer.write_error <- Some error;
          Error error
      | Ok () ->
          Eio.Stream.add writer.stream (Chunk string);
          Ok ())

let write_request_body_subbytes writer bytes ~off ~len =
  if invalid_write_bounds bytes ~off ~len then
    Error (body_error "invalid write bounds")
  else
    match writer.write_error with
    | Some error -> Error error
    | None -> (
        match check_write_length writer len with
        | Error error ->
            writer.write_error <- Some error;
            Error error
        | Ok () ->
            Eio.Stream.add writer.stream
              (Chunk (Stdlib.Bytes.sub_string bytes off len));
            Ok ())

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
  let write_subbytes = write_request_body_subbytes

  let write_bytes writer bytes =
    write_request_body_subbytes writer bytes ~off:0 ~len:(Bytes.length bytes)
end

let body_to_cohttp ?(on_error = fun _ -> ()) ?(on_escaped_exn = fun _ -> ())
    ~(conn : conn) ~sw = function
  | Source (_, body) ->
      { body = Some body; finished = Eio.Promise.create_resolved (Ok ()) }
  | Stream (descriptor, write) ->
      let stream = Eio.Stream.create 16 in
      let writer = writer_for descriptor stream in
      let request_body_finished, wake_request_body_finished =
        Eio.Promise.create ()
      in
      let finish result =
        ignore
          (Eio.Promise.try_resolve wake_request_body_finished result : bool)
      in
      Eio.Fiber.fork ~sw (fun () ->
          match
            with_timeout_result conn.time_clock conn.timeout_policy
              `Request_body (fun () -> write writer)
          with
          | Ok () ->
              let result = check_finished_length writer in
              Eio.Stream.add stream End;
              finish result
          | Error error ->
              Log.warn (fun m ->
                  m "request body stream failed: %s"
                    (Awskit.Error.to_string_hum error));
              on_error error;
              finish (Error error);
              Eio.Stream.add stream (Failed (Awskit.Error.to_string_hum error))
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception exn -> (
              match Awskit.Body.Request.escaped_exn exn with
              | Some escaped ->
                  on_escaped_exn escaped;
                  finish
                    (Error (body_error "request body writer escaped exception"));
                  Eio.Stream.add stream End
              | None ->
                  let error = body_error (Exn.to_string exn) in
                  Log.warn (fun m ->
                      m "request body stream raised: %s"
                        (Awskit.Error.to_string_hum error));
                  on_error error;
                  finish (Error error);
                  Eio.Stream.add stream
                    (Failed (Awskit.Error.to_string_hum error))));
      { body = Some (stream_source stream); finished = request_body_finished }

let do_with_response (conn : conn) (request : Awskit.Request.t) request_body ~f
    =
  let (Net net) = conn.net in
  let headers = Http.Header.of_list request.headers in
  let uri = make_uri request in
  match (request.target.scheme, conn.https) with
  | `Https, None -> Error missing_https_connector_error
  | `Http, _ | `Https, Some _ ->
      let client = Cohttp_eio.Client.make ~https:conn.https net in
      let meth = to_cohttp_meth request.method_ in
      let make_response_body body =
        {
          body;
          time_clock = conn.time_clock;
          timeout_policy = conn.timeout_policy;
          max_response_drain_bytes = conn.max_response_drain_bytes;
        }
      in
      let successful_status status = status >= 200 && status < 300 in
      let early_result = ref None in
      let request_body_stream_error = ref None in
      let request_body_escaped_exn = ref None in
      let exception Early_response in
      let exception Callback_raised of exn in
      let call_f response body =
        match f response body with
        | result -> result
        | exception exn -> raise (Callback_raised exn)
      in
      let run_call ~call_sw =
        let bridge =
          body_to_cohttp ~conn ~sw:call_sw
            ~on_error:(fun error -> request_body_stream_error := Some error)
            ~on_escaped_exn:(fun exn -> request_body_escaped_exn := Some exn)
            request_body
        in
        try
          with_timeout_result conn.time_clock conn.timeout_policy `Attempt
            (fun () ->
              match
                with_timeout_result conn.time_clock conn.timeout_policy `Connect
                  (fun () ->
                    let response, body =
                      Cohttp_eio.Client.call client ~sw:call_sw ~headers
                        ?body:bridge.body meth uri
                    in
                    Ok (response, body))
              with
              | Error _ as error -> error
              | Ok (response, body) -> (
                  let status =
                    Http.Response.status response |> Http.Status.to_int
                  in
                  let request_body_result =
                    if successful_status status then
                      Eio.Promise.await bridge.finished
                    else
                      match Eio.Promise.peek bridge.finished with
                      | Some result -> result
                      | None ->
                          early_result :=
                            Some
                              (call_f (to_aws_response response)
                                 (make_response_body body));
                          raise Early_response
                  in
                  match !request_body_escaped_exn with
                  | Some escaped -> raise escaped
                  | None -> (
                      match request_body_result with
                      | Error _ as error -> error
                      | Ok () ->
                          Log.debug (fun m -> m "HTTP %d" status);
                          call_f (to_aws_response response)
                            (make_response_body body))))
        with
        | Early_response as exn -> raise exn
        | Callback_raised exn -> raise exn
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> (
            match !request_body_escaped_exn with
            | Some escaped -> raise escaped
            | None -> (
                match Eio.Promise.peek bridge.finished with
                | Some (Error error) -> Error error
                | _ ->
                    let message = Exn.to_string exn in
                    Log.warn (fun m -> m "HTTP call failed: %s" message);
                    Error
                      (Awskit.Error.Producer.transport ~retryable:true message))
            )
      in
      with_timeout_result conn.time_clock conn.timeout_policy `Operation
        (fun () ->
          match request_body with
          | Source _ -> run_call ~call_sw:conn.sw
          | Stream _ -> (
              try
                Eio.Switch.run ~name:"awskit stream request body attempt"
                  (fun request_body_sw -> run_call ~call_sw:request_body_sw)
              with
              | Early_response -> (
                  match !early_result with
                  | Some result -> result
                  | None -> Error (body_error "missing early response result"))
              | Callback_raised exn -> raise exn
              | Eio.Cancel.Cancelled _ as exn -> raise exn
              | exn -> (
                  match !request_body_escaped_exn with
                  | Some escaped -> raise escaped
                  | None -> (
                      match !request_body_stream_error with
                      | Some error -> Error error
                      | None -> raise exn))))

type connection = conn
type 'a t = 'a

let invalid_read_bounds bytes ~off ~len =
  off < 0 || len < 0 || len > Bytes.length bytes - off

let inactive_reader_error =
  Awskit.Error.Producer.body "response body reader used outside its scope"

let close_response_body_reader reader = reader.active <- false

let rec read_from_current reader bytes ~off ~len =
  if len = 0 then Ok 0
  else if reader.offset < String.length reader.chunk then begin
    let available = String.length reader.chunk - reader.offset in
    let copied = min available len in
    Stdlib.String.blit reader.chunk reader.offset bytes off copied;
    reader.offset <- reader.offset + copied;
    Ok copied
  end
  else
    let buffer = Cstruct.create 0x8000 in
    let read = Eio.Flow.single_read reader.body buffer in
    reader.chunk <- Cstruct.to_string ~len:read buffer;
    reader.offset <- 0;
    read_from_current reader bytes ~off ~len

let read_response_body reader bytes ~off ~len =
  if not reader.active then Error inactive_reader_error
  else if invalid_read_bounds bytes ~off ~len then
    Error (Awskit.Error.Producer.body "invalid read bounds")
  else
    with_timeout_result reader.time_clock reader.timeout_policy `Response_body
      (fun () ->
        try read_from_current reader bytes ~off ~len with
        | End_of_file -> Ok 0
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> Error (Awskit.Error.Producer.body (Exn.to_string exn)))

let next_response_body ?(chunk_size = 8192) reader =
  if chunk_size <= 0 then
    Error (Awskit.Error.Producer.body "chunk_size must be positive")
  else
    let buffer = Bytes.create chunk_size in
    match read_response_body reader buffer ~off:0 ~len:chunk_size with
    | Error _ as error -> error
    | Ok 0 -> Ok None
    | Ok n -> Ok (Some (Bytes.sub buffer ~pos:0 ~len:n))

let discard_reader reader ~remaining ~max_response_drain_bytes =
  let buffer = Bytes.create 8192 in
  let rec loop remaining =
    let len =
      if remaining <= 0 then 1 else min (Bytes.length buffer) remaining
    in
    match read_response_body reader buffer ~off:0 ~len with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok n ->
        if n > remaining then Error (drain_limit_error max_response_drain_bytes)
        else loop (remaining - n)
  in
  loop remaining

let discard_response_body_reader (reader : response_body_reader)
    (body : response_body) =
  with_timeout_result body.time_clock body.timeout_policy `Drain (fun () ->
      discard_reader reader ~remaining:body.max_response_drain_bytes
        ~max_response_drain_bytes:body.max_response_drain_bytes)

let discard_response_body_after_exception reader body exn =
  Eio.Cancel.protect (fun () ->
      match discard_response_body_reader reader body with
      | Ok () | Error _ -> ()
      | exception _ -> ());
  close_response_body_reader reader;
  raise exn

let with_response_body (body : response_body) ~consume =
  let reader =
    {
      body = body.body;
      time_clock = body.time_clock;
      timeout_policy = body.timeout_policy;
      active = true;
      chunk = "";
      offset = 0;
    }
  in
  match consume reader with
  | exception exn -> discard_response_body_after_exception reader body exn
  | Ok _ as result -> (
      match discard_response_body_reader reader body with
      | Ok () ->
          close_response_body_reader reader;
          result
      | Error _ as error ->
          close_response_body_reader reader;
          error)
  | Error _ as error -> (
      match discard_response_body_reader reader body with
      | Ok () | Error _ ->
          close_response_body_reader reader;
          error)

let discard_response_body (body : response_body) =
  discard_response_body_reader
    {
      body = body.body;
      time_clock = body.time_clock;
      timeout_policy = body.timeout_policy;
      active = true;
      chunk = "";
      offset = 0;
    }
    body

module Response_body = struct
  type 'a io = 'a
  type t = response_body
  type reader = response_body_reader

  let read = read_response_body
  let next = next_response_body
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
  type connection = conn
  type nonrec request_body = request_body
  type nonrec response_body = response_body

  let with_response conn request ~body ~consume =
    do_with_response conn request body ~f:consume
end

module Clock = struct
  type connection = conn

  let now c = c.clock ()
end

module Sleeper = struct
  type 'a io = 'a
  type connection = conn

  let sleep (c : conn) span =
    let (Time_clock clock) = c.time_clock in
    Eio.Time.sleep clock (Ptime.Span.to_float_s span)
end

module Random = struct
  type connection = conn

  let float c ~upper_bound = c.random_float ~upper_bound
end

module Credentials = struct
  type 'a io = 'a
  type connection = conn

  let resolve c = Ok c.credentials
end

module Endpoint = struct
  type connection = conn

  let region c = c.region
  let endpoint c = c.endpoint
end

module Retry = struct
  type connection = conn

  let policy c = c.retry_policy
end

module Timeout = struct
  type connection = conn

  let policy (c : conn) = c.timeout_policy
end
