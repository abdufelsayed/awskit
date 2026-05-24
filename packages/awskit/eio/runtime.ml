open Base

let src = Logs.Src.create "awskit-eio" ~doc:"AWS Eio HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

type net = Net : _ Eio.Net.t -> net
type time_clock = Time_clock : _ Eio.Time.clock -> time_clock

type conn = {
  net : net;
  time_clock : time_clock;
  sw : Eio.Switch.t;
  region : Awskit.Region.t;
  credentials : Awskit.Credentials.t;
  clock : unit -> Ptime.t;
  retry_policy : Awskit.Retry.t;
  endpoint : Awskit.Endpoint.t option;
  max_response_body_bytes : int;
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

type upload_writer = stream_item Eio.Stream.t

type upload_body =
  | Source of Awskit.Body.Upload.descriptor * Cohttp_eio.Body.t
  | Stream of
      Awskit.Body.Upload.descriptor
      * (upload_writer -> (unit, Awskit.Error.t) Result.t)

type download_body = { body : Cohttp_eio.Body.t; max_response_body_bytes : int }
type download_reader = Cohttp_eio.Body.t

let default_max_response_body_bytes = 64 * 1024 * 1024
let authenticator = lazy (Ca_certs.authenticator ())

let tls_config =
  lazy
    (match Lazy.force authenticator with
    | Error (`Msg msg) ->
        invalid_arg
          (Fmt.str
             "Awskit_eio.create: failed to create system X509 authenticator: %s"
             msg)
    | Ok authenticator -> (
        match Tls.Config.client ~authenticator () with
        | Error (`Msg msg) ->
            invalid_arg
              (Fmt.str "Awskit_eio.create: failed to create TLS config: %s" msg)
        | Ok config -> config))

let ensure_tls_runtime () = Mirage_crypto_rng_unix.use_default ()

let https_connector uri raw =
  ensure_tls_runtime ();
  let host =
    Uri.host uri
    |> Option.map ~f:(fun x -> Domain_name.(host_exn (of_string_exn x)))
  in
  Tls_eio.client_of_flow ?host (Lazy.force tls_config) raw

let create ~env ~sw ~region ~credentials ?(clock = Ptime_clock.now)
    ?(retry_policy = Awskit.Retry.default) ?endpoint
    ?(max_response_body_bytes = default_max_response_body_bytes) () =
  if max_response_body_bytes <= 0 then
    invalid_arg "Awskit_eio.create: max_response_body_bytes must be positive";
  let net = Net (env :> < net : _ Eio.Net.t ; .. >)#net in
  let time_clock =
    Time_clock (env :> < clock : _ Eio.Time.clock ; .. >)#clock
  in
  {
    net;
    time_clock;
    sw;
    region;
    credentials;
    clock;
    retry_policy;
    endpoint;
    max_response_body_bytes;
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

let descriptor_for_string body =
  {
    Awskit.Body.Upload.content_length = Some (String.length body |> Int64.of_int);
    payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
    replayable = true;
  }

let empty_body = Source (descriptor_for_string "", Cohttp_eio.Body.of_string "")

let string_body body =
  Source (descriptor_for_string body, Cohttp_eio.Body.of_string body)

let bytes_body body =
  let body = Bytes.to_string body in
  string_body body

let stream_body descriptor ~write = Stream (descriptor, write)

let upload_descriptor = function
  | Source (descriptor, _) -> descriptor
  | Stream (descriptor, _) -> descriptor

let write_string writer string =
  Eio.Stream.add writer (Chunk string);
  Ok ()

let body_to_cohttp conn = function
  | Source (_, body) -> Some body
  | Stream (_, write) ->
      let stream = Eio.Stream.create 16 in
      Eio.Fiber.fork ~sw:conn.sw (fun () ->
          match write stream with
          | Ok () -> Eio.Stream.add stream End
          | Error error ->
              Eio.Stream.add stream (Failed (Awskit.Error.to_string_hum error)));
      Some (stream_source stream)

let do_call (conn : conn) (request : Awskit.Request.t) upload_body =
  let (Net net) = conn.net in
  let https =
    match request.target.scheme with
    | `Http -> None
    | `Https -> Some https_connector
  in
  let client = Cohttp_eio.Client.make ~https net in
  let headers = Http.Header.of_list request.headers in
  let uri = make_uri request in
  let meth = to_cohttp_meth request.method_ in
  try
    let response, body =
      Cohttp_eio.Client.call client ~sw:conn.sw ~headers
        ?body:(body_to_cohttp conn upload_body)
        meth uri
    in
    Log.debug (fun m ->
        m "HTTP %d" (Http.Response.status response |> Http.Status.to_int));
    Ok
      ( to_aws_response response,
        { body; max_response_body_bytes = conn.max_response_body_bytes } )
  with exn ->
    let message = Exn.to_string exn in
    Log.warn (fun m -> m "HTTP call failed: %s" message);
    Error (Awskit.Error.transport ~retryable:true message)

type +'a t = 'a

let return x = x
let bind x f = f x

type connection = conn

let now c = c.clock ()
let region c = c.region
let credentials c = Ok c.credentials
let endpoint c = c.endpoint
let retry_policy c = c.retry_policy

let sleep c span =
  let (Time_clock clock) = c.time_clock in
  Eio.Time.sleep clock (Ptime.Span.to_float_s span)

let read source bytes ~off ~len =
  if len = 0 then Ok 0
  else
    try Ok (Eio.Flow.single_read source (Cstruct.of_bytes ~off ~len bytes)) with
    | End_of_file -> Ok 0
    | exn -> Error (Awskit.Error.body (Exn.to_string exn))

let drain_limit_error max_response_body_bytes =
  Awskit.Error.body
    ~limit:(Int64.of_int max_response_body_bytes)
    "response body exceeded max_response_body_bytes"

let rec discard_reader reader ~remaining ~max_response_body_bytes =
  let buffer = Bytes.create 8192 in
  let len = if remaining <= 0 then 1 else min (Bytes.length buffer) remaining in
  match read reader buffer ~off:0 ~len with
  | Error _ as error -> error
  | Ok 0 -> Ok ()
  | Ok n ->
      if n > remaining then Error (drain_limit_error max_response_body_bytes)
      else
        discard_reader reader ~remaining:(remaining - n)
          ~max_response_body_bytes

let discard_download_reader reader body =
  discard_reader reader ~remaining:body.max_response_body_bytes
    ~max_response_body_bytes:body.max_response_body_bytes

let with_download_body body ~consume =
  match consume body.body with
  | Ok _ as result -> (
      match discard_download_reader body.body body with
      | Ok () -> result
      | Error _ as error -> error)
  | Error _ as error -> (
      match discard_download_reader body.body body with
      | Ok () -> error
      | Error _ as drain_error -> drain_error)

let discard_download_body body = discard_download_reader body.body body
let call = do_call
