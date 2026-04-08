open Base

let src = Logs.Src.create "awskit-eio" ~doc:"AWS Eio HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

type net = Net : _ Eio.Net.t -> net

type conn = {
  net : net;
  region : string;
  credentials : Awskit.Credentials.t;
  clock : unit -> Ptime.t;
  endpoint : Awskit.Endpoint.t option;
  max_response_body_bytes : int;
}

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

let create ~env ~region ~credentials ?(clock = Ptime_clock.now) ?endpoint
    ?(max_response_body_bytes = default_max_response_body_bytes) () =
  if max_response_body_bytes <= 0 then
    invalid_arg "Awskit_eio.create: max_response_body_bytes must be positive";
  let net = Net (env :> < net : _ Eio.Net.t ; .. >)#net in
  { net; region; credentials; clock; endpoint; max_response_body_bytes }

let to_cohttp_meth = function
  | Awskit.Request.GET -> `GET
  | PUT -> `PUT
  | POST -> `POST
  | DELETE -> `DELETE
  | HEAD -> `HEAD

let to_aws_response http_response body_str : Awskit.Response.t =
  {
    status = Http.Response.status http_response |> Http.Status.to_int;
    headers = Http.Response.headers http_response |> Http.Header.to_list;
    body = body_str;
  }

let scheme conn =
  match conn.endpoint with
  | Some endpoint -> Awskit.Endpoint.scheme endpoint
  | None -> `Https

let make_uri (conn : conn) (request : Awskit.Request.t) =
  let scheme_str =
    match scheme conn with `Http -> "http" | `Https -> "https"
  in
  let host_port =
    match request.port with
    | Some p -> Fmt.str "%s:%d" request.host p
    | None -> request.host
  in
  Uri.of_string (Fmt.str "%s://%s%s" scheme_str host_port request.path)

let do_call (conn : conn) (request : Awskit.Request.t) =
  let (Net net) = conn.net in
  let https =
    match scheme conn with `Http -> None | `Https -> Some https_connector
  in
  let client = Cohttp_eio.Client.make ~https net in
  let headers = Http.Header.of_list request.headers in
  let uri = make_uri conn request in
  let meth = to_cohttp_meth request.meth in
  try
    Eio.Switch.run (fun sw ->
        let response, body_str =
          match meth with
          | `HEAD ->
              let response = Cohttp_eio.Client.head client ~sw ~headers uri in
              (response, "")
          | `GET | `DELETE ->
              let response, body =
                Cohttp_eio.Client.call client ~sw ~headers meth uri
              in
              let body_str =
                Eio.Buf_read.(
                  of_flow ~max_size:conn.max_response_body_bytes body
                  |> take_all)
              in
              (response, body_str)
          | `PUT | `POST ->
              let body = Cohttp_eio.Body.of_string request.body in
              let response, rbody =
                Cohttp_eio.Client.call client ~sw ~headers ~body meth uri
              in
              let body_str =
                Eio.Buf_read.(
                  of_flow ~max_size:conn.max_response_body_bytes rbody
                  |> take_all)
              in
              (response, body_str)
        in
        Log.debug (fun m ->
            m "HTTP %d — %d bytes"
              (Http.Response.status response |> Http.Status.to_int)
              (String.length body_str));
        Ok (to_aws_response response body_str))
  with exn ->
    let message = Exn.to_string exn in
    Log.warn (fun m -> m "HTTP call failed: %s" message);
    Error (`Body_read_error message)

type +'a t = 'a

let return x = x
let bind x f = f x

type connection = conn

let region c = c.region
let credentials c = c.credentials
let clock c = c.clock ()
let endpoint c = c.endpoint
let call = do_call
