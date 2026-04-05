open Base

let src = Logs.Src.create "aws-eio" ~doc:"AWS Eio HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

type net = Net : _ Eio.Net.t -> net

type conn = {
  net : net;
  region : string;
  credentials : Awskit.Credentials.t;
  clock : unit -> Ptime.t;
  endpoint : string option;
  port : int option;
}

let create ~env ~region ~credentials ?(clock = Ptime_clock.now) ?endpoint ?port
    () =
  let net = Net (env :> < net : _ Eio.Net.t ; .. >)#net in
  { net; region; credentials; clock; endpoint; port }

(* ── Method conversion ──────────────────────────────────────────── *)

let to_cohttp_meth = function
  | Awskit.Request.GET -> `GET
  | PUT -> `PUT
  | POST -> `POST
  | DELETE -> `DELETE
  | HEAD -> `HEAD

(* ── Response conversion ────────────────────────────────────────── *)

let to_aws_response http_response body_str : Awskit.Response.t =
  {
    status = Http.Response.status http_response |> Http.Status.to_int;
    headers = Http.Response.headers http_response |> Http.Header.to_list;
    body = body_str;
  }

(* ── URI construction ───────────────────────────────────────────── *)

let make_uri (request : Awskit.Request.t) =
  let host_port =
    match request.port with
    | Some p -> Fmt.str "%s:%d" request.host p
    | None -> request.host
  in
  Uri.of_string (Fmt.str "http://%s%s" host_port request.path)

(* ── HTTP call ──────────────────────────────────────────────────── *)

let do_call (conn : conn) (request : Awskit.Request.t) =
  let (Net net) = conn.net in
  let client = Cohttp_eio.Client.make ~https:None net in
  let headers = Http.Header.of_list request.headers in
  let uri = make_uri request in
  let meth = to_cohttp_meth request.meth in
  try
    Eio.Switch.run (fun sw ->
        let response, body_str =
          match meth with
          | `HEAD ->
              (* Client.head discards the body — cohttp-eio's generic call
                 would hang trying to read Content-Length bytes that the
                 server never sends for HEAD responses. *)
              let response = Cohttp_eio.Client.head client ~sw ~headers uri in
              (response, "")
          | `GET | `DELETE ->
              let response, body =
                Cohttp_eio.Client.call client ~sw ~headers meth uri
              in
              let body_str =
                Eio.Buf_read.(of_flow ~max_size:Int.max_value body |> take_all)
              in
              (response, body_str)
          | `PUT | `POST ->
              let body = Cohttp_eio.Body.of_string request.body in
              let response, rbody =
                Cohttp_eio.Client.call client ~sw ~headers ~body meth uri
              in
              let body_str =
                Eio.Buf_read.(of_flow ~max_size:Int.max_value rbody |> take_all)
              in
              (response, body_str)
          | _ -> assert false
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

(* ── Module satisfying Awskit.Runtime.S ────────────────────────────── *)

type +'a t = 'a

let return x = x
let bind x f = f x

type connection = conn

let region c = c.region
let credentials c = c.credentials
let clock c = c.clock ()
let endpoint_host c = c.endpoint
let endpoint_port c = c.port
let call = do_call
