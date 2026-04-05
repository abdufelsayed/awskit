open Base

let src = Logs.Src.create "aws-lwt" ~doc:"AWS Lwt HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Client : Cohttp_lwt.S.Client) = struct
  type conn = {
    ctx : Client.ctx option;
    scheme : [ `Http | `Https ];
    region : string;
    credentials : Awskit.Credentials.t;
    clock : unit -> Ptime.t;
    endpoint : string option;
    port : int option;
  }

  let create ?ctx ?(scheme = `Https) ~region ~credentials ~clock ?endpoint ?port
      () =
    { ctx; scheme; region; credentials; clock; endpoint; port }

  (* ── URI construction ──────────────────────────────────────────── *)

  let make_uri (conn : conn) (request : Awskit.Request.t) =
    let scheme_str =
      match conn.scheme with `Http -> "http" | `Https -> "https"
    in
    let host_port =
      match request.port with
      | Some p -> Fmt.str "%s:%d" request.host p
      | None -> request.host
    in
    Uri.of_string (Fmt.str "%s://%s%s" scheme_str host_port request.path)

  (* ── Method conversion ─────────────────────────────────────────── *)

  let to_cohttp_meth = function
    | Awskit.Request.GET -> `GET
    | PUT -> `PUT
    | POST -> `POST
    | DELETE -> `DELETE
    | HEAD -> `HEAD

  (* ── Response conversion ───────────────────────────────────────── *)

  let to_aws_response http_response body_str : Awskit.Response.t =
    {
      status =
        Cohttp.Response.status http_response |> Cohttp.Code.code_of_status;
      headers = Cohttp.Response.headers http_response |> Cohttp.Header.to_list;
      body = body_str;
    }

  (* ── HTTP call ─────────────────────────────────────────────────── *)

  let do_call (conn : conn) (request : Awskit.Request.t) =
    let uri = make_uri conn request in
    let headers = Cohttp.Header.of_list request.headers in
    let body = Cohttp_lwt.Body.of_string request.body in
    let meth = to_cohttp_meth request.meth in
    Lwt.catch
      (fun () ->
        Lwt.bind
          (Client.call ?ctx:conn.ctx ~headers ~body ~chunked:false meth uri)
          (fun (response, response_body) ->
            Lwt.bind (Cohttp_lwt.Body.to_string response_body) (fun body_str ->
                Log.debug (fun m ->
                    m "HTTP %d — %d bytes"
                      (Cohttp.Response.status response
                      |> Cohttp.Code.code_of_status)
                      (String.length body_str));
                Lwt.return_ok (to_aws_response response body_str))))
      (fun exn ->
        let message = Exn.to_string exn in
        Log.warn (fun m -> m "HTTP call failed: %s" message);
        Lwt.return_error (`Body_read_error message))

  (* ── Module satisfying Awskit.Runtime.S ──────────────────────────── *)

  module Runtime = struct
    type +'a t = 'a Lwt.t

    let return = Lwt.return
    let bind = Lwt.bind

    type connection = conn

    let region c = c.region
    let credentials c = c.credentials
    let clock c = c.clock ()
    let endpoint_host c = c.endpoint
    let endpoint_port c = c.port
    let call = do_call
  end

  type t = conn
end
