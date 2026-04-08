open Base

let src = Logs.Src.create "awskit-lwt" ~doc:"AWS Lwt HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Client : Cohttp_lwt.S.Client) = struct
  let default_max_response_body_bytes = 64 * 1024 * 1024

  type conn = {
    ctx : Client.ctx option;
    endpoint : Awskit.Endpoint.t option;
    region : string;
    credentials : Awskit.Credentials.t;
    clock : unit -> Ptime.t;
    max_response_body_bytes : int;
  }

  let validate_create_args ?endpoint ~max_response_body_bytes () =
    if max_response_body_bytes <= 0 then
      invalid_arg
        "Awskit_lwt.Make.create: max_response_body_bytes must be positive";
    Option.iter endpoint ~f:(fun endpoint ->
        ignore (Awskit.Endpoint.to_url_prefix endpoint))

  let create ?ctx ?endpoint ~region ~credentials ~clock
      ?(max_response_body_bytes = default_max_response_body_bytes) () =
    validate_create_args ?endpoint ~max_response_body_bytes ();
    { ctx; endpoint; region; credentials; clock; max_response_body_bytes }

  let scheme conn =
    match conn.endpoint with
    | Some endpoint -> Awskit.Endpoint.scheme endpoint
    | None -> `Https

  (* ── URI construction ──────────────────────────────────────────── *)

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

  let read_body_with_limit ~max_bytes body =
    let stream = Cohttp_lwt.Body.to_stream body in
    let buffer = Buffer.create 1024 in
    let rec loop total =
      Lwt.bind (Lwt_stream.get stream) (function
        | None -> Lwt.return_ok (Buffer.contents buffer)
        | Some chunk ->
            let total = total + String.length chunk in
            if total > max_bytes then
              Lwt.return_error
                (`Body_read_error
                   (Fmt.str "response body exceeded limit of %d bytes" max_bytes))
            else begin
              Buffer.add_string buffer chunk;
              loop total
            end)
    in
    loop 0

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
            Lwt.bind
              (read_body_with_limit ~max_bytes:conn.max_response_body_bytes
                 response_body) (function
              | Error _ as error -> Lwt.return error
              | Ok body_str ->
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
    let endpoint c = c.endpoint
    let call = do_call
  end

  type t = conn
end
