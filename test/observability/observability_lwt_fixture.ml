open Base

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type behavior =
  | Return of response
  | Returns of response list
  | Pending of (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

let behavior = ref (Return { status = 200; headers = []; body = "payload" })
let calls = Atomic.make 0

module Client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _method _uri =
    ignore (Atomic.fetch_and_add calls 1 : int);
    let response =
      match !behavior with
      | Returns (response :: remaining) ->
          behavior := Returns remaining;
          Return response
      | Returns [] -> failwith "observability fixture response queue exhausted"
      | behavior -> behavior
    in
    match response with
    | Pending promise -> promise
    | Return response ->
        let headers =
          Cohttp.Header.of_list
            (("content-length", Int.to_string (String.length response.body))
            :: response.headers)
        in
        let consume_request =
          match body with
          | None -> Lwt.return_unit
          | Some body -> Lwt.map (fun _ -> ()) (Cohttp_lwt.Body.to_string body)
        in
        Lwt.bind consume_request (fun () ->
            Lwt.return
              ( Cohttp.Response.make ~headers
                  ~status:(Cohttp.Code.status_of_code response.status)
                  (),
                Cohttp_lwt.Body.of_string response.body ))
    | Returns _ -> failwith "unreachable fixture response queue"
end

module S3 = Awskit_s3_lwt.Make (Client)

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let connection ?(retry_policy = Awskit.Retry.disabled) ?observability () =
  match
    S3.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy ~timeout_policy:Awskit.Timeout.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.)
      ?observability ()
  with
  | Ok connection -> connection
  | Error error -> failwith (Awskit.Error.to_string_hum error)

let set_response ?(headers = []) ~status body =
  behavior := Return { status; headers; body }

let set_responses responses =
  behavior :=
    Returns
      (List.map responses ~f:(fun (status, headers, body) ->
           { status; headers; body }))

let set_pending promise = behavior := Pending promise
let reset_calls () = Atomic.set calls 0
let call_count () = Atomic.get calls

let get conn =
  S3.Object.get_string conn
    ~bucket:(Awskit_s3.Bucket_name.of_string_exn "observability-bucket")
    ~key:(Awskit_s3.Object_key.of_string_exn "object")
    ~max_bytes:1024L ()

let get_value conn =
  Lwt.map
    (function
      | Ok result -> Ok result.Awskit_s3.Object.Get.value
      | Error _ as error -> error)
    (get conn)

let put_string conn contents =
  S3.Object.put_string conn
    ~bucket:(Awskit_s3.Bucket_name.of_string_exn "observability-bucket")
    ~key:(Awskit_s3.Object_key.of_string_exn "object")
    ~contents ()

let presign_get conn =
  S3.Presigned.get_object conn
    ~bucket:(Awskit_s3.Bucket_name.of_string_exn "observability-bucket")
    ~key:(Awskit_s3.Object_key.of_string_exn "object")
    ()
