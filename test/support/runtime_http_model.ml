open Base

type method_ = Awskit.Request.Method.t

type framing =
  | Empty
  | Content_length of { declared : int; actual : string }
  | Chunked of string list
  | Malformed_chunked of string

type connection = Close | Keep_alive
type expected_body = No_body | Body of string | Body_error

type scenario = {
  name : string;
  method_ : method_;
  status : int;
  headers : (string * string) list;
  framing : framing;
  connection : connection;
  expected_body : expected_body;
}

let method_to_string = Awskit.Request.Method.to_string

let connection_header = function
  | Close -> ("Connection", "close")
  | Keep_alive -> ("Connection", "keep-alive")

let is_bodiless_status status = status = 204 || status = 304

let is_bodiless scenario =
  match scenario.method_ with
  | `HEAD -> true
  | `GET | `PUT | `POST | `DELETE | `PATCH -> is_bodiless_status scenario.status

let headers_for_framing = function
  | Empty -> []
  | Content_length { declared; _ } ->
      [ ("Content-Length", Int.to_string declared) ]
  | Chunked _ | Malformed_chunked _ -> [ ("Transfer-Encoding", "chunked") ]

let chunk_wire chunk = Printf.sprintf "%x\r\n%s\r\n" (String.length chunk) chunk

let body_for_framing = function
  | Empty -> ""
  | Content_length { actual; _ } -> actual
  | Chunked chunks ->
      String.concat (List.map chunks ~f:chunk_wire) ^ "0\r\n\r\n"
  | Malformed_chunked wire -> wire

let response_headers scenario =
  scenario.headers
  @ headers_for_framing scenario.framing
  @ [ connection_header scenario.connection ]

let expected_body_for ~bodiless framing =
  if bodiless then No_body
  else
    match framing with
    | Empty -> Body ""
    | Content_length { declared; actual } ->
        let actual_length = String.length actual in
        if Int.equal declared actual_length then Body actual
        else if declared < actual_length then
          Body (String.prefix actual declared)
        else Body_error
    | Chunked chunks -> Body (String.concat chunks)
    | Malformed_chunked _ -> Body_error

let scenario ~name ~method_ ~status ?(headers = []) ~framing ~connection () =
  let partial =
    {
      name;
      method_;
      status;
      headers;
      framing;
      connection;
      expected_body = No_body;
    }
  in
  {
    partial with
    expected_body = expected_body_for ~bodiless:(is_bodiless partial) framing;
  }

let framing_to_string = function
  | Empty -> "empty"
  | Content_length { declared; actual } ->
      Printf.sprintf "content-length declared=%d actual=%d" declared
        (String.length actual)
  | Chunked chunks ->
      Printf.sprintf "chunked chunks=[%s]"
        (String.concat ~sep:","
           (List.map chunks ~f:(fun chunk ->
                Int.to_string (String.length chunk))))
  | Malformed_chunked wire ->
      Printf.sprintf "malformed-chunked bytes=%d" (String.length wire)

let connection_to_string = function
  | Close -> "close"
  | Keep_alive -> "keep-alive"

let expected_body_to_string = function
  | No_body -> "no-body"
  | Body body -> Printf.sprintf "body(%d)" (String.length body)
  | Body_error -> "body-error"

let to_string scenario =
  Printf.sprintf "%s method=%s status=%d framing=%s connection=%s expected=%s"
    scenario.name
    (method_to_string scenario.method_)
    scenario.status
    (framing_to_string scenario.framing)
    (connection_to_string scenario.connection)
    (expected_body_to_string scenario.expected_body)
