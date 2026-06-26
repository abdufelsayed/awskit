open Base

type method_ = Awskit.Request.Method.t

type framing =
  | Empty
  | Content_length of { declared : int; actual : string }
  | Duplicate_content_length of { first : int; second : int; actual : string }
  | Conflicting_length_and_chunked of { declared : int; chunks : string list }
  | Early_close of string
  | Chunked of string list
  | Malformed_chunked of string
  | Malformed_header_block of string

type connection = Close | Keep_alive
type expected_body = No_body | Body of string | Body_error

type consume =
  | Read_all
  | Read_once of int
  | Drop_without_read
  | Raise_in_consume

type observed =
  | Observed_body of string
  | Observed_error of string
  | Observed_exception

type scenario = {
  name : string;
  method_ : method_;
  status : int;
  headers : (string * string) list;
  framing : framing;
  connection : connection;
  consume : consume;
  expected_body : expected_body;
}

let method_to_string = Awskit.Request.Method.to_string

let connection_header = function
  | Close -> ("Connection", "close")
  | Keep_alive -> ("Connection", "keep-alive")

let is_bodiless_status status =
  (status >= 100 && status < 200) || status = 204 || status = 304

let is_bodiless_response ~method_ ~status =
  match method_ with
  | `HEAD -> true
  | `GET | `PUT | `POST | `DELETE | `PATCH -> is_bodiless_status status

let is_bodiless scenario =
  is_bodiless_response ~method_:scenario.method_ ~status:scenario.status

let headers_for_framing = function
  | Empty -> []
  | Content_length { declared; _ } ->
      [ ("Content-Length", Int.to_string declared) ]
  | Duplicate_content_length { first; second; _ } ->
      [
        ("Content-Length", Int.to_string first);
        ("Content-Length", Int.to_string second);
      ]
  | Conflicting_length_and_chunked { declared; _ } ->
      [
        ("Content-Length", Int.to_string declared);
        ("Transfer-Encoding", "chunked");
      ]
  | Early_close actual ->
      [ ("Content-Length", Int.to_string (String.length actual + 1)) ]
  | Chunked _ | Malformed_chunked _ -> [ ("Transfer-Encoding", "chunked") ]
  | Malformed_header_block _ -> []

let chunk_wire chunk = Printf.sprintf "%x\r\n%s\r\n" (String.length chunk) chunk

let body_for_framing = function
  | Empty -> ""
  | Content_length { actual; _ } -> actual
  | Duplicate_content_length { actual; _ } -> actual
  | Conflicting_length_and_chunked { chunks; _ } ->
      String.concat (List.map chunks ~f:chunk_wire) ^ "0\r\n\r\n"
  | Early_close actual -> actual
  | Chunked chunks ->
      String.concat (List.map chunks ~f:chunk_wire) ^ "0\r\n\r\n"
  | Malformed_chunked wire -> wire
  | Malformed_header_block _ -> ""

let response_headers scenario =
  scenario.headers
  @ headers_for_framing scenario.framing
  @ [ connection_header scenario.connection ]

let header_line (name, value) = Printf.sprintf "%s: %s\r\n" name value
let header_block headers = String.concat (List.map headers ~f:header_line)

let raw_header_block block =
  if String.is_suffix block ~suffix:"\r\n" then block else block ^ "\r\n"

let response_header_block scenario =
  let framing_header_block =
    match scenario.framing with
    | Malformed_header_block block -> raw_header_block block
    | framing -> header_block (headers_for_framing framing)
  in
  header_block (scenario.headers @ [ connection_header scenario.connection ])
  ^ framing_header_block

let expected_content_length_body ~declared ~actual =
  let actual_length = String.length actual in
  if Int.equal declared actual_length then Body actual
  else if declared < actual_length then Body (String.prefix actual declared)
  else Body_error

let visible_header_lines_and_hidden_body block =
  let block = raw_header_block block in
  let rec loop offset headers =
    match String.substr_index ~pos:offset block ~pattern:"\r\n" with
    | None -> (List.rev headers, "")
    | Some line_end -> (
        let line = String.sub block ~pos:offset ~len:(line_end - offset) in
        let next = line_end + 2 in
        if String.is_empty line then
          (List.rev headers, String.drop_prefix block next)
        else
          match String.lsplit2 line ~on:':' with
          | Some (name, value) ->
              loop next ((String.strip name, String.strip value) :: headers)
          | None -> (List.rev headers, String.drop_prefix block next ^ "\r\n"))
  in
  loop 0 []

let header_values name headers =
  List.filter_map headers ~f:(fun (candidate, value) ->
      if String.Caseless.equal candidate name then Some value else None)

let comma_separated_header_values ?(drop_empty = true) values =
  values
  |> List.concat_map ~f:(fun value -> String.split value ~on:',')
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun value ->
      (not drop_empty) || not (String.is_empty value))

let ascii_digits value =
  (not (String.is_empty value))
  && String.for_all value ~f:(function '0' .. '9' -> true | _ -> false)

let visible_content_length_error headers =
  match
    header_values "Content-Length" headers
    |> comma_separated_header_values ~drop_empty:false
  with
  | [] -> false
  | value :: values -> (
      if not (ascii_digits value) then true
      else
        match Stdlib.Int64.of_string_opt value with
        | None -> true
        | Some first ->
            List.exists values ~f:(fun value ->
                (not (ascii_digits value))
                ||
                match Stdlib.Int64.of_string_opt value with
                | None -> true
                | Some length -> not (Int64.equal first length)))

let visible_content_length_present headers =
  not (List.is_empty (header_values "Content-Length" headers))

let visible_transfer_encoding_values headers =
  comma_separated_header_values (header_values "Transfer-Encoding" headers)

let visible_transfer_encoding_present headers =
  not (List.is_empty (visible_transfer_encoding_values headers))

let visible_header_rejects_before_body_consumer headers =
  visible_content_length_error headers
  || visible_content_length_present headers
     && visible_transfer_encoding_present headers

let bodiless_header_rejects_before_body_consumer headers =
  visible_content_length_error headers

let visible_header_pairs_for_framing = function
  | Malformed_header_block block ->
      fst (visible_header_lines_and_hidden_body block)
  | framing -> headers_for_framing framing

let visible_headers_reject_before_body_consumer ~headers ~bodiless framing =
  let headers = headers @ visible_header_pairs_for_framing framing in
  if bodiless then bodiless_header_rejects_before_body_consumer headers
  else visible_header_rejects_before_body_consumer headers

let malformed_header_block_rejects_before_body_consumer ~bodiless block =
  let headers, _hidden_body = visible_header_lines_and_hidden_body block in
  if bodiless then bodiless_header_rejects_before_body_consumer headers
  else visible_header_rejects_before_body_consumer headers

let malformed_header_block_hidden_body block =
  let _headers, hidden_body = visible_header_lines_and_hidden_body block in
  hidden_body

let rejects_before_body_consumer scenario =
  visible_headers_reject_before_body_consumer ~headers:scenario.headers
    ~bodiless:(is_bodiless scenario) scenario.framing

let expected_body_for ~headers ~bodiless framing =
  match framing with
  | Malformed_header_block block
    when malformed_header_block_rejects_before_body_consumer ~bodiless block ->
      Body_error
  | _
    when visible_headers_reject_before_body_consumer ~headers ~bodiless framing
    ->
      Body_error
  | Duplicate_content_length { first; actual; _ } ->
      if bodiless then No_body
      else expected_content_length_body ~declared:first ~actual
  | Conflicting_length_and_chunked _ -> if bodiless then No_body else Body_error
  | _ when bodiless -> No_body
  | Malformed_header_block block ->
      Body (malformed_header_block_hidden_body block)
  | Early_close _ -> Body_error
  | Empty -> Body ""
  | Content_length { declared; actual } ->
      expected_content_length_body ~declared ~actual
  | Chunked chunks -> Body (String.concat chunks)
  | Malformed_chunked _ -> Body_error

let method_bin scenario =
  "http.method." ^ String.lowercase (method_to_string scenario.method_)

let status_bin scenario =
  if is_bodiless_status scenario.status then "http.status.bodiless"
  else if scenario.status >= 400 then "http.status.error"
  else "http.status.body"

let framing_bin = function
  | Empty -> "http.framing.empty"
  | Content_length { declared; actual } ->
      if Int.equal declared (String.length actual) then
        "http.framing.content-length.exact"
      else if declared > String.length actual then
        "http.framing.content-length.underflow"
      else "http.framing.content-length.overflow"
  | Duplicate_content_length { first; second; _ } ->
      if Int.equal first second then
        "http.framing.duplicate-content-length.equal"
      else "http.framing.duplicate-content-length.mismatch"
  | Conflicting_length_and_chunked _ ->
      "http.framing.conflicting-length-and-chunked"
  | Early_close _ -> "http.framing.early-close"
  | Chunked _ -> "http.framing.chunked"
  | Malformed_chunked _ -> "http.framing.malformed-chunked"
  | Malformed_header_block _ -> "http.framing.malformed-header-block"

let expected_body_bin = function
  | No_body -> "http.expected.no-body"
  | Body _ -> "http.expected.body"
  | Body_error -> "http.expected.body-error"

let consume_bin = function
  | Read_all -> "http.consume.read-all"
  | Read_once _ -> "http.consume.read-once"
  | Drop_without_read -> "http.consume.drop-without-read"
  | Raise_in_consume -> "http.consume.raise-in-consume"

let coverage_bins scenario =
  [
    method_bin scenario;
    status_bin scenario;
    framing_bin scenario.framing;
    consume_bin scenario.consume;
    expected_body_bin scenario.expected_body;
  ]

let scenario ~name ~method_ ~status ?(headers = []) ~framing ~connection
    ?(consume = Read_all) () =
  let partial =
    {
      name;
      method_;
      status;
      headers;
      framing;
      connection;
      consume;
      expected_body = No_body;
    }
  in
  {
    partial with
    expected_body =
      expected_body_for ~headers ~bodiless:(is_bodiless partial) framing;
  }

let framing_to_string = function
  | Empty -> "empty"
  | Content_length { declared; actual } ->
      Printf.sprintf "content-length declared=%d actual=%d" declared
        (String.length actual)
  | Duplicate_content_length { first; second; actual } ->
      Printf.sprintf "duplicate-content-length first=%d second=%d actual=%d"
        first second (String.length actual)
  | Conflicting_length_and_chunked { declared; chunks } ->
      Printf.sprintf "conflicting-length-and-chunked declared=%d chunks=[%s]"
        declared
        (String.concat ~sep:","
           (List.map chunks ~f:(fun chunk ->
                Int.to_string (String.length chunk))))
  | Early_close actual ->
      Printf.sprintf "early-close declared=%d actual=%d"
        (String.length actual + 1)
        (String.length actual)
  | Chunked chunks ->
      Printf.sprintf "chunked chunks=[%s]"
        (String.concat ~sep:","
           (List.map chunks ~f:(fun chunk ->
                Int.to_string (String.length chunk))))
  | Malformed_chunked wire ->
      Printf.sprintf "malformed-chunked bytes=%d" (String.length wire)
  | Malformed_header_block block ->
      Printf.sprintf "malformed-header-block bytes=%d" (String.length block)

let connection_to_string = function
  | Close -> "close"
  | Keep_alive -> "keep-alive"

let expected_body_to_string = function
  | No_body -> "no-body"
  | Body body -> Printf.sprintf "body(%d)" (String.length body)
  | Body_error -> "body-error"

let consume_to_string = function
  | Read_all -> "read-all"
  | Read_once n -> Printf.sprintf "read-once(%d)" n
  | Drop_without_read -> "drop-without-read"
  | Raise_in_consume -> "raise-in-consume"

let observed_to_string = function
  | Observed_body body -> Printf.sprintf "body(%d):%S" (String.length body) body
  | Observed_error error -> Printf.sprintf "error:%s" error
  | Observed_exception -> "exception"

let expected_observation scenario =
  match
    ( rejects_before_body_consumer scenario,
      scenario.consume,
      scenario.expected_body )
  with
  | true, _, _ -> `Body_error
  | false, Raise_in_consume, _ -> `Exception_preserved
  | false, Drop_without_read, _ -> `No_body_required
  | false, Read_once n, Body body ->
      `Body_prefix (String.prefix body (Int.min n (String.length body)))
  | false, Read_once _, No_body -> `Body_prefix ""
  | false, Read_once _, Body_error -> `Body_error
  | false, Read_all, No_body -> `Body ""
  | false, Read_all, Body body -> `Body body
  | false, Read_all, Body_error -> `Body_error

let to_string scenario =
  Printf.sprintf
    "%s method=%s status=%d framing=%s connection=%s consume=%s expected=%s"
    scenario.name
    (method_to_string scenario.method_)
    scenario.status
    (framing_to_string scenario.framing)
    (connection_to_string scenario.connection)
    (consume_to_string scenario.consume)
    (expected_body_to_string scenario.expected_body)
