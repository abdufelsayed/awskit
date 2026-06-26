open Base
module Model = Runtime_http_model

type t = { path : string; scenario : Model.scenario }
type parse_error = { line : int; source : string; message : string }

let ( let* ) result f = Result.bind result ~f

let parse_error_to_string ~path error =
  Printf.sprintf "%s:%d: %s: %S" path error.line error.message error.source

let parse_error ~line ~source message = Error { line; source; message }

let hex_char value =
  let digits = "0123456789abcdef" in
  digits.[value land 0x0f]

let hex_encode text =
  String.init
    (String.length text * 2)
    ~f:(fun index ->
      let byte = Char.to_int text.[index / 2] in
      if index % 2 = 0 then hex_char (byte lsr 4) else hex_char byte)

let replay_string text = "h" ^ hex_encode text

let replay_string_list values =
  values |> List.map ~f:replay_string |> String.concat ~sep:","

let headers_directive headers =
  headers
  |> List.concat_map ~f:(fun (name, value) ->
      [ replay_string name; replay_string value ])
  |> fun fields ->
  String.concat ~sep:" " (Int.to_string (List.length headers) :: fields)

let replay_framing_directive = function
  | Model.Empty -> "empty"
  | Content_length { declared; actual } ->
      Printf.sprintf "content-length %d %s" declared (replay_string actual)
  | Duplicate_content_length { first; second; actual } ->
      Printf.sprintf "duplicate-content-length %d %d %s" first second
        (replay_string actual)
  | Conflicting_length_and_chunked { declared; chunks } ->
      Printf.sprintf "conflicting-length-and-chunked %d %s" declared
        (replay_string_list chunks)
  | Early_close actual -> Printf.sprintf "early-close %s" (replay_string actual)
  | Chunked chunks -> Printf.sprintf "chunked %s" (replay_string_list chunks)
  | Malformed_chunked wire ->
      Printf.sprintf "malformed-chunked %s" (replay_string wire)
  | Malformed_header_block block ->
      Printf.sprintf "malformed-header-block %s" (replay_string block)

let replay_consume_directive = function
  | Model.Read_all -> "read-all"
  | Read_once n -> Printf.sprintf "read-once:%d" n
  | Drop_without_read -> "drop-without-read"
  | Raise_in_consume -> "raise-in-consume"

let scenario_to_replay_fixture scenario =
  String.concat ~sep:"\n"
    [
      "runtime-http-replay-v1";
      "name=" ^ scenario.Model.name;
      "method=" ^ Model.method_to_string scenario.method_;
      Printf.sprintf "status=%d" scenario.status;
      "headers=" ^ headers_directive scenario.headers;
      "framing=" ^ replay_framing_directive scenario.framing;
      "connection=" ^ Model.connection_to_string scenario.connection;
      "consume=" ^ replay_consume_directive scenario.consume;
    ]
  ^ "\n"

let concat_many = function
  | [] -> invalid_arg "concat_many"
  | head :: rest -> List.fold rest ~init:head ~f:Stdlib.Filename.concat

let fixture_dir_candidates =
  [
    concat_many [ "test"; "fixtures"; "runtime-http-replay" ];
    concat_many [ ".."; ".."; "fixtures"; "runtime-http-replay" ];
  ]

let is_directory path =
  Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path

let fixture_dir () =
  match List.find fixture_dir_candidates ~f:is_directory with
  | Some dir -> dir
  | None ->
      invalid_arg
        (Printf.sprintf "runtime HTTP replay fixture directory not found: %s"
           (String.concat ~sep:", " fixture_dir_candidates))

let read_file path =
  let channel = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in_noerr channel)
    (fun () ->
      let length = Stdlib.in_channel_length channel in
      Stdlib.really_input_string channel length)

let split_once_on_equal ~line source =
  match String.lsplit2 source ~on:'=' with
  | Some (key, value) -> Ok (key, value)
  | None -> parse_error ~line ~source "expected key=value line"

let parse_field ~line expected_key source =
  let* key, value = split_once_on_equal ~line source in
  if String.equal key expected_key then Ok value
  else
    parse_error ~line ~source
      (Printf.sprintf "expected %S field, found %S" expected_key key)

let split_lines text =
  let lines = String.split text ~on:'\n' in
  match List.rev lines with "" :: rest -> List.rev rest | _ -> lines

let hex_value = function
  | '0' .. '9' as char -> Some (Char.to_int char - Char.to_int '0')
  | 'a' .. 'f' as char -> Some (Char.to_int char - Char.to_int 'a' + 10)
  | 'A' .. 'F' as char -> Some (Char.to_int char - Char.to_int 'A' + 10)
  | _ -> None

let decode_hex ~line ~source token =
  let length = String.length token in
  if length = 0 || not (Char.equal token.[0] 'h') then
    parse_error ~line ~source "expected h-prefixed hex string"
  else if (length - 1) % 2 <> 0 then
    parse_error ~line ~source "hex string has odd length"
  else
    let output = Bytes.create ((length - 1) / 2) in
    let rec loop source_index output_index =
      if source_index = length then Ok (Bytes.to_string output)
      else
        match
          (hex_value token.[source_index], hex_value token.[source_index + 1])
        with
        | Some high, Some low ->
            Bytes.set output output_index
              (Char.of_int_exn ((high lsl 4) lor low));
            loop (source_index + 2) (output_index + 1)
        | None, _ | _, None ->
            parse_error ~line ~source "hex string contains non-hex digit"
    in
    loop 1 0

let parse_int ~line ~source value =
  match Int.of_string_opt value with
  | Some value -> Ok value
  | None -> parse_error ~line ~source "expected integer"

let parse_headers ~line ~source value =
  let rec loop count tokens acc =
    if count = 0 then
      match tokens with
      | [] -> Ok (List.rev acc)
      | _ :: _ -> parse_error ~line ~source "headers has extra tokens"
    else
      match tokens with
      | name :: value :: rest ->
          let* name = decode_hex ~line ~source name in
          let* value = decode_hex ~line ~source value in
          loop (count - 1) rest ((name, value) :: acc)
      | [] | [ _ ] -> parse_error ~line ~source "headers expected pair tokens"
  in
  match
    String.split value ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  with
  | count :: rest ->
      let* count = parse_int ~line ~source count in
      loop count rest []
  | [] -> parse_error ~line ~source "headers expected count"

let parse_method ~line ~source = function
  | "GET" -> Ok `GET
  | "HEAD" -> Ok `HEAD
  | "PUT" -> Ok `PUT
  | "POST" -> Ok `POST
  | "DELETE" -> Ok `DELETE
  | "PATCH" -> Ok `PATCH
  | value ->
      parse_error ~line ~source (Printf.sprintf "unknown method %S" value)

let parse_connection ~line ~source = function
  | "close" -> Ok Model.Close
  | "keep-alive" -> Ok Model.Keep_alive
  | value ->
      parse_error ~line ~source (Printf.sprintf "unknown connection %S" value)

let parse_consume ~line ~source = function
  | "read-all" -> Ok Model.Read_all
  | "drop-without-read" -> Ok Model.Drop_without_read
  | "raise-in-consume" -> Ok Model.Raise_in_consume
  | value when String.is_prefix value ~prefix:"read-once:" ->
      let size = String.drop_prefix value (String.length "read-once:") in
      let* size = parse_int ~line ~source size in
      Ok (Model.Read_once size)
  | value ->
      parse_error ~line ~source (Printf.sprintf "unknown consume mode %S" value)

let decode_hex_list ~line ~source value =
  if String.is_empty value then Ok []
  else
    value
    |> String.split ~on:','
    |> List.map ~f:(decode_hex ~line ~source)
    |> Result.all

let parse_framing ~line ~source value =
  match String.split value ~on:' ' with
  | [ "empty" ] -> Ok Model.Empty
  | [ "content-length"; declared; actual ] ->
      let* declared = parse_int ~line ~source declared in
      let* actual = decode_hex ~line ~source actual in
      Ok (Model.Content_length { declared; actual })
  | [ "duplicate-content-length"; first; second; actual ] ->
      let* first = parse_int ~line ~source first in
      let* second = parse_int ~line ~source second in
      let* actual = decode_hex ~line ~source actual in
      Ok (Model.Duplicate_content_length { first; second; actual })
  | [ "conflicting-length-and-chunked"; declared; chunks ] ->
      let* declared = parse_int ~line ~source declared in
      let* chunks = decode_hex_list ~line ~source chunks in
      Ok (Model.Conflicting_length_and_chunked { declared; chunks })
  | [ "early-close"; actual ] ->
      let* actual = decode_hex ~line ~source actual in
      Ok (Model.Early_close actual)
  | [ "chunked"; chunks ] ->
      let* chunks = decode_hex_list ~line ~source chunks in
      Ok (Model.Chunked chunks)
  | [ "malformed-chunked"; wire ] ->
      let* wire = decode_hex ~line ~source wire in
      Ok (Model.Malformed_chunked wire)
  | [ "malformed-header-block"; block ] ->
      let* block = decode_hex ~line ~source block in
      Ok (Model.Malformed_header_block block)
  | _ -> parse_error ~line ~source "unknown framing directive"

let parse_record path text =
  match split_lines text with
  | [
   "runtime-http-replay-v1";
   name_line;
   method_line;
   status_line;
   headers_line;
   framing_line;
   connection_line;
   consume_line;
  ] ->
      let* name = parse_field ~line:2 "name" name_line in
      let* method_value = parse_field ~line:3 "method" method_line in
      let* method_ = parse_method ~line:3 ~source:method_line method_value in
      let* status_value = parse_field ~line:4 "status" status_line in
      let* status = parse_int ~line:4 ~source:status_line status_value in
      let* headers_value = parse_field ~line:5 "headers" headers_line in
      let* headers = parse_headers ~line:5 ~source:headers_line headers_value in
      let* framing_value = parse_field ~line:6 "framing" framing_line in
      let* framing = parse_framing ~line:6 ~source:framing_line framing_value in
      let* connection_value =
        parse_field ~line:7 "connection" connection_line
      in
      let* connection =
        parse_connection ~line:7 ~source:connection_line connection_value
      in
      let* consume_value = parse_field ~line:8 "consume" consume_line in
      let* consume = parse_consume ~line:8 ~source:consume_line consume_value in
      Ok
        {
          path;
          scenario =
            Model.scenario ~name ~method_ ~status ~headers ~framing ~connection
              ~consume ();
        }
  | first :: _ ->
      parse_error ~line:1 ~source:first
        "expected runtime-http-replay-v1 and seven replay fields"
  | [] ->
      parse_error ~line:1 ~source:""
        "expected runtime-http-replay-v1 and seven replay fields"

let is_replay_file path =
  (not (Stdlib.Sys.is_directory path))
  && not (String.equal (Stdlib.Filename.basename path) "README.md")

let sorted_files dir =
  Stdlib.Sys.readdir dir
  |> Array.to_list
  |> List.map ~f:(Stdlib.Filename.concat dir)
  |> List.filter ~f:is_replay_file
  |> List.sort ~compare:String.compare

let read_replay path =
  match parse_record path (read_file path) with
  | Ok replay -> replay
  | Error error -> failwith (parse_error_to_string ~path error)

let all () = fixture_dir () |> sorted_files |> List.map ~f:read_replay
