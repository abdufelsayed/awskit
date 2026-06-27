let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      really_input_string channel length)

let trim_final_newline text =
  let length = String.length text in
  if length > 0 && Char.equal text.[length - 1] '\n' then
    String.sub text 0 (length - 1)
  else text

let read_text_fixture_value path = read_file path |> trim_final_newline

let first_difference left right =
  let left_length = String.length left in
  let right_length = String.length right in
  let limit = min left_length right_length in
  let rec loop index =
    if index = limit then
      if left_length = right_length then None else Some index
    else if Char.equal left.[index] right.[index] then loop (index + 1)
    else Some index
  in
  loop 0

let line_column text index =
  let rec loop cursor line column =
    if cursor = index then (line, column)
    else if Char.equal text.[cursor] '\n' then loop (cursor + 1) (line + 1) 1
    else loop (cursor + 1) line (column + 1)
  in
  loop 0 1 1

let check_string label ~expected ~actual =
  let expected = trim_final_newline expected in
  let actual = trim_final_newline actual in
  match first_difference expected actual with
  | None -> ()
  | Some index ->
      let line, column =
        line_column expected (min index (String.length expected))
      in
      Alcotest.failf
        "%s differs at byte %d, line %d, column %d\nexpected: %S\nactual:   %S"
        label index line column expected actual

let check_file label path ~actual =
  check_string label ~expected:(read_file path) ~actual

let sort_query query =
  query
  |> List.map (fun (key, values) -> (key, List.sort String.compare values))
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let normalize_uri ?(redact = []) uri =
  let should_redact key = List.exists (String.equal key) redact in
  let uri = Uri.of_string uri in
  let query =
    Uri.query uri
    |> List.map (fun (key, values) ->
        if should_redact key then (key, [ "REDACTED" ]) else (key, values))
    |> sort_query
  in
  Uri.with_query uri query |> Uri.to_string
