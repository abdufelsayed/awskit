open Base

let encode ?(encode_slash = true) value =
  let buf = Buffer.create (String.length value) in
  String.iter value ~f:(fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.' ->
          Buffer.add_char buf c
      | '/' when not encode_slash -> Buffer.add_char buf c
      | c -> Buffer.add_string buf (Fmt.str "%%%02X" (Char.to_int c)));
  Buffer.contents buf

let expanded_query_pairs params =
  List.concat_map params ~f:(fun (key, values) ->
      match values with
      | [] -> [ (key, "") ]
      | values -> List.map values ~f:(fun value -> (key, value)))

let render_query_pairs pairs =
  pairs
  |> List.map ~f:(fun (key, value) ->
      Fmt.str "%s=%s" (encode key) (encode value))
  |> String.concat ~sep:"&"

let render_query_params params =
  expanded_query_pairs params |> render_query_pairs

let canonical_query_params params =
  expanded_query_pairs params
  |> List.map ~f:(fun (key, value) -> (encode key, encode value))
  |> List.sort ~compare:(fun (key_a, value_a) (key_b, value_b) ->
      let key_compare = String.compare key_a key_b in
      if key_compare <> 0 then key_compare else String.compare value_a value_b)
  |> List.map ~f:(fun (key, value) -> key ^ "=" ^ value)
  |> String.concat ~sep:"&"
