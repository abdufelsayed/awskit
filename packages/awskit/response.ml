open Base

type t = { status : int; headers : (string * string) list; body : string }
[@@deriving show, eq]

let header t name =
  List.find_map t.headers ~f:(fun (k, v) ->
      if String.Caseless.equal k name then Some v else None)

let header_exn t name =
  match header t name with
  | Some v when not (String.is_empty v) -> Ok v
  | Some v -> Error (`Invalid_header (name, v))
  | None -> Error (`Missing_response_header name)

let is_success t = t.status >= 200 && t.status < 300

let header_int t name =
  match header t name with
  | None -> Ok None
  | Some value -> (
      try
        let parsed = Int.of_string value in
        if parsed < 0 then Error (`Invalid_header (name, value))
        else Ok (Some parsed)
      with exn ->
        Error
          (`Invalid_header (name, Fmt.str "%s (%s)" value (Exn.to_string exn))))

let header_int_exn t name =
  match header t name with
  | None -> Error (`Missing_response_header name)
  | Some value -> (
      try
        let parsed = Int.of_string value in
        if parsed < 0 then Error (`Invalid_header (name, value)) else Ok parsed
      with exn ->
        Error
          (`Invalid_header (name, Fmt.str "%s (%s)" value (Exn.to_string exn))))
