let decode_with_context ~what message =
  Awskit.Error.Producer.decode message
  |> Awskit.Error.Producer.with_context (Fmt.str "decoding %s" what)

let el name children = `El ((("", name), []), children)
let text name value = el name [ `Data value ]
let to_string node = Ezxmlm.to_string [ node ]

let root body =
  try
    let _, nodes = Ezxmlm.from_string body in
    match
      List.find_map
        (function
          | `El (((_, name), _), children) -> Some (name, children) | _ -> None)
        nodes
    with
    | Some root -> Ok root
    | None ->
        Error (decode_with_context ~what:"XML document" "empty XML document")
  with exn ->
    Error (decode_with_context ~what:"XML document" (Printexc.to_string exn))

let children name nodes =
  List.filter_map
    (function
      | `El (((_, child_name), _), children) when String.equal child_name name
        ->
          Some children
      | _ -> None)
    nodes

let rec text_content nodes =
  nodes
  |> List.map (function
    | `Data data -> data
    | `El ((_, _), children) -> text_content children)
  |> String.concat ""

let child name nodes =
  match children name nodes with [] -> None | x :: _ -> Some x

let child_text name nodes = Option.map text_content (child name nodes)
let child_texts name nodes = List.map text_content (children name nodes)

let decode_field_error ~path fmt =
  Fmt.kstr
    (fun message ->
      Error
        (decode_with_context ~what:path
           (Fmt.str "invalid XML field: %s" message)))
    fmt

let required_child_text ~path name nodes =
  match child_text name nodes with
  | Some value -> Ok value
  | None -> decode_field_error ~path "missing required <%s>" name

let optional_child_parse ~path name parse nodes =
  match child_text name nodes with
  | None -> Ok None
  | Some value -> (
      match parse value with
      | Some parsed -> Ok (Some parsed)
      | None -> decode_field_error ~path "<%s> has invalid value %S" name value)

let optional_child_result ~path name parse nodes =
  match child_text name nodes with
  | None -> Ok None
  | Some value -> (
      match parse value with
      | Ok parsed -> Ok (Some parsed)
      | Error error ->
          decode_field_error ~path "<%s> has invalid value %S: %s" name value
            (Awskit.Error.to_string_hum error))

let children_result name nodes ~f =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | child :: rest -> (
        match f index child with
        | Error _ as error -> error
        | Ok value -> loop (index + 1) (value :: acc) rest)
  in
  loop 0 [] (children name nodes)

let decode_root body ~name =
  match root body with
  | Error _ as error -> error
  | Ok (actual, children) when String.equal actual name -> Ok children
  | Ok (actual, _) ->
      Error
        (decode_with_context ~what:"XML document"
           (Fmt.str "expected %s root element, got %s" name actual))

type service_error = {
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
}

let empty_service_error =
  { code = None; message = None; request_id = None; host_id = None }

let non_empty_child_text name nodes =
  match child_text name nodes with
  | None -> None
  | Some value ->
      let value = String.trim value in
      if String.equal value "" then None else Some value

let service_error body =
  match root body with
  | Error _ -> empty_service_error
  | Ok (root_name, _) when not (String.equal root_name "Error") ->
      empty_service_error
  | Ok (_, nodes) ->
      {
        code = non_empty_child_text "Code" nodes;
        message = non_empty_child_text "Message" nodes;
        request_id =
          (match non_empty_child_text "RequestId" nodes with
          | Some _ as request_id -> request_id
          | None -> non_empty_child_text "RequestID" nodes);
        host_id = non_empty_child_text "HostId" nodes;
      }

let service_code body = (service_error body).code
let service_message body = (service_error body).message
