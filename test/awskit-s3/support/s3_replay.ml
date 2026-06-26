type t = S3_command.t list
type parse_error = { line : int; source : string; message : string }

let parse_error_to_string error =
  Printf.sprintf "line %d: %s: %S" error.line error.message error.source

let parse_error ~line ~source message = Error { line; source; message }

let starts_with ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.equal prefix (String.sub value 0 prefix_length)

let split_command line =
  match String.index_opt line ' ' with
  | None -> (line, "")
  | Some index ->
      let command = String.sub line 0 index in
      let rest =
        String.sub line (index + 1) (String.length line - index - 1)
        |> String.trim
      in
      (command, rest)

let split_one_arg rest =
  match String.split_on_char ' ' rest |> List.filter (( <> ) "") with
  | [ arg ] -> Some arg
  | [] | _ :: _ :: _ -> None

let split_key_and_body rest =
  match String.index_opt rest ' ' with
  | None -> None
  | Some index ->
      let key = String.sub rest 0 index in
      let body =
        String.sub rest (index + 1) (String.length rest - index - 1)
        |> String.trim
      in
      if String.equal key "" || String.equal body "" then None
      else Some (key, body)

let versioning_status_of_string = function
  | "Enabled" -> Some Awskit_s3.Bucket.Versioning.Status.Enabled
  | "Suspended" -> Some Suspended
  | _ -> None

let encode_command = function
  | S3_command.Put_string (key, body, []) ->
      Some (Printf.sprintf "put %s %s" key body)
  | Delete_object key -> Some (Printf.sprintf "delete %s" key)
  | Put_versioning status ->
      Some
        (Printf.sprintf "put-versioning %s"
           (Awskit_s3.Bucket.Versioning.Status.to_string status))
  | Get_versioning -> Some "get-versioning"
  | Put_string (_, _, _ :: _)
  | Put_string_metadata _ | Get_string _ | Find_string _ | Head_object _
  | Exists_object _ | List_keys | List_prefix _ | List_keys_page _
  | List_versions_page _ | Copy_object _ | Copy_object_metadata _
  | Put_object_tags _ | Get_object_tags _ | Delete_object_tags _
  | Put_bucket_tags _ | Get_bucket_tags | Delete_bucket_tags ->
      None

let encode commands =
  let rec loop acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | command :: rest -> (
        match encode_command command with
        | Some line -> loop (line :: acc) rest
        | None ->
            invalid_arg
              (Printf.sprintf "S3_replay.encode cannot encode %s"
                 (S3_command.to_string command)))
  in
  loop [] commands

let decode_command ~line ~source command rest =
  match (command, rest) with
  | "put", rest -> (
      match split_key_and_body rest with
      | Some (key, body) -> Ok (S3_command.Put_string (key, body, []))
      | None -> parse_error ~line ~source "expected: put <key> <body>")
  | "put-versioning", rest -> (
      match split_one_arg rest with
      | Some status -> (
          match versioning_status_of_string status with
          | Some status -> Ok (S3_command.Put_versioning status)
          | None ->
              parse_error ~line ~source
                "expected versioning status Enabled or Suspended")
      | None -> parse_error ~line ~source "expected: put-versioning <status>")
  | "delete", rest -> (
      match split_one_arg rest with
      | Some key -> Ok (S3_command.Delete_object key)
      | None -> parse_error ~line ~source "expected: delete <key>")
  | "get-versioning", "" -> Ok S3_command.Get_versioning
  | "get-versioning", _ ->
      parse_error ~line ~source "get-versioning does not take arguments"
  | "", _ -> parse_error ~line ~source "missing replay command"
  | unsupported, _ ->
      parse_error ~line ~source
        (Printf.sprintf "unsupported replay command %S" unsupported)

let decode_line ~line source =
  let stripped = String.trim source in
  match stripped with
  | "" -> Ok None
  | comment when starts_with ~prefix:"# " comment -> Ok None
  | value ->
      let command, rest = split_command value in
      decode_command ~line ~source command rest
      |> Result.map (fun command -> Some command)

let decode input =
  let lines = String.split_on_char '\n' input in
  let rec loop line acc = function
    | [] -> Ok (List.rev acc)
    | source :: rest -> (
        match decode_line ~line source with
        | Error _ as error -> error
        | Ok None -> loop (line + 1) acc rest
        | Ok (Some command) -> loop (line + 1) (command :: acc) rest)
  in
  loop 1 [] lines
