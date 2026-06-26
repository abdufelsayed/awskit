type t = S3_command.t list
type parse_error = { line : int; source : string; message : string }

let ( let* ) = Result.bind

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

let split_args rest = String.split_on_char ' ' rest |> List.filter (( <> ) "")
let hex_digit value = String.unsafe_get "0123456789abcdef" (value land 0xf)

let encode_string value =
  let output = Bytes.create ((String.length value * 2) + 1) in
  Bytes.set output 0 'h';
  String.iteri
    (fun index char ->
      let code = Char.code char in
      Bytes.set output ((index * 2) + 1) (hex_digit (code lsr 4));
      Bytes.set output ((index * 2) + 2) (hex_digit code))
    value;
  Bytes.unsafe_to_string output

let hex_value = function
  | '0' .. '9' as char -> Some (Char.code char - Char.code '0')
  | 'a' .. 'f' as char -> Some (Char.code char - Char.code 'a' + 10)
  | 'A' .. 'F' as char -> Some (Char.code char - Char.code 'A' + 10)
  | _ -> None

let decode_string ~line ~source token =
  let length = String.length token in
  if length = 0 || not (Char.equal token.[0] 'h') then
    parse_error ~line ~source "expected escaped string token"
  else if (length - 1) mod 2 <> 0 then
    parse_error ~line ~source "escaped string token has odd hex length"
  else
    let output = Bytes.create ((length - 1) / 2) in
    let rec loop source_index output_index =
      if source_index = length then Ok (Bytes.unsafe_to_string output)
      else
        match
          (hex_value token.[source_index], hex_value token.[source_index + 1])
        with
        | Some high, Some low ->
            Bytes.set output output_index (Char.chr ((high lsl 4) lor low));
            loop (source_index + 2) (output_index + 1)
        | _ -> parse_error ~line ~source "escaped string token contains non-hex"
    in
    loop 1 0

let encode_pairs pairs =
  string_of_int (List.length pairs)
  :: List.concat_map
       (fun (key, value) -> [ encode_string key; encode_string value ])
       pairs

let decode_int ~line ~source token =
  match int_of_string_opt token with
  | Some value when value >= 0 -> Ok value
  | Some _ | None -> parse_error ~line ~source "expected non-negative integer"

let decode_versioning_status ~line ~source = function
  | "Enabled" -> Ok Awskit_s3.Bucket.Versioning.Status.Enabled
  | "Suspended" -> Ok Suspended
  | _ ->
      parse_error ~line ~source
        "expected versioning status Enabled or Suspended"

let decode_pairs ~line ~source tokens =
  match tokens with
  | [] -> parse_error ~line ~source "expected pair count"
  | count :: rest ->
      let* count = decode_int ~line ~source count in
      let rec loop count acc tokens =
        if count = 0 then Ok (List.rev acc, tokens)
        else
          match tokens with
          | key :: value :: rest ->
              let* key = decode_string ~line ~source key in
              let* value = decode_string ~line ~source value in
              loop (count - 1) ((key, value) :: acc) rest
          | [] | [ _ ] -> parse_error ~line ~source "expected encoded pair"
      in
      loop count [] rest

let encode_optional_string = function
  | None -> [ "none" ]
  | Some value -> [ "some"; encode_string value ]

let decode_optional_string ~line ~source = function
  | "none" :: rest -> Ok (None, rest)
  | "some" :: value :: rest ->
      let* value = decode_string ~line ~source value in
      Ok (Some value, rest)
  | [] | _ :: _ ->
      parse_error ~line ~source "expected optional string token none or some"

let expect_no_args ~line ~source command tokens value =
  match tokens with
  | [] -> Ok value
  | _ :: _ ->
      parse_error ~line ~source
        (Printf.sprintf "%s does not take more arguments" command)

let encode_versioning_status status =
  encode_string (Awskit_s3.Bucket.Versioning.Status.to_string status)

let encode_copy_metadata = function
  | S3_command.Copy_source_metadata -> [ "copy-source-metadata" ]
  | Replace_metadata metadata -> "replace-metadata" :: encode_pairs metadata

let encode_command command =
  let command, args =
    match command with
    | S3_command.Put_string (key, body, tags) ->
        ( "put-string",
          [ encode_string key; encode_string body ] @ encode_pairs tags )
    | Put_string_metadata (key, body, tags, metadata) ->
        ( "put-string-metadata",
          [ encode_string key; encode_string body ]
          @ encode_pairs tags
          @ encode_pairs metadata )
    | Get_string key -> ("get-string", [ encode_string key ])
    | Find_string key -> ("find-string", [ encode_string key ])
    | Head_object key -> ("head-object", [ encode_string key ])
    | Exists_object key -> ("exists-object", [ encode_string key ])
    | Delete_object key -> ("delete-object", [ encode_string key ])
    | List_keys -> ("list-keys", [])
    | List_prefix prefix -> ("list-prefix", [ encode_string prefix ])
    | List_keys_page { prefix; max_keys } ->
        ( "list-keys-page",
          encode_optional_string prefix @ [ string_of_int max_keys ] )
    | List_versions_page { max_keys } ->
        ("list-versions-page", [ string_of_int max_keys ])
    | Copy_object (source_key, destination_key) ->
        ( "copy-object",
          [ encode_string source_key; encode_string destination_key ] )
    | Copy_object_metadata (source_key, destination_key, metadata) ->
        ( "copy-object-metadata",
          [ encode_string source_key; encode_string destination_key ]
          @ encode_copy_metadata metadata )
    | Put_object_tags (key, tags) ->
        ("put-object-tags", encode_string key :: encode_pairs tags)
    | Get_object_tags key -> ("get-object-tags", [ encode_string key ])
    | Delete_object_tags key -> ("delete-object-tags", [ encode_string key ])
    | Put_bucket_tags tags -> ("put-bucket-tags", encode_pairs tags)
    | Get_bucket_tags -> ("get-bucket-tags", [])
    | Delete_bucket_tags -> ("delete-bucket-tags", [])
    | Put_versioning status ->
        ("put-versioning", [ encode_versioning_status status ])
    | Get_versioning -> ("get-versioning", [])
  in
  String.concat " " ("replay-v1" :: command :: args)

let encode commands = commands |> List.map encode_command |> String.concat "\n"

let decode_one_string ~line ~source tokens make =
  match tokens with
  | [ value ] ->
      let* value = decode_string ~line ~source value in
      Ok (make value)
  | [] | _ :: _ :: _ -> parse_error ~line ~source "expected one string argument"

let decode_two_strings ~line ~source tokens make =
  match tokens with
  | [ left; right ] ->
      let* left = decode_string ~line ~source left in
      let* right = decode_string ~line ~source right in
      Ok (make left right)
  | [] | [ _ ] | _ :: _ :: _ :: _ ->
      parse_error ~line ~source "expected two string arguments"

let decode_put_string ~line ~source tokens =
  match tokens with
  | key :: body :: rest ->
      let* key = decode_string ~line ~source key in
      let* body = decode_string ~line ~source body in
      let* tags, rest = decode_pairs ~line ~source rest in
      expect_no_args ~line ~source "put-string" rest
        (S3_command.Put_string (key, body, tags))
  | [] | [ _ ] -> parse_error ~line ~source "expected put-string arguments"

let decode_put_string_metadata ~line ~source tokens =
  match tokens with
  | key :: body :: rest ->
      let* key = decode_string ~line ~source key in
      let* body = decode_string ~line ~source body in
      let* tags, rest = decode_pairs ~line ~source rest in
      let* metadata, rest = decode_pairs ~line ~source rest in
      expect_no_args ~line ~source "put-string-metadata" rest
        (S3_command.Put_string_metadata (key, body, tags, metadata))
  | [] | [ _ ] ->
      parse_error ~line ~source "expected put-string-metadata arguments"

let decode_list_keys_page ~line ~source tokens =
  let* prefix, rest = decode_optional_string ~line ~source tokens in
  match rest with
  | [ max_keys ] ->
      let* max_keys = decode_int ~line ~source max_keys in
      Ok (S3_command.List_keys_page { prefix; max_keys })
  | [] | _ :: _ :: _ ->
      parse_error ~line ~source "expected list-keys-page max_keys"

let decode_copy_metadata ~line ~source tokens =
  match tokens with
  | [ "copy-source-metadata" ] -> Ok S3_command.Copy_source_metadata
  | "replace-metadata" :: rest ->
      let* metadata, rest = decode_pairs ~line ~source rest in
      expect_no_args ~line ~source "copy-object-metadata" rest
        (S3_command.Replace_metadata metadata)
  | [] | _ :: _ -> parse_error ~line ~source "expected copy metadata directive"

let decode_copy_object_metadata ~line ~source tokens =
  match tokens with
  | source_key :: destination_key :: rest ->
      let* source_key = decode_string ~line ~source source_key in
      let* destination_key = decode_string ~line ~source destination_key in
      let* metadata = decode_copy_metadata ~line ~source rest in
      Ok
        (S3_command.Copy_object_metadata (source_key, destination_key, metadata))
  | [] | [ _ ] ->
      parse_error ~line ~source "expected copy-object-metadata arguments"

let decode_put_object_tags ~line ~source tokens =
  match tokens with
  | key :: rest ->
      let* key = decode_string ~line ~source key in
      let* tags, rest = decode_pairs ~line ~source rest in
      expect_no_args ~line ~source "put-object-tags" rest
        (S3_command.Put_object_tags (key, tags))
  | [] -> parse_error ~line ~source "expected put-object-tags arguments"

let decode_put_bucket_tags ~line ~source tokens =
  let* tags, rest = decode_pairs ~line ~source tokens in
  expect_no_args ~line ~source "put-bucket-tags" rest
    (S3_command.Put_bucket_tags tags)

let decode_new_command ~line ~source command tokens =
  match command with
  | "put-string" -> decode_put_string ~line ~source tokens
  | "put-string-metadata" -> decode_put_string_metadata ~line ~source tokens
  | "get-string" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Get_string key)
  | "find-string" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Find_string key)
  | "head-object" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Head_object key)
  | "exists-object" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Exists_object key)
  | "delete-object" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Delete_object key)
  | "list-keys" ->
      expect_no_args ~line ~source command tokens S3_command.List_keys
  | "list-prefix" ->
      decode_one_string ~line ~source tokens (fun prefix ->
          S3_command.List_prefix prefix)
  | "list-keys-page" -> decode_list_keys_page ~line ~source tokens
  | "list-versions-page" -> (
      match tokens with
      | [ max_keys ] ->
          let* max_keys = decode_int ~line ~source max_keys in
          Ok (S3_command.List_versions_page { max_keys })
      | [] | _ :: _ :: _ ->
          parse_error ~line ~source "expected list-versions-page max_keys")
  | "copy-object" ->
      decode_two_strings ~line ~source tokens (fun source_key destination_key ->
          S3_command.Copy_object (source_key, destination_key))
  | "copy-object-metadata" -> decode_copy_object_metadata ~line ~source tokens
  | "put-object-tags" -> decode_put_object_tags ~line ~source tokens
  | "get-object-tags" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Get_object_tags key)
  | "delete-object-tags" ->
      decode_one_string ~line ~source tokens (fun key ->
          S3_command.Delete_object_tags key)
  | "put-bucket-tags" -> decode_put_bucket_tags ~line ~source tokens
  | "get-bucket-tags" ->
      expect_no_args ~line ~source command tokens S3_command.Get_bucket_tags
  | "delete-bucket-tags" ->
      expect_no_args ~line ~source command tokens S3_command.Delete_bucket_tags
  | "put-versioning" -> (
      match tokens with
      | [ status ] ->
          let* status = decode_string ~line ~source status in
          let* status = decode_versioning_status ~line ~source status in
          Ok (S3_command.Put_versioning status)
      | [] | _ :: _ :: _ ->
          parse_error ~line ~source "expected put-versioning status")
  | "get-versioning" ->
      expect_no_args ~line ~source command tokens S3_command.Get_versioning
  | unsupported ->
      parse_error ~line ~source
        (Printf.sprintf "unsupported replay-v1 command %S" unsupported)

let decode_command ~line ~source command rest =
  match (command, rest) with
  | "replay-v1", rest -> (
      match split_args rest with
      | command :: tokens -> decode_new_command ~line ~source command tokens
      | [] -> parse_error ~line ~source "expected replay-v1 command")
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
