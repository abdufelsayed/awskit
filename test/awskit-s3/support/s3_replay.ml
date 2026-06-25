type t = S3_command.t list

let encode = S3_command.transcript

let validation line =
  Error
    (Awskit.Error.Producer.validation ~field:"s3_replay"
       (Printf.sprintf "unsupported replay line %S" line))

let strip_number line =
  match String.index_opt line '.' with
  | None -> String.trim line
  | Some dot ->
      let prefix = String.sub line 0 dot in
      if String.for_all (function '0' .. '9' -> true | _ -> false) prefix then
        String.trim (String.sub line (dot + 1) (String.length line - dot - 1))
      else String.trim line

let decode_line line =
  match strip_number line with
  | "" -> Ok None
  | "list-keys" -> Ok (Some S3_command.List_keys)
  | "get-bucket-tags" -> Ok (Some Get_bucket_tags)
  | "delete-bucket-tags" -> Ok (Some Delete_bucket_tags)
  | "get-versioning" -> Ok (Some Get_versioning)
  | unsupported -> validation unsupported

let decode input =
  let lines = String.split_on_char '\n' input in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest -> (
        match decode_line line with
        | Error _ as error -> error
        | Ok None -> loop acc rest
        | Ok (Some command) -> loop (command :: acc) rest)
  in
  loop [] lines
