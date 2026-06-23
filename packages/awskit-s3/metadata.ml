type entry = { key : string; value : Header_value.t }
type t = entry list

let prefix = "x-amz-meta-"

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Internal.validation ?field message))
    fmt

let has_ctl_or_del value =
  String.exists
    (fun c ->
      let code = Char.code c in
      code < 0x20 || code = 0x7F)
    value

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix

let validate_key seen key =
  if key = "" then invalid ~field:"metadata" "metadata key must be non-empty"
  else if has_ctl_or_del key then
    invalid ~field:"metadata" "metadata key contains control characters"
  else if is_prefix ~prefix (String.lowercase_ascii key) then
    invalid ~field:"metadata" "metadata keys must not include x-amz-meta-"
  else
    let lower = String.lowercase_ascii key in
    if List.exists (String.equal lower) seen then
      invalid ~field:"metadata" "metadata key %S is duplicated" key
    else Ok lower

let empty = []

let of_list entries =
  let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | (key, value) :: rest -> (
        match validate_key seen key with
        | Error _ as error -> error
        | Ok lower -> (
            match Header_value.of_string ~field:("metadata " ^ key) value with
            | Error _ as error -> error
            | Ok value -> loop (lower :: seen) ({ key; value } :: acc) rest))
  in
  loop [] [] entries

let of_list_exn entries = Awskit.Error.Internal.get_ok_exn (of_list entries)

let to_list metadata =
  List.map (fun { key; value } -> (key, Header_value.to_string value)) metadata

let entries metadata = metadata

let add ~key ~value metadata =
  let raw = to_list metadata @ [ (key, value) ] in
  of_list raw

let pp fmt metadata =
  Fmt.Dump.list
    (Fmt.Dump.pair Fmt.Dump.string Fmt.Dump.string)
    fmt (to_list metadata)

let equal left right = List.equal ( = ) (to_list left) (to_list right)
