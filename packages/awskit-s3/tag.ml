type t = { key : string; value : string }

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Producer.validation ?field message))
    fmt

let max_key_chars = 128
let max_value_chars = 256
let max_tags = 10

let has_reserved_prefix key =
  let prefix = "aws:" in
  let prefix_len = String.length prefix in
  String.length key >= prefix_len
  && String.lowercase_ascii (String.sub key 0 prefix_len) = prefix

let is_allowed_punctuation = function
  | '+' | '-' | '=' | '.' | '_' | ':' | '/' | '@' -> true
  | _ -> false

let is_allowed_ascii code =
  code <= 0x7F
  &&
  let char = Char.chr code in
  match char with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | ' ' -> true
  | char -> is_allowed_punctuation char

let is_allowed_unicode_category scalar =
  match Uucp.Gc.general_category scalar with
  | `Ll | `Lm | `Lo | `Lt | `Lu | `Nd | `Nl | `No | `Zs -> true
  | _ -> false

let is_allowed_scalar scalar =
  let code = Uchar.to_int scalar in
  is_allowed_ascii code || is_allowed_unicode_category scalar

let validate_tag_characters ~field ~name value =
  if S3_utf8.for_all_scalars value ~f:is_allowed_scalar then Ok value
  else
    invalid ~field
      "%s must contain only letters, numbers, spaces, and + - = . _ : / @" name

let create ~key ~value =
  match
    S3_utf8.validate ~max_utf16_units:max_key_chars ~field:"tag key"
      ~name:"tag key" key
  with
  | Error _ as error -> error
  | Ok key when has_reserved_prefix key ->
      invalid ~field:"tag key" "tag key must not start with aws:"
  | Ok key -> (
      match validate_tag_characters ~field:"tag key" ~name:"tag key" key with
      | Error _ as error -> error
      | Ok key -> (
          match
            S3_utf8.validate ~allow_empty:true ~max_utf16_units:max_value_chars
              ~field:"tag value" ~name:"tag value" value
          with
          | Error _ as error -> error
          | Ok value -> (
              match
                validate_tag_characters ~field:"tag value" ~name:"tag value"
                  value
              with
              | Error _ as error -> error
              | Ok value -> Ok { key; value })))

let create_exn ~key ~value =
  Awskit.Error.Producer.get_ok_exn (create ~key ~value)

let key tag = tag.key
let value tag = tag.value
let pp fmt tag = Fmt.pf fmt "{ key = %S; value = %S }" tag.key tag.value

let equal left right =
  String.equal left.key right.key && String.equal left.value right.value

module Set = struct
  type tag = t
  type t = tag list

  let empty = []

  let of_list tags =
    if List.length tags > max_tags then
      invalid ~field:"tags" "tag set must contain at most %d tags" max_tags
    else
      let rec loop seen acc = function
        | [] -> Ok (List.rev acc)
        | tag :: rest ->
            if List.exists (String.equal (key tag)) seen then
              invalid ~field:"tag key" "tag key %S is duplicated" (key tag)
            else loop (key tag :: seen) (tag :: acc) rest
      in
      loop [] [] tags

  let of_list_exn tags = Awskit.Error.Producer.get_ok_exn (of_list tags)
  let to_list tags = tags
end
