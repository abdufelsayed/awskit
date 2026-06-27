type t = string

let max_key_bytes = 1024

let validate_key_text ?(allow_empty = false) ~field ~name value =
  S3_utf8.validate ~max_bytes:max_key_bytes ~allow_empty ~field ~name value

let relative_path_segments_are_valid value =
  let rec loop depth = function
    | [] -> true
    | "" :: segments | "." :: segments -> loop depth segments
    | ".." :: _ when depth = 0 -> false
    | ".." :: segments -> loop (depth - 1) segments
    | _ :: segments -> loop (depth + 1) segments
  in
  loop 0 (String.split_on_char '/' value)

let validate_object_key ?allow_empty ~field ~name value =
  match validate_key_text ?allow_empty ~field ~name value with
  | Error _ as error -> error
  | Ok value ->
      if relative_path_segments_are_valid value then Ok value
      else
        Error
          (Awskit.Error.Producer.validation ~field
             (Printf.sprintf
                "%s relative path segments must not exceed non-relative \
                 segments"
                name))

let of_string value = validate_object_key ~field:"key" ~name:"key" value
let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal

module Prefix = struct
  type t = string

  let of_string value = validate_key_text ~field:"prefix" ~name:"prefix" value
  let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Delimiter = struct
  type t = string

  let of_string value =
    S3_utf8.validate ~field:"delimiter" ~name:"delimiter" value

  let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end
