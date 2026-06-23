type t = string

let max_key_bytes = 1024

let validate_object_key ?(allow_empty = false) ~field ~name value =
  S3_utf8.validate ~max_bytes:max_key_bytes ~allow_empty ~field ~name value

let of_string value = validate_object_key ~field:"key" ~name:"key" value
let of_string_exn value = Awskit.Error.Internal.get_ok_exn (of_string value)
let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal

module Prefix = struct
  type t = string

  let of_string value = validate_object_key ~field:"prefix" ~name:"prefix" value
  let of_string_exn value = Awskit.Error.Internal.get_ok_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Delimiter = struct
  type t = string

  let of_string value =
    S3_utf8.validate ~field:"delimiter" ~name:"delimiter" value

  let of_string_exn value = Awskit.Error.Internal.get_ok_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end
