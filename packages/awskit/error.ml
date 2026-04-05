open Base

type base =
  [ `Access_denied
  | `Service_unavailable of string
  | `Request_error of int * string
  | `Body_read_error of string
  | `Invalid_response of string
  | `Invalid_header of string * string
  | `Missing_response_header of string
  | `Invalid_xml of string
  | `Invalid_json of string
  | `Invalid_request of string ]
[@@deriving show, eq]

let pp_base = pp_base
let equal_base = equal_base
