type t = string

let field = "bucket"
let invalid message = Error (Awskit.Error.Producer.validation ~field message)

(* General-purpose S3 bucket-name rules:
   https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucketnamingrules.html

   This module validates bucket identifiers accepted by supported S3
   operations. Operation-specific constraints, such as transfer acceleration
   rejecting dotted names or account-regional namespace creation rules, belong
   on those operation options rather than in the shared bucket-name type. *)

let reserved_prefixes = [ "xn--"; "sthree-"; "amzn-s3-demo-" ]

let reserved_suffixes =
  [ "-s3alias"; "--ol-s3"; ".mrap"; "--x-s3"; "--table-s3" ]

let is_lower = function 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alnum c = is_lower c || is_digit c
let is_bucket_char = function '.' | '-' -> true | c -> is_alnum c

let valid_length bucket =
  let len = String.length bucket in
  len >= 3 && len <= 63

let starts_and_ends_with_alnum bucket =
  let len = String.length bucket in
  len > 0 && is_alnum bucket.[0] && is_alnum bucket.[len - 1]

let has_adjacent_dot bucket =
  let len = String.length bucket in
  let rec loop i =
    i + 1 < len
    &&
    match (bucket.[i], bucket.[i + 1]) with
    | '.', '.' -> true
    | _ -> loop (i + 1)
  in
  loop 0

let decimal_octet value =
  value <> ""
  &&
  match S3_parse.int_of_string_opt value with
  | Some n -> n >= 0 && n <= 255
  | None -> false

let looks_like_ipv4 bucket =
  match String.split_on_char '.' bucket with
  | [ a; b; c; d ] -> List.for_all decimal_octet [ a; b; c; d ]
  | _ -> false

let invalid_reserved_prefix bucket =
  List.find_opt
    (fun prefix -> S3_string.is_prefix ~prefix bucket)
    reserved_prefixes

let invalid_reserved_suffix bucket =
  List.find_opt
    (fun suffix -> S3_string.is_suffix ~suffix bucket)
    reserved_suffixes

let first_rule_violation bucket =
  if not (valid_length bucket) then Some "bucket must be 3-63 characters"
  else if not (String.for_all is_bucket_char bucket) then
    Some "bucket must contain only lowercase letters, digits, dots, and hyphens"
  else if not (starts_and_ends_with_alnum bucket) then
    Some "bucket must start and end with a lowercase letter or digit"
  else if has_adjacent_dot bucket then
    Some "bucket must not contain adjacent dots"
  else if looks_like_ipv4 bucket then
    Some "bucket must not be formatted as an IPv4 address"
  else
    match invalid_reserved_prefix bucket with
    | Some prefix -> Some (Fmt.str "bucket must not start with %s" prefix)
    | None -> (
        match invalid_reserved_suffix bucket with
        | Some suffix -> Some (Fmt.str "bucket must not end with %s" suffix)
        | None -> None)

let of_string bucket =
  match first_rule_violation bucket with
  | Some message -> invalid message
  | None -> Ok bucket

let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal
