open Base

type t = Bytes of int * int | From of int | Suffix of int
[@@deriving show, eq]

module Validation = struct
  let non_negative ~name value =
    if value < 0 then
      Error (`Invalid_request (Fmt.str "%s must be non-negative" name))
    else Ok value

  let positive ~name value =
    if value <= 0 then
      Error (`Invalid_request (Fmt.str "%s must be positive" name))
    else Ok value
end

let to_header = function
  | Bytes (start, finish) -> (
      match Validation.non_negative ~name:"range start" start with
      | Error _ as error -> error
      | Ok start -> (
          match Validation.non_negative ~name:"range end" finish with
          | Error _ as error -> error
          | Ok finish ->
              if start > finish then
                Error (`Invalid_request "range start must be <= range end")
              else Ok (Fmt.str "bytes=%d-%d" start finish)))
  | From start ->
      Result.map (Validation.non_negative ~name:"range start" start)
        ~f:(fun start -> Fmt.str "bytes=%d-" start)
  | Suffix length ->
      Result.map (Validation.positive ~name:"range suffix length" length)
        ~f:(fun length -> Fmt.str "bytes=-%d" length)
