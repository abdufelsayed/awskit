type view = Bytes of int64 * int64 | From of int64 | Suffix of int64
type t = view

let invalid ?field message =
  Error (Awskit.Error.Internal.validation ?field message)

let result_exn = Awskit.Error.Internal.get_ok_exn
let ( let* ) = Result.bind

let non_negative ~field value =
  if Int64.compare value 0L < 0 then
    invalid ~field (field ^ " must be non-negative")
  else Ok ()

let bytes ~start ~finish =
  match non_negative ~field:"range start" start with
  | Error _ as error -> error
  | Ok () -> (
      match non_negative ~field:"range finish" finish with
      | Error _ as error -> error
      | Ok () ->
          if Int64.compare finish start < 0 then
            invalid ~field:"range"
              "finish must be greater than or equal to start"
          else Ok (Bytes (start, finish)))

let bytes_exn ~start ~finish = result_exn (bytes ~start ~finish)

let from start =
  match non_negative ~field:"range start" start with
  | Error _ as error -> error
  | Ok () -> Ok (From start)

let from_exn start = result_exn (from start)

let suffix length =
  if Int64.compare length 0L <= 0 then
    invalid ~field:"range suffix" "suffix length must be positive"
  else Ok (Suffix length)

let suffix_exn length = result_exn (suffix length)

let to_header = function
  | Bytes (start, finish) -> Fmt.str "bytes=%Ld-%Ld" start finish
  | From start -> Fmt.str "bytes=%Ld-" start
  | Suffix length -> Fmt.str "bytes=-%Ld" length

let view = function
  | Bytes (start, finish) -> Bytes (start, finish)
  | From start -> From start
  | Suffix length -> Suffix length

module Content_range = struct
  type t = { start : int64; finish : int64; complete_length : int64 option }

  let decode message = Error (Awskit.Error.Internal.decode message)

  let decimal value =
    value <> ""
    && String.for_all (function '0' .. '9' -> true | _ -> false) value

  let parse_int64 ~field value =
    if not (decimal value) then decode (field ^ " must be a decimal integer")
    else
      try Ok (Int64.of_string value)
      with Failure _ -> decode (field ^ " is out of int64 range")

  let of_header value =
    let value = String.trim value in
    let prefix = "bytes " in
    let prefix_length = String.length prefix in
    if
      String.length value <= prefix_length
      || String.sub value 0 prefix_length <> prefix
    then decode "Content-Range must use bytes range-unit"
    else
      let spec =
        String.sub value prefix_length (String.length value - prefix_length)
      in
      match String.split_on_char '/' spec with
      | [ positions; complete_length ] -> (
          match String.split_on_char '-' positions with
          | [ start; finish ] ->
              let* start = parse_int64 ~field:"Content-Range start" start in
              let* finish = parse_int64 ~field:"Content-Range finish" finish in
              if Int64.compare finish start < 0 then
                decode
                  "Content-Range finish must be greater than or equal to start"
              else
                let* complete_length =
                  match complete_length with
                  | "*" -> Ok None
                  | value ->
                      let* length =
                        parse_int64 ~field:"Content-Range complete length" value
                      in
                      if Int64.compare finish length >= 0 then
                        decode
                          "Content-Range finish must be below complete length"
                      else Ok (Some length)
                in
                Ok { start; finish; complete_length }
          | _ -> decode "Content-Range byte positions must be start-finish")
      | _ -> decode "Content-Range must be start-finish/length"

  let to_header { start; finish; complete_length } =
    let length =
      match complete_length with
      | None -> "*"
      | Some length -> Int64.to_string length
    in
    Fmt.str "bytes %Ld-%Ld/%s" start finish length
end
