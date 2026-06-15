type view = Bytes of int64 * int64 | From of int64 | Suffix of int64
type t = view

let invalid ?field message =
  Error (Awskit.Error.Internal.validation ?field message)

let result_exn = Awskit.Error.Internal.get_ok_exn

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
