let ( let* ) = S3_result.( let* )

module Customer_key = struct
  type t = { key_base64 : string; key_md5_base64 : string }

  let algorithm = "AES256"
  let aes256_key_length = 32

  let of_raw_string ~field raw =
    let length = String.length raw in
    if length <> aes256_key_length then
      S3_error_context.invalid ~field
        "%s must be exactly %d bytes for AES256 SSE-C, got %d bytes" field
        aes256_key_length length
    else
      let key_base64 = Base64.encode_exn raw in
      let key_md5_base64 =
        Digestif.MD5.(digest_string raw |> to_raw_string) |> Base64.encode_exn
      in
      Ok { key_base64; key_md5_base64 }

  let of_bytes bytes =
    of_raw_string ~field:"sse_customer_key" (Bytes.to_string bytes)

  let of_bytes_exn bytes = S3_result.result_exn (of_bytes bytes)

  let of_base64 value =
    if String.equal value "" then
      S3_error_context.invalid ~field:"sse_customer_key"
        "sse_customer_key must be non-empty"
    else
      match Base64.decode value with
      | Ok raw -> of_raw_string ~field:"sse_customer_key" raw
      | Error (`Msg message) ->
          S3_error_context.invalid ~field:"sse_customer_key"
            "invalid base64 SSE-C customer key: %s" message

  let of_base64_exn value = S3_result.result_exn (of_base64 value)
  let algorithm _ = algorithm
  let key_base64 t = t.key_base64
  let key_md5_base64 t = t.key_md5_base64
end

module Destination = struct
  type t =
    | Sse_s3
    | Sse_kms of { key_id : string option; bucket_key_enabled : bool option }
    | Dsse_kms of { key_id : string option }
    | Sse_c of Customer_key.t

  let validate_key_id = function
    | None -> Ok ()
    | Some key_id ->
        S3_validation.validate_header_value ~field:"sse_kms_key_id" key_id

  let sse_s3 = Sse_s3

  let sse_kms ?key_id ?bucket_key_enabled () =
    let* () = validate_key_id key_id in
    Ok (Sse_kms { key_id; bucket_key_enabled })

  let sse_kms_exn ?key_id ?bucket_key_enabled () =
    S3_result.result_exn (sse_kms ?key_id ?bucket_key_enabled ())

  let dsse_kms ?key_id () =
    let* () = validate_key_id key_id in
    Ok (Dsse_kms { key_id })

  let dsse_kms_exn ?key_id () = S3_result.result_exn (dsse_kms ?key_id ())
  let sse_c key = Sse_c key
end

module Source = struct
  type t = Sse_c of Customer_key.t
end

module Observed = struct
  type kms = { key_id : string option; bucket_key_enabled : bool option }

  type t =
    | Sse_s3
    | Sse_kms of kms
    | Dsse_kms of kms
    | Sse_c
    | Aws_fsx
    | Unknown of string
end
