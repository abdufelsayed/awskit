let ( let* ) = S3_result.( let* )

module Kms = struct
  type t = { key_id : string option; bucket_key_enabled : bool option }

  let create ?key_id ?bucket_key_enabled () =
    let* () =
      match key_id with
      | None -> Ok ()
      | Some key_id ->
          S3_validation.validate_header_value ~field:"sse_kms_key_id" key_id
    in
    Ok { key_id; bucket_key_enabled }

  let create_exn ?key_id ?bucket_key_enabled () =
    S3_result.result_exn (create ?key_id ?bucket_key_enabled ())

  let key_id t = t.key_id
  let bucket_key_enabled t = t.bucket_key_enabled
end

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

let validate_kms ~allow_bucket_key kms =
  let* () =
    match Kms.key_id kms with
    | None -> Ok ()
    | Some key_id ->
        S3_validation.validate_header_value ~field:"sse_kms_key_id" key_id
  in
  match (allow_bucket_key, Kms.bucket_key_enabled kms) with
  | false, Some _ ->
      S3_error_context.invalid ~field:"sse_bucket_key_enabled"
        "bucket keys are not supported for DSSE-KMS request encryption"
  | _ -> Ok ()

module Destination = struct
  type t =
    | Sse_s3
    | Sse_kms of Kms.t
    | Dsse_kms of Kms.t
    | Sse_c of Customer_key.t

  let validate_request = function
    | Sse_s3 | Sse_c _ -> Ok ()
    | Sse_kms kms -> validate_kms ~allow_bucket_key:true kms
    | Dsse_kms kms -> validate_kms ~allow_bucket_key:false kms
end

module Source = struct
  type t = Sse_c of Customer_key.t
end

module Observed = struct
  type t =
    | Sse_s3
    | Sse_kms of Kms.t
    | Dsse_kms of Kms.t
    | Sse_c
    | Aws_fsx
    | Unknown of string
end
