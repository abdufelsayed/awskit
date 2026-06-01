open Core
open Bucket_config_xml_support

module Request_payment = struct
  let xml payer =
    Xml.el "RequestPaymentConfiguration"
      [ Xml.text "Payer" (Bucket.Request_payment.Payer.to_string payer) ]
    |> xml_body

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"RequestPaymentConfiguration" in
    let payer =
      Option.bind
        (Xml.child_text "Payer" nodes)
        Bucket.Request_payment.Payer.of_string
    in
    Ok { Bucket.Request_payment.payer; response }
end

module Accelerate = struct
  let xml status =
    Xml.el "AccelerateConfiguration"
      [ Xml.text "Status" (Bucket.Accelerate.Status.to_string status) ]
    |> xml_body

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"AccelerateConfiguration" in
    let status =
      Option.bind
        (Xml.child_text "Status" nodes)
        Bucket.Accelerate.Status.of_string
    in
    Ok { Bucket.Accelerate.status; response }
end

module Logging = struct
  let validate_config (config : Bucket.Logging.config) =
    match config.logging with
    | None -> Ok ()
    | Some target ->
        validate_all
          [
            validate_header_value ~field:"target_bucket" target.target_bucket;
            validate_header_value ~field:"target_prefix" target.target_prefix;
          ]

  let xml (config : Bucket.Logging.config) =
    Xml.el "BucketLoggingStatus"
      (match config.logging with
      | None -> []
      | Some target ->
          [
            Xml.el "LoggingEnabled"
              [
                Xml.text "TargetBucket" target.target_bucket;
                Xml.text "TargetPrefix" target.target_prefix;
              ];
          ])
    |> xml_body

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"BucketLoggingStatus" in
    let logging =
      match Xml.child "LoggingEnabled" nodes with
      | None -> None
      | Some nodes -> (
          match
            ( Xml.child_text "TargetBucket" nodes,
              Xml.child_text "TargetPrefix" nodes )
          with
          | Some target_bucket, Some target_prefix ->
              Some { Bucket.Logging.target_bucket; target_prefix }
          | _ -> None)
    in
    Ok { Bucket.Logging.config = { logging }; response }
end
