module Xml = S3_xml

let ( let* ) = S3_result.( let* )

open Headers
open Bucket_xml_support

let apply_xml (default : Bucket.Encryption.Default_encryption.t) =
  let algorithm, key_id =
    match default with
    | Sse_s3 -> ("AES256", None)
    | Sse_kms { key_id; _ } -> ("aws:kms", key_id)
    | Dsse_kms { key_id } -> ("aws:kms:dsse", key_id)
  in
  let children =
    []
    |> add_opt_header "KMSMasterKeyID" key_id
    |> add_opt_header "SSEAlgorithm" (Some algorithm)
    |> List.map (fun (name, value) -> Xml.text name value)
  in
  [ Xml.el "ApplyServerSideEncryptionByDefault" children ]

let bucket_key_xml (default : Bucket.Encryption.Default_encryption.t) =
  let bucket_key_enabled =
    match default with
    | Sse_kms { bucket_key_enabled; _ } -> bucket_key_enabled
    | Sse_s3 | Dsse_kms _ -> None
  in
  match bucket_key_enabled with
  | None -> []
  | Some value -> [ Xml.text "BucketKeyEnabled" (bool_text value) ]

let blocked_encryption_xml (policy : Bucket.Encryption.Sse_c_policy.t) =
  let value = match policy with Allow -> "NONE" | Block -> "SSE-C" in
  [ Xml.el "BlockedEncryptionTypes" [ Xml.text "EncryptionType" value ] ]

let rule_xml (rule : Bucket.Encryption.Rule.t) =
  let default, sse_c_policy =
    match rule with
    | Default default -> (Some default, None)
    | Sse_c policy -> (None, Some policy)
    | Default_and_sse_c { default_encryption; sse_c_policy } ->
        (Some default_encryption, Some sse_c_policy)
  in
  let default_xml =
    match default with
    | None -> []
    | Some default -> apply_xml default @ bucket_key_xml default
  in
  let sse_c_xml =
    match sse_c_policy with
    | None -> []
    | Some policy -> blocked_encryption_xml policy
  in
  Xml.el "Rule" (default_xml @ sse_c_xml)

let xml (config : Bucket.Encryption.Config.t) =
  Xml.el "ServerSideEncryptionConfiguration" (List.map rule_xml config.rules)
  |> xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"ServerSideEncryptionConfiguration" in
  let parse_apply nodes =
    match Xml.child "ApplyServerSideEncryptionByDefault" nodes with
    | None -> None
    | Some apply ->
        let algorithm =
          Option.map Bucket.Encryption.Observed.Algorithm.of_string
            (Xml.child_text "SSEAlgorithm" apply)
        in
        let kms_key_id = Xml.child_text "KMSMasterKeyID" apply in
        Some { Bucket.Encryption.Observed.algorithm; kms_key_id }
  in
  let parse_bucket_key nodes =
    match Xml.child_text "BucketKeyEnabled" nodes with
    | None -> Ok None
    | Some value -> (
        match Response.parse_bool value with
        | Some value -> Ok (Some value)
        | None ->
            Xml.decode_field_error
              ~path:"ServerSideEncryptionConfiguration.Rule"
              "<BucketKeyEnabled> has invalid value %S" value)
  in
  let parse_blocked nodes =
    Xml.child "BlockedEncryptionTypes" nodes
    |> Option.value ~default:[]
    |> Xml.child_texts "EncryptionType"
    |> List.map Bucket.Encryption.Observed.Sse_c_policy.of_string
  in
  let rec loop acc = function
    | [] ->
        Ok
          {
            Bucket.Encryption.config =
              { Bucket.Encryption.Observed.rules = List.rev acc };
            response;
          }
    | nodes :: rest ->
        let default_encryption = parse_apply nodes in
        let* bucket_key_enabled = parse_bucket_key nodes in
        let rule =
          {
            Bucket.Encryption.Observed.default_encryption;
            bucket_key_enabled;
            sse_c_policies = parse_blocked nodes;
          }
        in
        loop (rule :: acc) rest
  in
  loop [] (Xml.children "Rule" nodes)
