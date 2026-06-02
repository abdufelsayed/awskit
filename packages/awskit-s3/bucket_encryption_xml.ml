open Core
open Bucket_config_xml_support

let validate_rule (rule : Bucket.Encryption.Rule.t) =
  let validate_algorithm = function
    | None
    | Some Bucket.Encryption.Algorithm.Aes256
    | Some Aws_kms
    | Some Aws_kms_dsse ->
        Ok ()
    | Some (Unknown _) ->
        invalid ~field:"sse_algorithm"
          "unknown encryption algorithms cannot be written"
  in
  let validate_blocked_encryption_type = function
    | Bucket.Encryption.Blocked_encryption_type.Sse_c | No_block -> Ok ()
    | Unknown _ ->
        invalid ~field:"blocked_encryption_types"
          "unknown blocked encryption types cannot be written"
  in
  let validate_kms_usage () =
    match (rule.sse_algorithm, rule.kms_master_key_id) with
    | (Some Aws_kms | Some Aws_kms_dsse), Some _ -> Ok ()
    | _, None -> Ok ()
    | _ ->
        invalid ~field:"kms_master_key_id"
          "kms_master_key_id requires aws:kms or aws:kms:dsse"
  in
  let validate_bucket_key_usage () =
    match (rule.sse_algorithm, rule.bucket_key_enabled) with
    | (Some Aws_kms | Some Aws_kms_dsse), Some _ -> Ok ()
    | _, None -> Ok ()
    | _ ->
        invalid ~field:"bucket_key_enabled"
          "bucket_key_enabled requires aws:kms or aws:kms:dsse"
  in
  let rec validate_blocked = function
    | [] -> Ok ()
    | value :: rest ->
        let* () = validate_blocked_encryption_type value in
        validate_blocked rest
  in
  let* () = validate_algorithm rule.sse_algorithm in
  let* () = validate_blocked rule.blocked_encryption_types in
  let* () = validate_kms_usage () in
  let* () = validate_bucket_key_usage () in
  validate_opt_header "kms_master_key_id" rule.kms_master_key_id

let validate_config (config : Bucket.Encryption.config) =
  match config.rules with
  | [] -> invalid ~field:"encryption" "encryption config must include a rule"
  | rules ->
      let rec loop = function
        | [] -> Ok ()
        | rule :: rest ->
            let* () = validate_rule rule in
            loop rest
      in
      loop rules

let apply_xml (rule : Bucket.Encryption.Rule.t) =
  let children =
    []
    |> add_opt_header "KMSMasterKeyID" rule.kms_master_key_id
    |> add_opt_header "SSEAlgorithm"
         (Option.map Bucket.Encryption.Algorithm.to_string rule.sse_algorithm)
    |> List.map (fun (name, value) -> Xml.text name value)
  in
  match children with
  | [] -> []
  | children -> [ Xml.el "ApplyServerSideEncryptionByDefault" children ]

let bucket_key_xml (rule : Bucket.Encryption.Rule.t) =
  match rule.bucket_key_enabled with
  | None -> []
  | Some value -> [ Xml.text "BucketKeyEnabled" (bool_text value) ]

let blocked_encryption_xml (rule : Bucket.Encryption.Rule.t) =
  match rule.blocked_encryption_types with
  | [] -> []
  | values ->
      [
        Xml.el "BlockedEncryptionTypes"
          (List.map
             (fun value ->
               Xml.text "EncryptionType"
                 (Bucket.Encryption.Blocked_encryption_type.to_string value))
             values);
      ]

let rule_xml (rule : Bucket.Encryption.Rule.t) =
  Xml.el "Rule"
    (apply_xml rule @ bucket_key_xml rule @ blocked_encryption_xml rule)

let xml (config : Bucket.Encryption.config) =
  Xml.el "ServerSideEncryptionConfiguration" (List.map rule_xml config.rules)
  |> xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"ServerSideEncryptionConfiguration" in
  let parse_apply nodes =
    match Xml.child "ApplyServerSideEncryptionByDefault" nodes with
    | None -> (None, None)
    | Some apply ->
        let sse_algorithm =
          Option.map Bucket.Encryption.Algorithm.of_string
            (Xml.child_text "SSEAlgorithm" apply)
        in
        let kms_master_key_id = Xml.child_text "KMSMasterKeyID" apply in
        (sse_algorithm, kms_master_key_id)
  in
  let parse_bucket_key nodes =
    Option.bind (Xml.child_text "BucketKeyEnabled" nodes) Response.parse_bool
  in
  let parse_blocked nodes =
    Xml.child "BlockedEncryptionTypes" nodes
    |> Option.value ~default:[]
    |> Xml.child_texts "EncryptionType"
    |> List.map Bucket.Encryption.Blocked_encryption_type.of_string
  in
  let rec loop acc = function
    | [] -> Ok { Bucket.Encryption.config = { rules = List.rev acc }; response }
    | nodes :: rest ->
        let sse_algorithm, kms_master_key_id = parse_apply nodes in
        let rule =
          {
            Bucket.Encryption.Rule.sse_algorithm;
            kms_master_key_id;
            bucket_key_enabled = parse_bucket_key nodes;
            blocked_encryption_types = parse_blocked nodes;
          }
        in
        loop (rule :: acc) rest
  in
  loop [] (Xml.children "Rule" nodes)
