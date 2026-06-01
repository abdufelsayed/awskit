open Core
open Bucket_config_xml_support

let validate_rule (rule : Bucket.Encryption.Rule.t) =
  match rule.kms_master_key_id with
  | None -> Ok ()
  | Some value -> validate_header_value ~field:"kms_master_key_id" value

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

let rule_xml (rule : Bucket.Encryption.Rule.t) =
  Xml.el "Rule"
    [
      Xml.el "ApplyServerSideEncryptionByDefault"
        ([
           Xml.text "SSEAlgorithm"
             (Bucket.Encryption.Algorithm.to_string rule.sse_algorithm);
         ]
        @
        match rule.kms_master_key_id with
        | None -> []
        | Some value -> [ Xml.text "KMSMasterKeyID" value ]);
    ]

let xml (config : Bucket.Encryption.config) =
  Xml.el "ServerSideEncryptionConfiguration" (List.map rule_xml config.rules)
  |> xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"ServerSideEncryptionConfiguration" in
  let rec loop acc = function
    | [] -> Ok { Bucket.Encryption.config = { rules = List.rev acc }; response }
    | nodes :: rest -> (
        match Xml.child "ApplyServerSideEncryptionByDefault" nodes with
        | None -> Error (decode "missing ApplyServerSideEncryptionByDefault")
        | Some apply -> (
            match Xml.child_text "SSEAlgorithm" apply with
            | None -> Error (decode "missing SSEAlgorithm")
            | Some algorithm -> (
                match Bucket.Encryption.Algorithm.of_string algorithm with
                | None ->
                    Error (decode "invalid encryption algorithm %S" algorithm)
                | Some sse_algorithm ->
                    let rule =
                      {
                        Bucket.Encryption.Rule.sse_algorithm;
                        kms_master_key_id =
                          Xml.child_text "KMSMasterKeyID" apply;
                      }
                    in
                    loop (rule :: acc) rest)))
  in
  loop [] (Xml.children "Rule" nodes)
