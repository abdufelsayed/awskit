module Xml = S3_xml

let ( let* ) = S3_result.( let* )

open Bucket_xml_support

let validate_rule (rule : Bucket.Cors.rule) =
  let* () =
    validate_all
      [
        validate_opt_header "cors rule id" rule.id;
        validate_string_list ~field:"allowed origin" rule.allowed_origins;
        validate_string_list ~field:"allowed header" rule.allowed_headers;
        validate_string_list ~field:"expose header" rule.expose_headers;
      ]
  in
  match (rule.allowed_origins, rule.allowed_methods) with
  | [], _ ->
      S3_error_context.invalid ~field:"cors"
        "CORS rule must include an allowed origin"
  | _, [] ->
      S3_error_context.invalid ~field:"cors"
        "CORS rule must include an allowed method"
  | _ -> (
      match rule.max_age_seconds with
      | Some value when value < 0 ->
          S3_error_context.invalid ~field:"cors"
            "max_age_seconds must be non-negative"
      | _ -> Ok ())

let validate_config (config : Bucket.Cors.config) =
  let rec loop = function
    | [] -> Ok ()
    | rule :: rest ->
        let* () = validate_rule rule in
        loop rest
  in
  loop config.rules

let rule_xml (rule : Bucket.Cors.rule) =
  Xml.el "CORSRule"
    ((match rule.id with None -> [] | Some id -> [ Xml.text "ID" id ])
    @ List.map (Xml.text "AllowedOrigin") rule.allowed_origins
    @ List.map
        (fun method_ ->
          Xml.text "AllowedMethod" (Bucket.Cors.Method.to_string method_))
        rule.allowed_methods
    @ List.map (Xml.text "AllowedHeader") rule.allowed_headers
    @ List.map (Xml.text "ExposeHeader") rule.expose_headers
    @
    match rule.max_age_seconds with
    | None -> []
    | Some value -> [ Xml.text "MaxAgeSeconds" (string_of_int value) ])

let xml (config : Bucket.Cors.config) =
  Xml.el "CORSConfiguration" (List.map rule_xml config.rules) |> xml_body

let parse_method value =
  match Bucket.Cors.Method.of_string value with
  | Some method_ -> Ok method_
  | None -> Error (S3_error_context.decode "invalid CORS method %S" value)

let parse_methods values =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* method_ = parse_method value in
        loop (method_ :: acc) rest
  in
  loop [] values

let parse_rule nodes =
  let* allowed_methods =
    parse_methods (Xml.child_texts "AllowedMethod" nodes)
  in
  let* max_age_seconds =
    Xml.optional_child_parse ~path:"CORSRule" "MaxAgeSeconds"
      S3_parse.non_negative_int_of_string_opt nodes
  in
  Ok
    {
      Bucket.Cors.id = Xml.child_text "ID" nodes;
      allowed_origins = Xml.child_texts "AllowedOrigin" nodes;
      allowed_methods;
      allowed_headers = Xml.child_texts "AllowedHeader" nodes;
      expose_headers = Xml.child_texts "ExposeHeader" nodes;
      max_age_seconds;
    }

let parse body response =
  let* nodes = Xml.decode_root body ~name:"CORSConfiguration" in
  let rec loop acc = function
    | [] -> Ok { Bucket.Cors.config = { rules = List.rev acc }; response }
    | nodes :: rest ->
        let* rule = parse_rule nodes in
        loop (rule :: acc) rest
  in
  loop [] (Xml.children "CORSRule" nodes)
