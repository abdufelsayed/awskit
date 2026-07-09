module Xml = S3_xml

let ( let* ) = S3_result.( let* )

open Bucket_xml_support

let rule_xml (rule : Bucket.Cors.Rule.t) =
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

let xml (config : Bucket.Cors.Config.t) =
  Xml.el "CORSConfiguration" (List.map rule_xml config.rules) |> xml_body

let parse_rule nodes =
  let allowed_methods =
    Xml.child_texts "AllowedMethod" nodes
    |> List.map Bucket.Cors.Method.observed_of_string
  in
  let* max_age_seconds =
    Xml.optional_child_parse ~path:"CORSRule" "MaxAgeSeconds"
      S3_parse.non_negative_int_of_string_opt nodes
  in
  Ok
    {
      Bucket.Cors.Observed.id = Xml.child_text "ID" nodes;
      allowed_origins = Xml.child_texts "AllowedOrigin" nodes;
      allowed_methods;
      allowed_headers = Xml.child_texts "AllowedHeader" nodes;
      expose_headers = Xml.child_texts "ExposeHeader" nodes;
      max_age_seconds;
    }

let parse body response =
  let* nodes = Xml.decode_root body ~name:"CORSConfiguration" in
  let rec loop acc = function
    | [] ->
        Ok
          {
            Bucket.Cors.config = { Bucket.Cors.Observed.rules = List.rev acc };
            response;
          }
    | nodes :: rest ->
        let* rule = parse_rule nodes in
        loop (rule :: acc) rest
  in
  loop [] (Xml.children "CORSRule" nodes)
