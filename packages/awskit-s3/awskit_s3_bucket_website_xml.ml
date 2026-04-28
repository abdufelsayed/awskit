open Awskit_s3_core
open Awskit_s3_bucket_config_xml_support

let validate_config (config : Bucket.Website.config) =
  match (config.index_document_suffix, config.error_document_key) with
  | None, None ->
      invalid ~field:"website"
        "website config must include an index document or error document"
  | index_document_suffix, error_document_key ->
      let* () =
        validate_opt_header "index_document_suffix" index_document_suffix
      in
      validate_opt_header "error_document_key" error_document_key

let xml (config : Bucket.Website.config) =
  Xml.el "WebsiteConfiguration"
    ((match config.index_document_suffix with
       | None -> []
       | Some suffix -> [ Xml.el "IndexDocument" [ Xml.text "Suffix" suffix ] ])
    @
    match config.error_document_key with
    | None -> []
    | Some key -> [ Xml.el "ErrorDocument" [ Xml.text "Key" key ] ])
  |> xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"WebsiteConfiguration" in
  Ok
    {
      Bucket.Website.config =
        {
          index_document_suffix =
            Option.bind
              (Xml.child "IndexDocument" nodes)
              (Xml.child_text "Suffix");
          error_document_key =
            Option.bind (Xml.child "ErrorDocument" nodes) (Xml.child_text "Key");
        };
      request = response;
    }
