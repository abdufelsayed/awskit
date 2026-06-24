module Xml = S3_xml

let ( let* ) = S3_result.( let* )

open Bucket_xml_support

module Public_access_block = struct
  let bool_node name value = Xml.text name (bool_text value)

  let xml (config : Bucket.Public_access_block.config) =
    Xml.el "PublicAccessBlockConfiguration"
      [
        bool_node "BlockPublicAcls" config.block_public_acls;
        bool_node "IgnorePublicAcls" config.ignore_public_acls;
        bool_node "BlockPublicPolicy" config.block_public_policy;
        bool_node "RestrictPublicBuckets" config.restrict_public_buckets;
      ]
    |> xml_body

  let bool_child name nodes =
    match Xml.child_text name nodes with
    | None -> Ok false
    | Some value -> (
        match Response.parse_bool value with
        | Some value -> Ok value
        | None ->
            Xml.decode_field_error ~path:"PublicAccessBlockConfiguration"
              "<%s> has invalid value %S" name value)

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"PublicAccessBlockConfiguration" in
    let* block_public_acls = bool_child "BlockPublicAcls" nodes in
    let* ignore_public_acls = bool_child "IgnorePublicAcls" nodes in
    let* block_public_policy = bool_child "BlockPublicPolicy" nodes in
    let* restrict_public_buckets = bool_child "RestrictPublicBuckets" nodes in
    Ok
      {
        Bucket.Public_access_block.config =
          {
            block_public_acls;
            ignore_public_acls;
            block_public_policy;
            restrict_public_buckets;
          };
        response;
      }
end

module Ownership_controls = struct
  let xml (config : Bucket.Ownership_controls.config) =
    Xml.el "OwnershipControls"
      [
        Xml.el "Rule"
          [
            Xml.text "ObjectOwnership"
              (Bucket.Ownership_controls.Object_ownership.to_string
                 config.object_ownership);
          ];
      ]
    |> xml_body

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"OwnershipControls" in
    match Xml.child "Rule" nodes with
    | None -> Error (S3_error_context.decode "missing ownership controls rule")
    | Some rule -> (
        match Xml.child_text "ObjectOwnership" rule with
        | None -> Error (S3_error_context.decode "missing ObjectOwnership")
        | Some "" ->
            Error (S3_error_context.decode "invalid object ownership %S" "")
        | Some value ->
            let object_ownership =
              Bucket.Ownership_controls.Object_ownership.of_string value
            in
            Ok
              {
                Bucket.Ownership_controls.config = { object_ownership };
                response;
              })
end
