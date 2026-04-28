open Awskit_s3_core
open Awskit_s3_bucket_config_xml_support

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
    Option.value ~default:false
      (Option.bind (Xml.child_text name nodes) parse_bool)

  let parse body response =
    let* nodes = Xml.decode_root body ~name:"PublicAccessBlockConfiguration" in
    Ok
      {
        Bucket.Public_access_block.config =
          {
            block_public_acls = bool_child "BlockPublicAcls" nodes;
            ignore_public_acls = bool_child "IgnorePublicAcls" nodes;
            block_public_policy = bool_child "BlockPublicPolicy" nodes;
            restrict_public_buckets = bool_child "RestrictPublicBuckets" nodes;
          };
        request = response;
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
    | None -> Error (decode "missing ownership controls rule")
    | Some rule -> (
        match Xml.child_text "ObjectOwnership" rule with
        | None -> Error (decode "missing ObjectOwnership")
        | Some value -> (
            match
              Bucket.Ownership_controls.Object_ownership.of_string value
            with
            | None -> Error (decode "invalid object ownership %S" value)
            | Some object_ownership ->
                Ok
                  {
                    Bucket.Ownership_controls.config = { object_ownership };
                    request = response;
                  }))
end
