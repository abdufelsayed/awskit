module Xml = S3_xml

let ( let* ) = S3_result.( let* )

let create_config region =
  Xml.el "CreateBucketConfiguration"
    [ Xml.text "LocationConstraint" (Awskit.Region.to_string region) ]
  |> Xml.to_string

let parse_list body =
  let* nodes = Xml.decode_root body ~name:"ListAllMyBucketsResult" in
  Xml.child "Buckets" nodes
  |> Option.value ~default:[]
  |> Xml.children_result "Bucket" ~f:(fun index nodes ->
      let path = Fmt.str "ListAllMyBucketsResult.Buckets.Bucket[%d]" index in
      let* name = Xml.required_child_text ~path "Name" nodes in
      let* name =
        match Bucket_name.of_string name with
        | Ok _ as result -> result
        | Error error ->
            Xml.decode_field_error ~path "%s" (Awskit.Error.to_string_hum error)
      in
      let* creation_date =
        Xml.optional_child_parse ~path "CreationDate" S3_time.of_string nodes
      in
      Ok { Bucket.name; creation_date })

let parse_location body =
  let* nodes = Xml.decode_root body ~name:"LocationConstraint" in
  let value = String.trim (Xml.text_content nodes) in
  let value = if String.equal value "" then "us-east-1" else value in
  let value = if String.equal value "EU" then "eu-west-1" else value in
  match Awskit.Region.of_string value with
  | Ok region -> Ok region
  | Error error ->
      Xml.decode_field_error ~path:"LocationConstraint"
        "<LocationConstraint> has invalid value %S: %s" value
        (Awskit.Error.to_string_hum error)
