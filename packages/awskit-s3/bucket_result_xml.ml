open Common

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
      Ok
        {
          Bucket.name;
          creation_date =
            Option.bind (Xml.child_text "CreationDate" nodes) ptime_of_string;
        })

let parse_location body =
  let* nodes = Xml.decode_root body ~name:"LocationConstraint" in
  let value = String.trim (Xml.text_content nodes) in
  let value = if value = "" then "us-east-1" else value in
  let value = if value = "EU" then "eu-west-1" else value in
  Result.map Option.some (Awskit.Region.of_string value)
