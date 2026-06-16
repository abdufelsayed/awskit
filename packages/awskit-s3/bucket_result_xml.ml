open Common

let create_config region =
  Xml.el "CreateBucketConfiguration"
    [ Xml.text "LocationConstraint" (Awskit.Region.to_string region) ]
  |> Xml.to_string

let parse_list body =
  let* nodes = Xml.decode_root body ~name:"ListAllMyBucketsResult" in
  let buckets =
    Xml.child "Buckets" nodes
    |> Option.value ~default:[]
    |> Xml.children "Bucket"
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Name" nodes with
        | None -> None
        | Some name ->
            Some
              {
                Bucket.name;
                creation_date =
                  Option.bind
                    (Xml.child_text "CreationDate" nodes)
                    ptime_of_string;
              })
  in
  Ok buckets

let parse_location body =
  let* nodes = Xml.decode_root body ~name:"LocationConstraint" in
  let value = String.trim (Xml.text_content nodes) in
  let value = if value = "" then "us-east-1" else value in
  let value = if value = "EU" then "eu-west-1" else value in
  Result.map Option.some (Awskit.Region.of_string value)
