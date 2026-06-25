let control_chars = [ '\000'; '\n'; '\r'; '\t'; '\127' ]

let insert_control value =
  let open QCheck.Gen in
  let* index = int_range 0 (String.length value) in
  let* ch = oneof_list control_chars in
  return
    (String.sub value 0 index
    ^ String.make 1 ch
    ^ String.sub value index (String.length value - index))

let truncate value =
  let open QCheck.Gen in
  if String.length value = 0 then return value
  else
    let* length = int_range 0 (String.length value - 1) in
    return (String.sub value 0 length)

let duplicate_fragment fragment value =
  QCheck.Gen.return (value ^ fragment ^ fragment)

let xml_tagging_seed =
  "<Tagging><TagSet><Tag><Key>env</Key><Value>dev</Value></Tag></TagSet></Tagging>"

let endpoint_seed = "https://s3.us-east-1.amazonaws.com"

let mutated_tagging_xml =
  QCheck.Gen.oneof
    [
      insert_control xml_tagging_seed;
      truncate xml_tagging_seed;
      duplicate_fragment "<Tag>" xml_tagging_seed;
      QCheck.Gen.return
        "<Tagging><TagSet><Tag><Key></Key><Value>x</Value></Tag></TagSet></Tagging>";
      QCheck.Gen.return
        "<Tagging><TagSet><Tag><Key>env</Key><Value>dev</Value></Tag><Tag><Key>env</Key><Value>prod</Value></Tag></TagSet></Tagging>";
      QCheck.Gen.return
        "<Tagging><TagSet><Tag><Key>aws:team</Key><Value>dev</Value></Tag></TagSet></Tagging>";
      QCheck.Gen.return
        "<Tagging><TagSet><Tag><Key>env</Key><Value>dev</Value></Tag><Ignored>x</Ignored></TagSet></Tagging>";
      QCheck.Gen.return "<Tagging><TagSet></TagSet></Tagging>";
    ]

let truncated_endpoint_path =
  QCheck.Gen.map (fun value -> value ^ "/bucket/path") (truncate endpoint_seed)

let mutated_endpoint =
  QCheck.Gen.oneof
    [
      QCheck.Gen.return endpoint_seed;
      insert_control endpoint_seed;
      truncated_endpoint_path;
      QCheck.Gen.return "https://user:pass@s3.us-east-1.amazonaws.com";
      QCheck.Gen.return "https://s3.us-east-1.amazonaws.com/path";
      QCheck.Gen.return "https://s3.us-east-1.amazonaws.com?bucket=name";
      QCheck.Gen.return "https://s3.us-east-1.amazonaws.com#bucket";
      QCheck.Gen.return "ftp://s3.us-east-1.amazonaws.com";
    ]
