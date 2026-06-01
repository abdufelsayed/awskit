open Core

let parse body response =
  let* nodes = Xml.decode_root body ~name:"PolicyStatus" in
  Ok
    {
      Bucket.Policy_status.is_public =
        Option.bind (Xml.child_text "IsPublic" nodes) parse_bool;
      response;
    }
