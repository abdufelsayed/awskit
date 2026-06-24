open Simulator_support
open Simulator_headers
module Object = Awskit_s3.Object

let etag body =
  Object.Etag.of_string_exn
    ("\"" ^ Digestif.MD5.(digest_string body |> to_hex) ^ "\"")

let empty_checksum = { Object.Checksum.values = []; checksum_type = None }

let checksum_response ?checksum_type values =
  { Object.Checksum.values; checksum_type }

let checksum_for_value = function
  | None -> empty_checksum
  | Some (value : Object.Checksum.value) -> checksum_response [ value ]

let checksum_for_algorithm ~body = function
  | None -> empty_checksum
  | Some Object.Checksum.Algorithm.Sha1 ->
      checksum_response
        [
          Object.Checksum.value_exn ~algorithm:Sha1
            ~value:
              (Digestif.SHA1.(digest_string body |> to_raw_string)
              |> Base64.encode_exn);
        ]
  | Some Sha256 ->
      checksum_response
        [
          Object.Checksum.value_exn ~algorithm:Sha256
            ~value:
              (Digestif.SHA256.(digest_string body |> to_raw_string)
              |> Base64.encode_exn);
        ]
  | Some _ -> empty_checksum

let checksum_summary (checksum : Object.Checksum.response) =
  {
    Object.Checksum.algorithms =
      List.map
        (fun (value : Object.Checksum.value) -> value.algorithm)
        checksum.values;
    checksum_type = checksum.checksum_type;
  }

let checksum_response_headers = function
  | { Object.Checksum.values = []; checksum_type = None } -> []
  | checksum ->
      let value_headers =
        checksum.values
        |> List.filter_map (fun (value : Object.Checksum.value) ->
            Option.map
              (fun name -> (name, value.value))
              (checksum_header_name value.algorithm))
      in
      let type_headers = checksum_type_header checksum.checksum_type in
      value_headers @ type_headers
