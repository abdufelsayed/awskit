open Simulator_support
open Simulator_headers
module Object = Awskit_s3.Object

let etag body =
  Object.Etag.of_string_exn
    ("\"" ^ Digestif.MD5.(digest_string body |> to_hex) ^ "\"")

let multipart_etag bodies =
  let part_digests =
    List.map
      (fun body -> Digestif.MD5.(digest_string body |> to_raw_string))
      bodies
  in
  let part_count = List.length bodies in
  Object.Etag.of_string_exn
    ("\""
    ^ Digestif.MD5.(digest_string (String.concat "" part_digests) |> to_hex)
    ^ "-"
    ^ string_of_int part_count
    ^ "\"")

let empty_checksum = { Object.Checksum.values = []; checksum_type = None }

let checksum_response ?checksum_type values =
  { Object.Checksum.values; checksum_type }

let bad_digest algorithm =
  Error
    (Awskit.Error.Producer.service ~status:400 ~code:"BadDigest"
       ~message:
         ("checksum "
         ^ Object.Checksum.Algorithm.to_string algorithm
         ^ " did not match payload")
       ~headers:[] ())

let unsupported_algorithm algorithm =
  invalid ~field:"checksum_algorithm"
    "simulator does not compute checksum algorithm %S"
    (Object.Checksum.Algorithm.to_string algorithm)

let validate_supported_algorithm = function
  | Object.Checksum.Algorithm.Md5 | Sha1 | Sha256 | Sha512 -> Ok ()
  | algorithm -> unsupported_algorithm algorithm

let computed_value ~body algorithm =
  let raw =
    match algorithm with
    | Object.Checksum.Algorithm.Md5 ->
        Ok Digestif.MD5.(digest_string body |> to_raw_string)
    | Sha1 -> Ok Digestif.SHA1.(digest_string body |> to_raw_string)
    | Sha256 -> Ok Digestif.SHA256.(digest_string body |> to_raw_string)
    | Sha512 -> Ok Digestif.SHA512.(digest_string body |> to_raw_string)
    | algorithm -> unsupported_algorithm algorithm
  in
  let* raw = raw in
  Object.Checksum.value ~algorithm ~value:(Base64.encode_exn raw)

let checksum_for_value ~body = function
  | None -> Ok empty_checksum
  | Some (value : Object.Checksum.value) ->
      let* computed = computed_value ~body value.algorithm in
      if String.equal value.value computed.value then
        Ok (checksum_response [ value ])
      else bad_digest value.algorithm

let checksum_for_algorithm ~body = function
  | None -> Ok empty_checksum
  | Some algorithm ->
      let* value = computed_value ~body algorithm in
      Ok (checksum_response [ value ])

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
