let prefix = "x-amz-meta-"

let of_headers headers =
  let metadata =
    List.filter_map
      (fun (key, value) ->
        let lower = String.lowercase_ascii key in
        if S3_string.is_prefix ~prefix lower then
          Some
            ( String.sub key (String.length prefix)
                (String.length key - String.length prefix),
              value )
        else None)
      headers
  in
  match Metadata.of_list metadata with
  | Ok _ as result -> result
  | Error error ->
      Error
        (S3_error_context.decode_with_context ~what:"S3 metadata headers"
           (Awskit.Error.to_string_hum error))

let to_headers metadata =
  List.map
    (fun (key, value) -> (prefix ^ key, value))
    (Metadata.to_list metadata)
