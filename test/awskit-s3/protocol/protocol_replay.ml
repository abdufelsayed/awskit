let fixture_dir =
  List.fold_left Filename.concat ".." [ "fixtures"; "protocol"; "fuzz-replay" ]

let corpus_path parts = List.fold_left Filename.concat fixture_dir parts

let is_ignored_corpus_file path =
  String.equal (Filename.basename path) "README.md"
  || Filename.check_suffix path ".expected"

let sorted_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.map (Filename.concat dir)
  |> List.filter (fun path ->
      (not (Sys.is_directory path)) && not (is_ignored_corpus_file path))

let replay_files dir =
  let rec loop dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> List.concat_map (fun path ->
        if Sys.is_directory path then loop path
        else if is_ignored_corpus_file path then []
        else [ path ])
  in
  loop dir |> List.sort String.compare

type operation =
  | Endpoint_of_string
  | Request_validate_headers
  | S3_bucket_tagging_get

type input_kind = Normalized_text | Bytes_hex

type replay_record = {
  fixture_path : string;
  operation : operation;
  input_kind : input_kind;
  input : string;
  expected_error_category : string;
}

let operation_of_string = function
  | "endpoint-of-string" -> Some Endpoint_of_string
  | "request-validate-headers" -> Some Request_validate_headers
  | "s3-bucket-tagging-get" -> Some S3_bucket_tagging_get
  | _ -> None

let input_kind_to_string = function
  | Normalized_text -> "normalized-text"
  | Bytes_hex -> "bytes-hex"

let input_kind_of_string = function
  | "normalized-text" -> Some Normalized_text
  | "bytes-hex" -> Some Bytes_hex
  | _ -> None

let expected_input_kind = function
  | Endpoint_of_string -> Normalized_text
  | Request_validate_headers | S3_bucket_tagging_get -> Bytes_hex

let parse_error path line message = Fmt.failwith "%s:%d: %s" path line message

let split_once_on_equal path line text =
  match String.index_opt text '=' with
  | None -> parse_error path line "expected key=value line"
  | Some index ->
      let key = String.sub text 0 index in
      let value =
        String.sub text (index + 1) (String.length text - index - 1)
      in
      (key, value)

let split_lines text =
  let lines = String.split_on_char '\n' text in
  match List.rev lines with "" :: rest -> List.rev rest | _ -> lines

let parse_field path line expected_key text =
  let key, value = split_once_on_equal path line text in
  if String.equal key expected_key then value
  else
    parse_error path line
      (Fmt.str "expected %S field, found %S" expected_key key)

let parse_record path text =
  match split_lines text with
  | [ _; _; _; _; "expected-error:" ] ->
      parse_error path 5 "expected public error category after expected-error:"
  | fixture_path_line
    :: operation_line
    :: input_kind_line
    :: input_line
    :: "expected-error:"
    :: expected_error_lines ->
      let fixture_path = parse_field path 1 "fixture-path" fixture_path_line in
      let operation =
        let value = parse_field path 2 "operation" operation_line in
        match operation_of_string value with
        | Some operation -> operation
        | None -> parse_error path 2 (Fmt.str "unknown operation %S" value)
      in
      let input_kind =
        let value = parse_field path 3 "input-kind" input_kind_line in
        match input_kind_of_string value with
        | Some input_kind -> input_kind
        | None -> parse_error path 3 (Fmt.str "unknown input kind %S" value)
      in
      let input = parse_field path 4 "input" input_line in
      {
        fixture_path;
        operation;
        input_kind;
        input;
        expected_error_category = String.concat "\n" expected_error_lines;
      }
  | _ ->
      parse_error path 1
        "expected fixture-path, operation, input-kind, input, and \
         expected-error fields"

let read_record path =
  let sidecar_path = path ^ ".expected" in
  Protocol_fixture_diff.read_file sidecar_path |> parse_record sidecar_path

let relative_fixture_path path =
  let prefix = fixture_dir ^ Filename.dir_sep in
  let prefix_length = String.length prefix in
  if
    String.length path >= prefix_length
    && String.equal prefix (String.sub path 0 prefix_length)
  then String.sub path prefix_length (String.length path - prefix_length)
  else path

let hex_digit value =
  let digits = "0123456789abcdef" in
  digits.[value]

let hex_encode text =
  String.init
    (String.length text * 2)
    (fun index ->
      let byte = Char.code text.[index / 2] in
      if index mod 2 = 0 then hex_digit (byte lsr 4)
      else hex_digit (byte land 0x0f))

let retry_class_to_string = function
  | Awskit.Error.Retryable -> "retryable"
  | Throttled -> "throttled"
  | Auth -> "auth"
  | Conflict -> "conflict"
  | Not_found -> "not-found"
  | Fatal -> "fatal"
  | Unknown -> "unknown"

let error_category error =
  let category =
    match Awskit.Error.kind error with
    | Validation validation ->
        Fmt.str "kind=validation\nfield=%s"
          (Option.value ~default:"none" validation.field)
    | Endpoint endpoint ->
        Fmt.str "kind=endpoint\nuri=%s"
          (Option.value ~default:"none" endpoint.uri)
    | Decode _ -> "kind=decode"
    | Body _ -> "kind=body"
    | Transport transport ->
        Fmt.str "kind=transport\nretryable=%b" transport.retryable
    | Service service -> Fmt.str "kind=service\nstatus=%d" service.status
    | Credentials _ -> "kind=credentials"
    | Signing _ -> "kind=signing"
    | Timeout _ -> "kind=timeout"
    | Cancelled _ -> "kind=cancelled"
    | Retry_exhausted retry ->
        Fmt.str "kind=retry-exhausted\nattempts=%d" retry.attempts
    | Not_supported not_supported ->
        Fmt.str "kind=not-supported\nfeature=%s"
          (Option.value ~default:"none" not_supported.feature)
    | Multiple errors -> Fmt.str "kind=multiple\ncount=%d" (List.length errors)
  in
  Fmt.str "%s\nretry-class=%s" category
    (retry_class_to_string (Awskit.Error.retry_class error))

let classify_error = error_category
