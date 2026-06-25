let fixture_dir =
  List.fold_left Filename.concat ".." [ "fixtures"; "protocol"; "fuzz-replay" ]

let corpus_path parts = List.fold_left Filename.concat fixture_dir parts

let sorted_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.map (Filename.concat dir)
  |> List.filter (fun path ->
      (not (Sys.is_directory path))
      && not (Filename.check_suffix path ".expected"))

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
