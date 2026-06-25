open Awskit_s3

let qcheck_seed = 0xA5111
let to_alcotest = Awskit_test.Qcheck.to_alcotest ~seed:qcheck_seed
let chars_of_string value = List.init (String.length value) (String.get value)
let gen_from_chars chars = QCheck.Gen.oneof_list (chars_of_string chars)

let gen_string ~min ~max ~chars =
  let open QCheck.Gen in
  string_size ~gen:(gen_from_chars chars) (int_range min max)

let query_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_.~"

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let query_params_gen =
  let open QCheck.Gen in
  let key_gen = gen_string ~min:1 ~max:12 ~chars:query_chars in
  let value_gen = gen_string ~min:0 ~max:12 ~chars:query_chars in
  list_size (int_range 0 12)
    (pair key_gen (list_size (int_range 0 3) value_gen))

let split_ampersand value =
  if String.equal value "" then [] else String.split_on_char '&' value

let expanded_count params =
  List.fold_left
    (fun count (_key, values) -> count + max 1 (List.length values))
    0 params

let expected_canonical_query params =
  params
  |> List.concat_map (fun (key, values) ->
      match values with
      | [] -> [ (key, "") ]
      | values -> List.map (fun value -> (key, value)) values)
  |> List.map (fun (key, value) ->
      (Awskit.Signing.uri_encode key, Awskit.Signing.uri_encode value))
  |> List.sort (fun (left_key, left_value) (right_key, right_value) ->
      match String.compare left_key right_key with
      | 0 -> String.compare left_value right_value
      | order -> order)
  |> List.map (fun (key, value) -> key ^ "=" ^ value)
  |> String.concat "&"

let prop_canonical_query_params_sorted =
  QCheck.Test.make ~count:500 ~name:"canonical query params sort encoded pairs"
    (QCheck.make
       ~print:(fun params ->
         params
         |> List.map (fun (key, values) ->
             Fmt.str "%s=[%s]" key (String.concat "," values))
         |> String.concat ";")
       query_params_gen)
    (fun params ->
      let canonical = Awskit.Signing.canonical_query_params params in
      let pairs = split_ampersand canonical in
      List.length pairs = expanded_count params
      && String.equal canonical (expected_canonical_query params))

let content_range_valid_gen =
  let open QCheck.Gen in
  let* start = int_range 0 100_000 in
  let* length = int_range 1 10_000 in
  let finish = start + length - 1 in
  let* complete_length =
    oneof_weighted
      [
        (1, return None);
        (3, map (fun extra -> Some (finish + extra)) (int_range 1 10_000));
      ]
  in
  return
    ( Int64.of_int start,
      Int64.of_int finish,
      Option.map Int64.of_int complete_length )

let content_range_header (start, finish, complete_length) =
  let length =
    match complete_length with
    | None -> "*"
    | Some length -> Int64.to_string length
  in
  Fmt.str "bytes %Ld-%Ld/%s" start finish length

let content_range_matches (start, finish, complete_length)
    (value : Range.Content_range.t) =
  Int64.equal start value.start
  && Int64.equal finish value.finish
  && Option.equal Int64.equal complete_length value.complete_length

let content_range_equal (left : Range.Content_range.t)
    (right : Range.Content_range.t) =
  Int64.equal left.start right.start
  && Int64.equal left.finish right.finish
  && Option.equal Int64.equal left.complete_length right.complete_length

let prop_content_range_valid_round_trips =
  QCheck.Test.make ~count:500 ~name:"Content-Range valid values round-trip"
    (QCheck.make
       ~print:(fun value -> content_range_header value)
       content_range_valid_gen)
    (fun generated ->
      let header = content_range_header generated in
      match Range.Content_range.of_header header with
      | Error _ -> false
      | Ok value -> (
          match
            Range.Content_range.to_header value |> Range.Content_range.of_header
          with
          | Error _ -> false
          | Ok reparsed ->
              content_range_matches generated value
              && content_range_equal value reparsed))

let content_range_invalid_gen =
  let open QCheck.Gen in
  let* start = int_range 0 1000 in
  let* finish_delta = int_range 0 1000 in
  let finish = start + finish_delta in
  oneof
    [
      return (Fmt.str "items %d-%d/%d" start finish (finish + 1));
      return (Fmt.str "bytes x-%d/%d" finish (finish + 1));
      return (Fmt.str "bytes %d-x/%d" start (finish + 1));
      return (Fmt.str "bytes %d-%d/x" start finish);
      return (Fmt.str "bytes %d-%d/%d" (finish + 1) finish (finish + 2));
      return (Fmt.str "bytes %d-%d/%d" start finish finish);
      return
        (Fmt.str "bytes %d-%d/%d/%d" start finish (finish + 1) (finish + 2));
      return
        (Fmt.str "bytes %d-%d-%d/%d" start finish (finish + 1) (finish + 2));
    ]

let prop_content_range_invalid_headers_decode_error =
  QCheck.Test.make ~count:300
    ~name:"Content-Range invalid boundary families are decode errors"
    (QCheck.make ~print:Fun.id content_range_invalid_gen) (fun header ->
      match Range.Content_range.of_header header with
      | Error error -> is_decode_error error
      | Ok _ -> false)

let prop_endpoint_rejects_url_parts =
  let gen =
    let open QCheck.Gen in
    let host = gen_string ~min:3 ~max:20 ~chars:"abcdefghijklmnopqrstuvwxyz" in
    let path = gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz" in
    let fragment =
      map2 (fun host path -> Fmt.str "https://%s#%s" host path) host path
    in
    let bad_port =
      map2
        (fun host port -> Fmt.str "https://%s:%s" host port)
        host
        (oneof_list [ "0"; "65536"; "-1"; "abc"; "" ])
    in
    let empty_host =
      oneof_list [ "https://"; "http://"; "https://:443"; "http://:80" ]
    in
    let malformed_ipv6 =
      oneof_list
        [
          "https://[::1";
          "https://[::1]extra";
          "https://::1";
          "https://[::1]:abc";
        ]
    in
    let unsupported_scheme =
      map2
        (fun scheme host -> Fmt.str "%s://%s" scheme host)
        (oneof_list [ "ftp"; "s3"; "file"; "https+unix" ])
        host
    in
    let userinfo =
      map2 (fun host path -> Fmt.str "https://user:%s@%s" path host) host path
    in
    let with_path =
      map2 (fun host path -> Fmt.str "https://%s/%s" host path) host path
    in
    let with_query =
      map2 (fun host path -> Fmt.str "https://%s?%s=1" host path) host path
    in
    let control =
      map2
        (fun host char -> Fmt.str "https://%s%cexample.com" host char)
        host
        (oneof_list [ '\000'; '\n'; '\r'; '\t'; '\127' ])
    in
    oneof
      [
        fragment;
        bad_port;
        empty_host;
        malformed_ipv6;
        unsupported_scheme;
        userinfo;
        with_path;
        with_query;
        control;
      ]
  in
  QCheck.Test.make ~count:200
    ~name:"endpoint parser rejects URL parts and malformed authorities"
    (QCheck.make ~print:String.escaped gen) (fun value ->
      Result.is_error (Awskit.Endpoint.of_string value))

let prop_header_values_reject_newline =
  let gen =
    let open QCheck.Gen in
    let piece =
      gen_string ~min:0 ~max:20
        ~chars:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    in
    map2 (fun left right -> left ^ "\n" ^ right) piece piece
  in
  QCheck.Test.make ~count:200 ~name:"request header values reject newline"
    (QCheck.make ~print:(fun value -> String.escaped value) gen)
    (fun value ->
      Result.is_error (Awskit.Request.validate_headers [ ("x-test", value) ]))

let prop_metadata_rejects_case_insensitive_duplicate_keys =
  let key_gen = gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz" in
  let value_gen =
    gen_string ~min:0 ~max:12
      ~chars:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  in
  QCheck.Test.make ~count:200
    ~name:"metadata rejects case-insensitive duplicate keys"
    (QCheck.make
       ~print:(fun (key, first, second) -> Fmt.str "%s=%s/%s" key first second)
       QCheck.Gen.(triple key_gen value_gen value_gen))
    (fun (key, first, second) ->
      Result.is_error
        (Metadata.of_list
           [ (key, first); (String.uppercase_ascii key, second) ]))

let prop_download_ranges_cover_content_length =
  let gen = QCheck.Gen.(pair (int_range 0 10000) (int_range 1 4096)) in
  QCheck.Test.make ~count:300
    ~name:"download ranges exactly cover content length"
    (QCheck.make
       ~print:(fun (content_length, part_size) ->
         Fmt.str "content_length=%d part_size=%d" content_length part_size)
       gen)
    (fun (content_length, part_size) ->
      match
        Transfer.Plan.download_ranges
          ~content_length:(Int64.of_int content_length)
          ~part_size
      with
      | Error _ -> false
      | Ok ranges ->
          let total =
            List.fold_left
              (fun acc (range : Transfer.Plan.download_range) ->
                acc + range.length)
              0 ranges
          in
          let rec contiguous expected_offset = function
            | [] -> true
            | (range : Transfer.Plan.download_range) :: rest ->
                Int64.equal range.offset (Int64.of_int expected_offset)
                && contiguous (expected_offset + range.length) rest
          in
          total = content_length && contiguous 0 ranges)

let retry_error = Awskit.Error.Producer.transport ~retryable:true "temporary"

let prop_retry_jitter_stays_within_policy_bounds =
  let gen = QCheck.Gen.(pair (int_range 1 8) (int_range 0 1000)) in
  let base_delay = Ptime.Span.of_int_s 1 in
  let max_delay = Ptime.Span.of_int_s 4 in
  let policy =
    Awskit.Retry.create_exn ~max_attempts:10 ~base_delay ~max_delay ~jitter:1.0
      ()
  in
  QCheck.Test.make ~count:200
    ~name:"retry jitter stays within configured delay bounds"
    (QCheck.make
       ~print:(fun (attempt, sample) ->
         Fmt.str "attempt=%d sample=%d" attempt sample)
       gen)
    (fun (attempt, sample) ->
      let random_float ~upper_bound = upper_bound *. (float sample /. 1000.0) in
      match
        Awskit.Retry.delay policy ~attempt ~error:retry_error ~random_float
      with
      | None -> false
      | Some delay ->
          let seconds = Ptime.Span.to_float_s delay in
          seconds >= 0.0 && seconds <= 4.0)

let suite =
  [
    ( "pbt:protocol",
      List.map to_alcotest
        [
          prop_canonical_query_params_sorted;
          prop_content_range_valid_round_trips;
          prop_content_range_invalid_headers_decode_error;
          prop_endpoint_rejects_url_parts;
          prop_header_values_reject_newline;
          prop_metadata_rejects_case_insensitive_duplicate_keys;
          prop_download_ranges_cover_content_length;
          prop_retry_jitter_stays_within_policy_bounds;
        ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-pbt" suite
