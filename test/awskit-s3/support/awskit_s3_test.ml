open Awskit_s3

let test_time = Awskit_test.Time.fixed
let credentials = Awskit_test.Credentials.basic
let creds = credentials

let ok_or_fail label result =
  Awskit_test.Expect.ok_or_fail Error.pp label result

let header = Awskit_test.Header.find
let string_contains = Awskit_test.String.contains
let version_string = Option.map Object.Version_id.to_string
let query_param name url = Uri.query (Uri.of_string url) |> List.assoc_opt name

let signed_headers_or_fail url =
  match query_param "X-Amz-SignedHeaders" url with
  | Some [ value ] -> String.split_on_char ';' value
  | _ -> Alcotest.fail "missing signed headers"

let check_checksum label algorithm value (checksum : Object.Checksum.response) =
  match
    List.find_opt
      (fun (actual : Object.Checksum.value) -> actual.algorithm = algorithm)
      checksum.values
  with
  | None -> Alcotest.failf "%s: expected checksum" label
  | Some actual ->
      Alcotest.(check bool)
        (label ^ " algorithm") true
        (actual.algorithm = algorithm);
      Alcotest.(check string) (label ^ " value") value actual.value

let expect_not_found label = function
  | Error error when Error.is_not_found error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected not found" label

let expect_status label status = function
  | Error error when Awskit.Error.service_status error = Some status -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected service status %d" label status

let expect_precondition_failed label result = expect_status label 412 result
let expect_not_modified label result = expect_status label 304 result

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let tag key value = { Tag.key; value }

module Recording_runtime = struct
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;
    read_error_after : int option;
  }

  type call = { request : Awskit.Request.t; body : string }

  type request_body = {
    body : (string, Awskit.Error.t) result;
    descriptor : Awskit.Body.Request.descriptor;
  }

  type connection = {
    region : Region.t;
    credentials : Credentials.t;
    endpoint_config : Awskit_s3.endpoint_config;
    retry_policy : Awskit.Retry.t;
    mutable calls : call list;
    mutable responses : response list;
    mutable sleeps : Ptime.Span.t list;
    mutable discarded : string list;
  }

  type 'a t = 'a

  let return value = value
  let bind value f = f value

  type response_body = { body : string; read_error_after : int option }
  type request_body_writer = Buffer.t

  type response_body_reader = {
    body : string;
    read_error_after : int option;
    mutable offset : int;
  }

  let connect ?(endpoint_config = Awskit_s3.default_endpoint_config)
      ?(region = Region.of_string_exn "us-east-1")
      ?(retry_policy = Awskit.Retry.default) responses =
    {
      region;
      credentials = creds;
      endpoint_config;
      retry_policy;
      calls = [];
      responses;
      sleeps = [];
      discarded = [];
    }

  let last_call conn =
    match conn.calls with
    | call :: _ -> call
    | [] -> Alcotest.fail "expected recorded request"

  let now _ = test_time
  let region conn = conn.region
  let credentials conn = Ok conn.credentials
  let s3_endpoint_config conn = conn.endpoint_config
  let retry_policy conn = conn.retry_policy
  let sleep conn span = conn.sleeps <- span :: conn.sleeps
  let endpoint _ = None

  let descriptor ?(replayable = true) body =
    {
      Awskit.Body.Request.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable;
    }

  let request_body ?replayable body =
    { body = Ok body; descriptor = descriptor ?replayable body }

  let empty_request_body = request_body ""
  let string_request_body body = request_body body
  let bytes_request_body body = request_body (Bytes.to_string body)

  let stream_request_body descriptor ~write =
    let buffer = Buffer.create 128 in
    let body =
      match write buffer with
      | Ok () -> (
          let body = Buffer.contents buffer in
          let length = Int64.of_int (String.length body) in
          match descriptor.Awskit.Body.Request.content_length with
          | Some declared when not (Stdlib.Int64.equal declared length) ->
              Error (Awskit.Error.body "request body length mismatch")
          | _ -> Ok body)
      | Error _ as error -> error
    in
    { body; descriptor }

  let request_body_descriptor body = body.descriptor

  let write_request_body_string writer body =
    Buffer.add_string writer body;
    Ok ()

  let read_response_body reader bytes ~off ~len =
    if len = 0 then Ok 0
    else if
      match reader.read_error_after with
      | Some limit -> reader.offset >= limit
      | None -> false
    then Stdlib.Error (Awskit.Error.body "simulated download read failure")
    else
      let remaining = String.length reader.body - reader.offset in
      if remaining <= 0 then Ok 0
      else
        let copied = min len remaining in
        String.blit reader.body reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        Ok copied

  let rec drain reader =
    let bytes = Bytes.create 8 in
    match read_response_body reader bytes ~off:0 ~len:(Bytes.length bytes) with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> drain reader

  let with_response_body (body : response_body) ~consume =
    let reader =
      { body = body.body; read_error_after = body.read_error_after; offset = 0 }
    in
    match consume reader with
    | Ok _ as result -> (
        match drain reader with Ok () -> result | Error _ as error -> error)
    | Error _ as error -> (
        match drain reader with
        | Ok () -> error
        | Error _ as drain_error -> drain_error)

  let discard_response_body (body : response_body) =
    let reader =
      { body = body.body; read_error_after = body.read_error_after; offset = 0 }
    in
    let result = drain reader in
    result

  module Request_body = struct
    let empty = empty_request_body
    let of_string = string_request_body
    let of_bytes = bytes_request_body
    let of_stream = stream_request_body
    let descriptor = request_body_descriptor
    let write_string = write_request_body_string
  end

  module Response_body = struct
    let read = read_response_body
    let with_reader = with_response_body
    let discard = discard_response_body
  end

  let with_response conn request (body : request_body) ~f =
    match body.body with
    | Error _ as error -> error
    | Ok body -> (
        conn.calls <- { request; body } :: conn.calls;
        match conn.responses with
        | [] ->
            Error (Awskit.Error.transport ~retryable:false "no canned response")
        | response :: rest ->
            conn.responses <- rest;
            f
              (Awskit.Response.create_exn ~status:response.status
                 ~headers:response.headers ())
              {
                body = response.body;
                read_error_after = response.read_error_after;
              })
end

module Recording_s3 = Awskit_s3.Make (Recording_runtime)

let response ?(headers = []) ?read_error_after status body =
  { Recording_runtime.status; headers; body; read_error_after }

let list_page ?continuation_token ?next_continuation_token ~truncated keys =
  let token_xml name = function
    | None -> ""
    | Some value -> Fmt.str "<%s>%s</%s>" name value name
  in
  let contents =
    keys
    |> List.map (fun key ->
        Fmt.str
          "<Contents><Key>%s</Key><Size>1</Size><ETag>\"etag\"</ETag></Contents>"
          key)
    |> String.concat ""
  in
  Fmt.str
    "<ListBucketResult><Name>my-bucket</Name><IsTruncated>%b</IsTruncated>%s%s%s</ListBucketResult>"
    truncated
    (token_xml "ContinuationToken" continuation_token)
    (token_xml "NextContinuationToken" next_continuation_token)
    contents

let list_parts_page ?next_part_number_marker ~truncated part_numbers =
  let next_marker_xml =
    match next_part_number_marker with
    | None -> ""
    | Some marker ->
        Fmt.str "<NextPartNumberMarker>%d</NextPartNumberMarker>" marker
  in
  let parts =
    part_numbers
    |> List.map (fun part_number ->
        Fmt.str
          "<Part><PartNumber>%d</PartNumber><ETag>\"etag-%d\"</ETag><Size>1</Size></Part>"
          part_number part_number)
    |> String.concat ""
  in
  Fmt.str "<ListPartsResult><IsTruncated>%b</IsTruncated>%s%s</ListPartsResult>"
    truncated next_marker_xml parts
