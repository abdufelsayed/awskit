open Awskit_s3

let test_time = Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.get

let creds =
  Credentials.create_exn ~access_key_id:"AKID" ~secret_access_key:"SECRET" ()

let error_t = Alcotest.testable Error.pp Error.equal

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Error.pp error

let header name headers = List.assoc_opt name headers

let checksum_value = function
  | None -> None
  | Some (checksum : Object.Checksum.response) -> Some checksum.value

let query_param name url = Uri.query (Uri.of_string url) |> List.assoc_opt name

let check_checksum label algorithm value = function
  | None -> Alcotest.failf "%s: expected checksum" label
  | Some (checksum : Object.Checksum.response) ->
      Alcotest.(check bool)
        (label ^ " algorithm") true
        (checksum.algorithm = algorithm);
      Alcotest.(check string) (label ^ " value") value checksum.value

let test_provider_resolution () =
  let provider = Provider.aws () in
  let request =
    Provider.resolve_object_request provider
      ~region:(Region.of_string_exn "us-east-1")
      ~bucket:"my-bucket" ~key:"dir/file.txt"
    |> ok_or_fail "resolve object"
  in
  Alcotest.(check string)
    "virtual host" "my-bucket.s3.us-east-1.amazonaws.com"
    (Endpoint.host request.endpoint);
  Alcotest.(check string) "path" "/dir/file.txt" request.path;
  let dotted =
    Provider.resolve_object_request provider
      ~region:(Region.of_string_exn "us-east-1")
      ~bucket:"my.bucket" ~key:"file.txt"
    |> ok_or_fail "resolve dotted object"
  in
  Alcotest.(check string)
    "path style host" "s3.us-east-1.amazonaws.com"
    (Endpoint.host dotted.endpoint);
  Alcotest.(check string) "path style path" "/my.bucket/file.txt" dotted.path

let test_provider_variants () =
  let provider = Provider.aws ~endpoint_variant:`Dualstack () in
  let endpoint =
    Provider.endpoint provider ~region:(Region.of_string_exn "eu-west-1")
    |> ok_or_fail "dualstack endpoint"
  in
  Alcotest.(check string)
    "dualstack" "s3.dualstack.eu-west-1.amazonaws.com" (Endpoint.host endpoint);
  let accelerate =
    Provider.aws ~endpoint_variant:`Accelerate_dualstack ()
    |> Provider.endpoint ~region:(Region.of_string_exn "us-west-2")
    |> ok_or_fail "accelerate endpoint"
  in
  Alcotest.(check string)
    "accelerate" "s3-accelerate.dualstack.amazonaws.com"
    (Endpoint.host accelerate)

let test_presigned_result () =
  let result =
    Presigned.get_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt" ()
    |> ok_or_fail "presigned get"
  in
  Alcotest.(check string)
    "method" "GET"
    (Awskit.Request.Method.to_string
       (result.method_ :> Awskit.Request.Method.t));
  Alcotest.(check bool)
    "has signature" true
    (String.contains result.url '?'
    && String.contains result.url '='
    && String.contains result.url '&');
  Alcotest.(check bool)
    "virtual-hosted URL" true
    (String.starts_with ~prefix:"https://bucket.s3.us-east-1.amazonaws.com/"
       result.url)

let test_presigned_put_checksum_headers () =
  let checksum : Object.Checksum.request =
    { Object.Checksum.algorithm = `SHA1; value = Some "provided-sha1" }
  in
  let options =
    { Presigned.Put_object.default_options with checksum = Some checksum }
  in
  let result =
    Presigned.put_object
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~bucket:"bucket" ~key:"file.txt"
      ~options ()
    |> ok_or_fail "presigned put"
  in
  Alcotest.(check (option string))
    "checksum algorithm header" (Some "SHA1")
    (header "x-amz-checksum-algorithm" result.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha1")
    (header "x-amz-checksum-sha1" result.headers);
  let signed_headers =
    match query_param "X-Amz-SignedHeaders" result.url with
    | Some [ value ] -> String.split_on_char ';' value
    | _ -> Alcotest.fail "missing signed headers"
  in
  Alcotest.(check bool)
    "signed checksum algorithm" true
    (List.mem "x-amz-checksum-algorithm" signed_headers);
  Alcotest.(check bool)
    "signed checksum value" true
    (List.mem "x-amz-checksum-sha1" signed_headers)

module Recording_runtime = struct
  type response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  type call = { request : Awskit.Request.t; body : string }

  type connection = {
    region : Region.t;
    credentials : Credentials.t;
    provider : Provider.t;
    retry_policy : Awskit.Retry.t;
    mutable calls : call list;
    mutable responses : response list;
    mutable sleeps : Ptime.Span.t list;
    mutable discarded : string list;
  }

  type 'a t = 'a

  let return value = value
  let bind value f = f value

  type upload_body = string
  type download_body = string
  type upload_writer = Buffer.t
  type download_reader = { body : string; mutable offset : int }

  let connect ?(provider = Provider.default)
      ?(region = Region.of_string_exn "us-east-1")
      ?(retry_policy = Awskit.Retry.default) responses =
    {
      region;
      credentials = creds;
      provider;
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
  let s3_provider conn = conn.provider
  let retry_policy conn = conn.retry_policy
  let sleep conn span = conn.sleeps <- span :: conn.sleeps

  let endpoint conn =
    Provider.endpoint conn.provider ~region:conn.region |> Result.to_option

  let descriptor body =
    {
      Awskit.Body.Upload.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let empty_body = ""
  let string_body body = body
  let bytes_body body = Bytes.to_string body

  let stream_body _descriptor ~write =
    let buffer = Buffer.create 128 in
    match write buffer with Ok () -> Buffer.contents buffer | Error _ -> ""

  let upload_descriptor = descriptor

  let write_string writer body =
    Buffer.add_string writer body;
    Ok ()

  let read reader bytes ~off ~len =
    if len = 0 then Ok 0
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
    match read reader bytes ~off:0 ~len:(Bytes.length bytes) with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> drain reader

  let with_download_body body ~consume =
    let reader = { body; offset = 0 } in
    match consume reader with
    | Ok _ as result -> (
        match drain reader with Ok () -> result | Error _ as error -> error)
    | Error _ as error -> (
        match drain reader with
        | Ok () -> error
        | Error _ as drain_error -> drain_error)

  let discard_download_body body =
    let reader = { body; offset = 0 } in
    let result = drain reader in
    result

  let call conn request body =
    conn.calls <- { request; body } :: conn.calls;
    match conn.responses with
    | [] -> Error (Awskit.Error.transport ~retryable:false "no canned response")
    | response :: rest ->
        conn.responses <- rest;
        Ok
          ( Awskit.Response.create_exn ~status:response.status
              ~headers:response.headers (),
            response.body )
end

module Recording_s3 = Awskit_s3.Make (Recording_runtime)

let response ?(headers = []) status body =
  { Recording_runtime.status; headers; body }

let test_bucket_head_request () =
  let conn =
    Recording_runtime.connect
      [ response 200 ~headers:[ ("x-amz-bucket-region", "us-west-2") ] "" ]
  in
  let info =
    Recording_s3.Bucket.head conn ~bucket:"my-bucket"
    |> ok_or_fail "bucket head"
  in
  Alcotest.(check (option string))
    "region" (Some "us-west-2")
    (Option.map Awskit.Region.to_string info.region);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string)
    "host" "my-bucket.s3.us-east-1.amazonaws.com" call.request.target.host;
  Alcotest.(check string) "path" "/" call.request.target.path

let test_bucket_list_parse () =
  let body =
    {|<ListAllMyBucketsResult><Buckets><Bucket><Name>alpha</Name><CreationDate>2026-04-08T12:00:00Z</CreationDate></Bucket><Bucket><Name>zeta</Name></Bucket></Buckets></ListAllMyBucketsResult>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let buckets = Recording_s3.Bucket.list conn |> ok_or_fail "bucket list" in
  Alcotest.(check (list string))
    "bucket names" [ "alpha"; "zeta" ]
    (List.map (fun (bucket : Bucket.info) -> bucket.name) buckets);
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string)
    "root host" "s3.us-east-1.amazonaws.com" call.request.target.host

let test_bucket_config_parse () =
  let versioning =
    {|<VersioningConfiguration><Status>Enabled</Status></VersioningConfiguration>|}
  in
  let tagging =
    {|<Tagging><TagSet><Tag><Key>env</Key><Value>prod</Value></Tag></TagSet></Tagging>|}
  in
  let encryption =
    {|<ServerSideEncryptionConfiguration><Rule><ApplyServerSideEncryptionByDefault><SSEAlgorithm>aws:kms</SSEAlgorithm><KMSMasterKeyID>key-1</KMSMasterKeyID></ApplyServerSideEncryptionByDefault></Rule></ServerSideEncryptionConfiguration>|}
  in
  let cors =
    {|<CORSConfiguration><CORSRule><ID>web</ID><AllowedOrigin>https://example.com</AllowedOrigin><AllowedMethod>GET</AllowedMethod><AllowedHeader>*</AllowedHeader><ExposeHeader>etag</ExposeHeader><MaxAgeSeconds>300</MaxAgeSeconds></CORSRule></CORSConfiguration>|}
  in
  let website =
    {|<WebsiteConfiguration><IndexDocument><Suffix>index.html</Suffix></IndexDocument><ErrorDocument><Key>error.html</Key></ErrorDocument></WebsiteConfiguration>|}
  in
  let public_access_block =
    {|<PublicAccessBlockConfiguration><BlockPublicAcls>true</BlockPublicAcls><IgnorePublicAcls>false</IgnorePublicAcls><BlockPublicPolicy>true</BlockPublicPolicy><RestrictPublicBuckets>false</RestrictPublicBuckets></PublicAccessBlockConfiguration>|}
  in
  let ownership =
    {|<OwnershipControls><Rule><ObjectOwnership>BucketOwnerEnforced</ObjectOwnership></Rule></OwnershipControls>|}
  in
  let request_payment =
    {|<RequestPaymentConfiguration><Payer>Requester</Payer></RequestPaymentConfiguration>|}
  in
  let accelerate =
    {|<AccelerateConfiguration><Status>Enabled</Status></AccelerateConfiguration>|}
  in
  let policy_status =
    {|<PolicyStatus><IsPublic>false</IsPublic></PolicyStatus>|}
  in
  let logging =
    {|<BucketLoggingStatus><LoggingEnabled><TargetBucket>log-bucket</TargetBucket><TargetPrefix>logs/</TargetPrefix></LoggingEnabled></BucketLoggingStatus>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 versioning;
        response 200 tagging;
        response 200 encryption;
        response 200 cors;
        response 200 website;
        response 200 public_access_block;
        response 200 ownership;
        response 200 request_payment;
        response 200 accelerate;
        response 200 policy_status;
        response 200 logging;
      ]
  in
  let versioning =
    Recording_s3.Bucket.Versioning.get conn ~bucket:"my-bucket"
    |> ok_or_fail "versioning"
  in
  Alcotest.(check bool)
    "versioning enabled" true
    (versioning.status = Some Bucket.Versioning.Status.Enabled);
  let tagging =
    Recording_s3.Bucket.Tagging.get conn ~bucket:"my-bucket"
    |> ok_or_fail "tagging"
  in
  Alcotest.(check int) "tag count" 1 (List.length tagging.tags);
  let encryption =
    Recording_s3.Bucket.Encryption.get conn ~bucket:"my-bucket"
    |> ok_or_fail "encryption"
  in
  (match encryption.config.rules with
  | [ rule ] ->
      Alcotest.(check string)
        "algorithm" "aws:kms"
        (Bucket.Encryption.Algorithm.to_string rule.sse_algorithm);
      Alcotest.(check (option string))
        "kms key" (Some "key-1") rule.kms_master_key_id
  | _ -> Alcotest.fail "expected one encryption rule");
  let cors =
    Recording_s3.Bucket.Cors.get conn ~bucket:"my-bucket" |> ok_or_fail "cors"
  in
  Alcotest.(check int) "cors rule count" 1 (List.length cors.config.rules);
  let website =
    Recording_s3.Bucket.Website.get conn ~bucket:"my-bucket"
    |> ok_or_fail "website"
  in
  Alcotest.(check (option string))
    "index" (Some "index.html") website.config.index_document_suffix;
  let public_access_block =
    Recording_s3.Bucket.Public_access_block.get conn ~bucket:"my-bucket"
    |> ok_or_fail "public access block"
  in
  Alcotest.(check bool)
    "block public policy" true public_access_block.config.block_public_policy;
  let ownership =
    Recording_s3.Bucket.Ownership_controls.get conn ~bucket:"my-bucket"
    |> ok_or_fail "ownership"
  in
  Alcotest.(check string)
    "ownership" "BucketOwnerEnforced"
    (Bucket.Ownership_controls.Object_ownership.to_string
       ownership.config.object_ownership);
  let request_payment =
    Recording_s3.Bucket.Request_payment.get conn ~bucket:"my-bucket"
    |> ok_or_fail "request payment"
  in
  Alcotest.(check bool)
    "requester payer" true
    (request_payment.payer = Some Bucket.Request_payment.Payer.Requester);
  let accelerate =
    Recording_s3.Bucket.Accelerate.get conn ~bucket:"my-bucket"
    |> ok_or_fail "accelerate"
  in
  Alcotest.(check bool)
    "accelerate enabled" true
    (accelerate.status = Some Bucket.Accelerate.Status.Enabled);
  let policy_status =
    Recording_s3.Bucket.Policy_status.get conn ~bucket:"my-bucket"
    |> ok_or_fail "policy status"
  in
  Alcotest.(check (option bool))
    "policy status" (Some false) policy_status.is_public;
  let logging =
    Recording_s3.Bucket.Logging.get conn ~bucket:"my-bucket"
    |> ok_or_fail "logging"
  in
  match logging.config.logging with
  | Some target ->
      Alcotest.(check string) "logging target" "log-bucket" target.target_bucket;
      Alcotest.(check string) "logging prefix" "logs/" target.target_prefix
  | None -> Alcotest.fail "expected logging target"

let test_object_checksum_headers_and_response () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [
              ("etag", "\"etag\""); ("x-amz-checksum-sha256", "provided-sha256");
            ]
          "";
      ]
  in
  let checksum : Object.Checksum.request =
    { Object.Checksum.algorithm = `SHA256; value = Some "provided-sha256" }
  in
  let options = { Object.Put.default_options with checksum = Some checksum } in
  let put =
    Recording_s3.Object.Buffer.put_string conn ~bucket:"my-bucket" ~key:"file"
      ~options "hello"
    |> ok_or_fail "put checksum"
  in
  check_checksum "put response checksum" `SHA256 "provided-sha256" put.checksum;
  let call = Recording_runtime.last_call conn in
  Alcotest.(check string) "body" "hello" call.body;
  Alcotest.(check (option string))
    "checksum algorithm header" (Some "SHA256")
    (header "x-amz-checksum-algorithm" call.request.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha256")
    (header "x-amz-checksum-sha256" call.request.headers)

let test_object_precondition_headers () =
  let time = Ptime.to_rfc3339 test_time in
  let conn =
    Recording_runtime.connect
      [
        response 200 ~headers:[ ("etag", "\"put\"") ] "";
        response 200
          ~headers:[ ("etag", "\"get\""); ("content-length", "0") ]
          "";
        response 200
          ~headers:[ ("etag", "\"head\""); ("content-length", "0") ]
          "";
        response 204 "";
        response 200
          {|<CopyObjectResult><ETag>"copy"</ETag></CopyObjectResult>|};
      ]
  in
  let etag = Object.Etag.of_string_exn "\"etag\"" in
  let write_preconditions =
    {
      Object.Preconditions.Write.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
    }
  in
  let put_options =
    { Object.Put.default_options with preconditions = write_preconditions }
  in
  ignore
    (Recording_s3.Object.Buffer.put_string conn ~bucket:"my-bucket" ~key:"file"
       ~options:put_options "body"
    |> ok_or_fail "put preconditions");
  let read_preconditions =
    {
      Object.Preconditions.Read.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
      if_modified_since = Some test_time;
      if_unmodified_since = Some test_time;
    }
  in
  let get_options =
    { Object.Get.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.Buffer.get_string conn ~bucket:"my-bucket" ~key:"file"
       ~options:get_options ~max_size:16L ()
    |> ok_or_fail "get preconditions");
  let head_options =
    { Object.Head.default_options with preconditions = read_preconditions }
  in
  ignore
    (Recording_s3.Object.head conn ~bucket:"my-bucket" ~key:"file"
       ~options:head_options ()
    |> ok_or_fail "head preconditions");
  let delete_preconditions =
    {
      Object.Preconditions.Delete.if_match =
        Some (Object.Etag_condition.etag etag);
      if_match_last_modified_time = Some test_time;
      if_match_size = Some 4L;
    }
  in
  let delete_options =
    { Object.Delete.default_options with preconditions = delete_preconditions }
  in
  ignore
    (Recording_s3.Object.delete conn ~bucket:"my-bucket" ~key:"file"
       ~options:delete_options ()
    |> ok_or_fail "delete preconditions");
  let copy_preconditions =
    {
      Object.Preconditions.Copy_source.if_match =
        Some (Object.Etag_condition.etag etag);
      if_none_match = Some Object.Etag_condition.any;
      if_modified_since = Some test_time;
      if_unmodified_since = Some test_time;
    }
  in
  let copy_options =
    {
      Object.Copy.default_options with
      source_preconditions = copy_preconditions;
    }
  in
  ignore
    (Recording_s3.Object.copy conn ~src_bucket:"my-bucket" ~src_key:"file"
       ~dst_bucket:"my-bucket" ~dst_key:"copy" ~options:copy_options ()
    |> ok_or_fail "copy preconditions");
  match List.rev conn.calls with
  | [ put; get; head; delete; copy ] ->
      Alcotest.(check (option string))
        "put if-match" (Some "\"etag\"")
        (header "if-match" put.request.headers);
      Alcotest.(check (option string))
        "put if-none-match" (Some "*")
        (header "if-none-match" put.request.headers);
      List.iter
        (fun (call : Recording_runtime.call) ->
          Alcotest.(check (option string))
            "read if-match" (Some "\"etag\"")
            (header "if-match" call.request.headers);
          Alcotest.(check (option string))
            "read if-none-match" (Some "*")
            (header "if-none-match" call.request.headers);
          Alcotest.(check (option string))
            "read if-modified-since" (Some time)
            (header "if-modified-since" call.request.headers);
          Alcotest.(check (option string))
            "read if-unmodified-since" (Some time)
            (header "if-unmodified-since" call.request.headers))
        [ get; head ];
      Alcotest.(check (option string))
        "delete if-match" (Some "\"etag\"")
        (header "if-match" delete.request.headers);
      Alcotest.(check (option string))
        "delete last modified" (Some time)
        (header "x-amz-if-match-last-modified-time" delete.request.headers);
      Alcotest.(check (option string))
        "delete size" (Some "4")
        (header "x-amz-if-match-size" delete.request.headers);
      Alcotest.(check (option string))
        "copy source if-match" (Some "\"etag\"")
        (header "x-amz-copy-source-if-match" copy.request.headers);
      Alcotest.(check (option string))
        "copy source if-none-match" (Some "*")
        (header "x-amz-copy-source-if-none-match" copy.request.headers);
      Alcotest.(check (option string))
        "copy source modified since" (Some time)
        (header "x-amz-copy-source-if-modified-since" copy.request.headers);
      Alcotest.(check (option string))
        "copy source unmodified since" (Some time)
        (header "x-amz-copy-source-if-unmodified-since" copy.request.headers)
  | _ -> Alcotest.fail "expected five recorded calls"

let test_retry_slow_down_then_success () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 503 slow_down; response 200 "" ]
  in
  let result =
    Recording_s3.Object.Buffer.put_string conn ~bucket:"my-bucket" ~key:"file"
      "body"
  in
  ignore (ok_or_fail "retry put" result);
  Alcotest.(check int) "attempts" 2 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 1 (List.length conn.sleeps)

let test_retry_fatal_error_not_retried () =
  let denied =
    {|<Error><Code>AccessDenied</Code><Message>denied</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect [ response 403 denied; response 200 "" ]
  in
  (match
     Recording_s3.Object.Buffer.put_string conn ~bucket:"my-bucket" ~key:"file"
       "body"
   with
  | Error error when Error.service_code error = Some "AccessDenied" -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected access denied");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

let test_retry_disabled_policy () =
  let slow_down =
    {|<Error><Code>SlowDown</Code><Message>reduce request rate</Message></Error>|}
  in
  let conn =
    Recording_runtime.connect ~retry_policy:Awskit.Retry.disabled
      [ response 503 slow_down; response 200 "" ]
  in
  (match
     Recording_s3.Object.Buffer.put_string conn ~bucket:"my-bucket" ~key:"file"
       "body"
   with
  | Error error when Error.service_code error = Some "SlowDown" -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected slow down");
  Alcotest.(check int) "attempts" 1 (List.length conn.calls);
  Alcotest.(check int) "sleeps" 0 (List.length conn.sleeps)

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

let test_object_paginator_follows_tokens () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_page ~next_continuation_token:"token-1" ~truncated:true
             [ "a.txt" ]);
        response 200
          (list_page ~continuation_token:"token-1" ~truncated:false [ "b.txt" ]);
      ]
  in
  let options = { Object.List.default_options with max_keys = Some 1 } in
  let keys =
    Recording_s3.Object.Paginator.keys conn ~bucket:"my-bucket" ~options ()
    |> ok_or_fail "paginator keys"
  in
  Alcotest.(check (list string)) "keys" [ "a.txt"; "b.txt" ] keys;
  let calls = List.rev conn.calls in
  Alcotest.(check int) "calls" 2 (List.length calls);
  match calls with
  | [ _first; second ] ->
      Alcotest.(check (option (list string)))
        "continuation token" (Some [ "token-1" ])
        (List.assoc_opt "continuation-token" second.request.target.query)
  | _ -> Alcotest.fail "expected two calls"

let test_object_paginator_max_pages () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_page ~next_continuation_token:"token-1" ~truncated:true
             [ "a.txt" ]);
        response 200 (list_page ~truncated:false [ "b.txt" ]);
      ]
  in
  let pages =
    Recording_s3.Object.Paginator.pages conn ~bucket:"my-bucket" ~max_pages:1 ()
    |> ok_or_fail "paginator pages"
  in
  Alcotest.(check int) "page count" 1 (List.length pages);
  Alcotest.(check int) "calls" 1 (List.length conn.calls)

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

let test_multipart_paginator_follows_markers () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          (list_parts_page ~next_part_number_marker:1 ~truncated:true [ 1 ]);
        response 200 (list_parts_page ~truncated:false [ 2 ]);
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let parts =
    Recording_s3.Multipart.Paginator.parts conn ~bucket:"my-bucket"
      ~key:"large.bin" ~upload_id ()
    |> ok_or_fail "multipart paginator parts"
  in
  Alcotest.(check (list int))
    "parts" [ 1; 2 ]
    (List.map
       (fun (part : Multipart.List_parts.part_info) -> part.part_number)
       parts);
  let calls = List.rev conn.calls in
  Alcotest.(check int) "calls" 2 (List.length calls);
  match calls with
  | [ _first; second ] ->
      Alcotest.(check (option (list string)))
        "part marker" (Some [ "1" ])
        (List.assoc_opt "part-number-marker" second.request.target.query)
  | _ -> Alcotest.fail "expected two calls"

let test_multipart_upload_part_checksum_headers () =
  let conn =
    Recording_runtime.connect
      [
        response 200
          ~headers:
            [ ("etag", "\"etag-1\""); ("x-amz-checksum-sha1", "provided-sha1") ]
          "";
      ]
  in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let checksum : Object.Checksum.request =
    { Object.Checksum.algorithm = `SHA1; value = Some "provided-sha1" }
  in
  let options = { Multipart.Upload_part.checksum = Some checksum } in
  let part =
    Recording_s3.Multipart.upload_part conn ~bucket:"my-bucket" ~key:"large.bin"
      ~upload_id ~part_number:1 ~body:"hello" ~options ()
    |> ok_or_fail "upload part checksum"
  in
  check_checksum "part response checksum" `SHA1 "provided-sha1" part.checksum;
  let call = Recording_runtime.last_call conn in
  Alcotest.(check (option string))
    "checksum algorithm header" (Some "SHA1")
    (header "x-amz-checksum-algorithm" call.request.headers);
  Alcotest.(check (option string))
    "checksum value header" (Some "provided-sha1")
    (header "x-amz-checksum-sha1" call.request.headers);
  Alcotest.(check (option (list string)))
    "part number" (Some [ "1" ])
    (List.assoc_opt "partNumber" call.request.target.query)

let make_sim () =
  let clock = Sim.Clock.create ~now:test_time () in
  let store = Sim.create_store ~clock () in
  let conn = Sim.connect store ~credentials:creds in
  ignore (Sim.Bucket.create conn ~bucket:"test-bucket" () |> ok_or_fail "bucket");
  conn

let test_sim_buffer_roundtrip () =
  let conn = make_sim () in
  let checksum : Object.Checksum.request =
    { Object.Checksum.algorithm = `SHA256; value = None }
  in
  let put =
    Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~options:
        {
          Object.Put.default_options with
          content_type = Some "text/plain";
          checksum = Some checksum;
        }
      "hello"
    |> ok_or_fail "put"
  in
  Alcotest.(check bool) "etag" true (Option.is_some put.etag);
  check_checksum "put checksum" `SHA256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" put.checksum;
  let info, body =
    Sim.Object.Buffer.get_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~max_size:16L ()
    |> ok_or_fail "get"
  in
  Alcotest.(check string) "body" "hello" body;
  Alcotest.(check (option string))
    "content-type" (Some "text/plain") info.content_type;
  check_checksum "get checksum" `SHA256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" info.checksum;
  let head =
    Sim.Object.head conn ~bucket:"test-bucket" ~key:"hello.txt" ()
    |> ok_or_fail "head checksum"
  in
  check_checksum "head checksum" `SHA256
    "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=" head.checksum;
  let page =
    Sim.Object.list conn ~bucket:"test-bucket" () |> ok_or_fail "list checksum"
  in
  match page.objects with
  | [ object_ ] ->
      Alcotest.(check (option string))
        "listed checksum" (Some "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=")
        (List.find_map
           (fun (checksum : Object.Checksum.response) -> Some checksum.value)
           object_.checksums)
  | _ -> Alcotest.fail "expected one listed object"

let test_sim_streaming_get () =
  let conn = make_sim () in
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"stream"
       "abcdef"
    |> ok_or_fail "put");
  let consume reader =
    let bytes = Bytes.create 3 in
    match Sim.Runtime.read reader bytes ~off:0 ~len:3 with
    | Error _ as error -> error
    | Ok read -> Ok (Bytes.sub_string bytes 0 read)
  in
  let _info, body =
    Sim.Object.get conn ~bucket:"test-bucket" ~key:"stream" ~consume ()
    |> ok_or_fail "stream get"
  in
  Alcotest.(check string) "partial body" "abc" body

let test_buffer_limit () =
  let conn = make_sim () in
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"large"
       "abcdef"
    |> ok_or_fail "put");
  match
    Sim.Object.Buffer.get_string conn ~bucket:"test-bucket" ~key:"large"
      ~max_size:3L ()
  with
  | Error (Awskit.Error.Body _) -> ()
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected max_size failure"

let test_sim_paginator_keys () =
  let conn = make_sim () in
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"logs/a.txt"
       "a"
    |> ok_or_fail "put a");
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"logs/b.txt"
       "b"
    |> ok_or_fail "put b");
  ignore
    (Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"other.txt"
       "other"
    |> ok_or_fail "put other");
  let options =
    {
      Object.List.default_options with
      prefix = Some "logs/";
      max_keys = Some 1;
    }
  in
  let keys =
    Sim.Object.Paginator.keys conn ~bucket:"test-bucket" ~options ()
    |> ok_or_fail "sim paginator keys"
  in
  Alcotest.(check (list string)) "keys" [ "logs/a.txt"; "logs/b.txt" ] keys

let suite =
  [
    ( "core",
      [
        Alcotest.test_case "presigned result" `Quick test_presigned_result;
        Alcotest.test_case "presigned put checksum headers" `Quick
          test_presigned_put_checksum_headers;
        Alcotest.test_case "provider resolution" `Quick test_provider_resolution;
        Alcotest.test_case "provider variants" `Quick test_provider_variants;
        Alcotest.test_case "bucket head request" `Quick test_bucket_head_request;
        Alcotest.test_case "bucket list parse" `Quick test_bucket_list_parse;
        Alcotest.test_case "bucket config parse" `Quick test_bucket_config_parse;
        Alcotest.test_case "object checksum headers and response" `Quick
          test_object_checksum_headers_and_response;
        Alcotest.test_case "object precondition headers" `Quick
          test_object_precondition_headers;
        Alcotest.test_case "retry slow down then success" `Quick
          test_retry_slow_down_then_success;
        Alcotest.test_case "retry fatal error not retried" `Quick
          test_retry_fatal_error_not_retried;
        Alcotest.test_case "retry disabled policy" `Quick
          test_retry_disabled_policy;
        Alcotest.test_case "object paginator follows tokens" `Quick
          test_object_paginator_follows_tokens;
        Alcotest.test_case "object paginator max pages" `Quick
          test_object_paginator_max_pages;
        Alcotest.test_case "multipart paginator follows markers" `Quick
          test_multipart_paginator_follows_markers;
        Alcotest.test_case "multipart upload part checksum headers" `Quick
          test_multipart_upload_part_checksum_headers;
        Alcotest.test_case "sim buffer roundtrip" `Quick
          test_sim_buffer_roundtrip;
        Alcotest.test_case "sim streaming get" `Quick test_sim_streaming_get;
        Alcotest.test_case "buffer limit" `Quick test_buffer_limit;
        Alcotest.test_case "sim paginator keys" `Quick test_sim_paginator_keys;
      ] );
  ]
