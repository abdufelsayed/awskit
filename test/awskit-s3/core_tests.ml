open Awskit_s3

let test_time = Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.get

let creds =
  Credentials.create_exn ~access_key_id:"AKID" ~secret_access_key:"SECRET" ()

let error_t = Alcotest.testable Error.pp Error.equal

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Error.pp error

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
    mutable calls : call list;
    mutable responses : response list;
  }

  type 'a t = 'a

  let return value = value
  let bind value f = f value

  type upload_body = string
  type download_body = string
  type upload_writer = Buffer.t
  type download_reader = { body : string; mutable offset : int }

  let connect ?(provider = Provider.default)
      ?(region = Region.of_string_exn "us-east-1") responses =
    { region; credentials = creds; provider; calls = []; responses }

  let last_call conn =
    match conn.calls with
    | call :: _ -> call
    | [] -> Alcotest.fail "expected recorded request"

  let now _ = test_time
  let region conn = conn.region
  let credentials conn = Ok conn.credentials
  let s3_provider conn = conn.provider

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

  let with_download_body body ~consume = consume { body; offset = 0 }

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

let make_sim () =
  let clock = Sim.Clock.create ~now:test_time () in
  let store = Sim.create_store ~clock () in
  let conn = Sim.connect store ~credentials:creds in
  ignore (Sim.Bucket.create conn ~bucket:"test-bucket" () |> ok_or_fail "bucket");
  conn

let test_sim_buffer_roundtrip () =
  let conn = make_sim () in
  let put =
    Sim.Object.Buffer.put_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~options:
        { Object.Put.default_options with content_type = Some "text/plain" }
      "hello"
    |> ok_or_fail "put"
  in
  Alcotest.(check bool) "etag" true (Option.is_some put.etag);
  let info, body =
    Sim.Object.Buffer.get_string conn ~bucket:"test-bucket" ~key:"hello.txt"
      ~max_size:16L ()
    |> ok_or_fail "get"
  in
  Alcotest.(check string) "body" "hello" body;
  Alcotest.(check (option string))
    "content-type" (Some "text/plain") info.content_type

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

let suite =
  [
    ( "core",
      [
        Alcotest.test_case "presigned result" `Quick test_presigned_result;
        Alcotest.test_case "provider resolution" `Quick test_provider_resolution;
        Alcotest.test_case "provider variants" `Quick test_provider_variants;
        Alcotest.test_case "bucket head request" `Quick test_bucket_head_request;
        Alcotest.test_case "bucket list parse" `Quick test_bucket_list_parse;
        Alcotest.test_case "bucket config parse" `Quick test_bucket_config_parse;
        Alcotest.test_case "sim buffer roundtrip" `Quick
          test_sim_buffer_roundtrip;
        Alcotest.test_case "sim streaming get" `Quick test_sim_streaming_get;
        Alcotest.test_case "buffer limit" `Quick test_buffer_limit;
      ] );
  ]
