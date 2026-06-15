# Awskit

Awskit is AWS infrastructure for OCaml.

It provides the pieces needed to build AWS clients in OCaml: credentials,
regions, endpoints, SigV4 signing, retry handling, request and response types,
runtime adapters, and S3 support. The core packages are pure OCaml; concrete
HTTP execution lives in runtime-specific adapter packages for Eio and Lwt.

Awskit currently focuses on AWS S3.

## Features

- Pure AWS core types, signing, endpoints, credentials, errors, and retries.
- Runtime adapters for Eio and Lwt applications.
- S3 bucket, object, multipart upload, policy, tagging, versioning, and
  presigned URL support.
- Deterministic in-memory S3 simulation for tests.
- Streaming request and response bodies with explicit replayability metadata.
- Unix helpers for standard AWS environment variables, shared credentials, and
  config files.

## Installation

Awskit is split into small packages. From a source checkout, install
dependencies and build with Dune:

```sh
opam install . --deps-only --with-test
opam exec -- dune build
```

When installing released packages from opam, install the adapter that matches
your runtime:

```sh
opam install awskit-s3-eio
```

or:

```sh
opam install awskit-s3-lwt-unix
```

## Packages

| Package | Description |
| --- | --- |
| `awskit` | Pure AWS core: credentials, regions, endpoints, SigV4 signing, retries, request/response types, errors, and the runtime module type. |
| `awskit-unix` | Unix helpers for clocks, environment variables, shared AWS credentials, and config files. |
| `awskit-lwt` | Generic Lwt runtime adapter over a caller-supplied Cohttp Lwt client. |
| `awskit-lwt-unix` | Ready-to-use Lwt + Unix runtime adapter using Cohttp Lwt Unix. |
| `awskit-eio` | Direct-style Eio runtime adapter using Cohttp Eio and a caller-provided HTTPS policy. |
| `awskit-s3` | Pure AWS S3 core: buckets, objects, multipart upload, presigned URLs, policies, and endpoint resolution. |
| `awskit-s3-sim` | Deterministic in-memory S3 implementation for tests. |
| `awskit-s3-lwt` | S3 adapter over the generic Awskit Lwt runtime. |
| `awskit-s3-lwt-unix` | Ready-to-use S3 client for Lwt + Unix applications. |
| `awskit-s3-eio` | Direct-style S3 client for Eio applications using a caller-provided HTTPS policy. |

The `awskit` and `awskit-s3` packages do not depend on Unix, Eio, Lwt, or
Cohttp runtime packages. `awskit-s3-sim` is intended for test code and does not
perform HTTP requests. Adapter packages carry runtime dependencies.

## Error Handling

Awskit APIs return explicit results. Runtime-backed operations use the shape
`('a, Awskit.Error.t) result io`, where `io` is supplied by the selected runtime
adapter. Pure constructors return `('a, Awskit.Error.t) result`.

`Awskit.Error.t` carries structured AWS context such as operation, resource,
HTTP status, AWS error code, request id, retry class, and retry attempts. Use
`Awskit.Error.pp` or `Awskit.Error.to_string_hum` for human logs, and
`Awskit.Error.sexp_of_t` for structured diagnostics and tests.

Application code should treat `Awskit.Error` as a consumer API: inspect,
classify, and print errors returned by Awskit operations. Error construction
is reserved for Awskit package implementations under `Awskit.Error.Internal`.

Functions ending in `_exn` raise `Awskit.Error.Awskit_error` on SDK validation
or construction failure. Prefer the result-returning form in libraries and
long-running services. Cancellation and user callback exceptions are not
converted into SDK errors.

## Quick Start

### Eio

Install the Eio S3 adapter:

```sh
opam install awskit-s3-eio eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng
```

Add the libraries to your Dune file:

```lisp
; dune
(libraries
 awskit
 awskit-s3
 awskit-s3-eio
 eio_main
 fmt
 tls-eio
 tls
 ca-certs
 domain-name
 mirage-crypto-rng.unix)
```

Upload an object:

```ocaml
open Eio.Std

module Https = struct
  let connector () =
    Mirage_crypto_rng_unix.use_default ();
    let authenticator =
      match Ca_certs.authenticator () with
      | Ok authenticator -> authenticator
      | Error (`Msg msg) -> invalid_arg ("failed to load CA roots: " ^ msg)
    in
    let tls_config =
      match Tls.Config.client ~authenticator () with
      | Ok config -> config
      | Error (`Msg msg) -> invalid_arg ("failed to create TLS config: " ^ msg)
    in
    Some
      (fun uri raw ->
        let host =
          Uri.host uri
          |> Option.map (fun host ->
                 Domain_name.(host_exn (of_string_exn host)))
        in
        Tls_eio.client_of_flow ?host tls_config raw)
end

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let credentials =
    Awskit.Credentials.create_exn
      ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
      ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in
  let https = Https.connector () in
  match
    Awskit_s3_eio.create ~sw ~env ~https ~region:"us-east-1" ~credentials ()
  with
  | Error error -> Fmt.epr "S3 error: %a@." Awskit_s3.Error.pp error
  | Ok s3 -> (
      match
        Awskit_s3_eio.Object.put s3
          ~bucket:"my-bucket"
          ~key:"hello.txt"
          ~body:(Awskit_s3_eio.Body.of_string "Hello, S3!")
          ()
      with
      | Ok uploaded ->
          Fmt.pr "Uploaded. ETag: %a@."
            (Fmt.option Awskit_s3.Object.Etag.pp)
            uploaded.etag
      | Error error -> Fmt.epr "S3 error: %a@." Awskit_s3.Error.pp error)
```

### Lwt + Unix

Install the Lwt Unix S3 adapter:

```sh
opam install awskit-s3-lwt-unix
```

Add the libraries to your Dune file:

```lisp
; dune
(libraries awskit awskit-s3 awskit-s3-lwt-unix lwt.unix)
```

Download an object:

```ocaml
open Lwt.Syntax

let run () =
  match Awskit_s3_lwt_unix.create () with
  | Error error -> Lwt_io.eprintf "S3 error: %a\n" Awskit_s3.Error.pp error
  | Ok s3 ->
      let* result =
        Awskit_s3_lwt_unix.Object.get s3
          ~bucket:"my-bucket"
          ~key:"hello.txt"
          ~consume:(Awskit_s3_lwt_unix.Reader.to_string ~max_bytes:1_048_576L)
          ()
      in
      match result with
      | Ok (_info, body) -> Lwt_io.printl body
      | Error error -> Lwt_io.eprintf "S3 error: %a\n" Awskit_s3.Error.pp error
```

When arguments are omitted, the Unix adapter reads standard AWS configuration
sources:

```text
AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY
AWS_SESSION_TOKEN
AWS_REGION
AWS_DEFAULT_REGION
AWS_PROFILE
AWS_SHARED_CREDENTIALS_FILE
AWS_CONFIG_FILE
AWS_CONTAINER_CREDENTIALS_RELATIVE_URI
AWS_CONTAINER_CREDENTIALS_FULL_URI
AWS_CONTAINER_AUTHORIZATION_TOKEN
AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE
```

Pass an explicit `endpoint` when testing against a local service or custom AWS
endpoint.

## S3

`awskit-s3` exposes AWS S3 operations for:

- bucket creation, deletion, listing, and configuration;
- object put, get, head, delete, copy, ranges, metadata, tags, and versions;
- multipart upload and local-file transfer helpers;
- presigned URLs;
- bucket policies and related XML/JSON wire types;
- S3 endpoint and addressing configuration;
- structured S3 error classifiers.

Awskit targets AWS S3 semantics. S3-compatible services such as MinIO are useful
for local contract testing, but provider-specific behavior should stay in tests
unless it matches AWS S3.

Optional lookup helpers convert object-not-found responses to `Ok None` while
leaving other failures structured. S3 can return status-only `HeadObject` 404
responses, so `find_metadata` treats a code-less 404 as an absent object; coded
`NoSuchBucket` responses remain `Error`.

```ocaml
module S3 = Awskit_s3_eio

match S3.Object.find_metadata s3 ~bucket ~key () with
| Ok (Some info) ->
    Fmt.pr "content length: %s@."
      (Option.fold ~none:"unknown" ~some:Int64.to_string info.content_length)
| Ok None -> Fmt.pr "object is absent@."
| Error error when Awskit_s3.Error.is_no_such_bucket error ->
    Fmt.epr "bucket is absent: %a@." Awskit.Error.pp error
| Error error ->
    Fmt.epr "S3 request failed: %a@." Awskit.Error.pp error
```

## S3 Simulation

Use `awskit-s3-sim` for deterministic in-memory S3 tests:

```ocaml
let credentials =
  Awskit.Credentials.create_exn
    ~access_key_id:"AK"
    ~secret_access_key:"SK"
    ()
in
let clock = Awskit_s3_sim.Clock.create () in
let store = Awskit_s3_sim.create_store ~clock () in
let conn = Awskit_s3_sim.connect store ~credentials in

Awskit_s3_sim.Bucket.create conn ~bucket:"test" () |> ignore;
Awskit_s3_sim.Object.put conn
  ~bucket:"test"
  ~key:"hello"
  ~body:(Awskit_s3_sim.Body.of_string "world")
  ()
|> ignore
```

## Addressing Style

S3 supports virtual-hosted and path-style bucket addressing. Awskit exposes
this as:

```ocaml
type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
```

`Auto` uses virtual-hosted addressing when the bucket and endpoint support it,
and falls back to path-style otherwise. Local test services commonly need
`~addressing_style:`Path`.

## Streaming

Runtime request bodies carry a descriptor with `content_length`,
`payload_hash`, and `replayable` metadata. S3 `PutObject` and `UploadPart`
currently require a known `content_length`; Awskit does not implement
unknown-length SigV4 aws-chunked streaming.

For already-buffered data, prefer string or bytes body helpers. Custom stream
bodies must emit exactly the declared number of bytes, and producer callback
errors are reported as request body failures. Mark custom streams replayable
only when the stream can actually be replayed for a retry.

Response bodies are streaming and scoped to the runtime response callback.
Runtime adapters expose `with_response`; inside that callback, consume bodies
through `Response_body.with_reader` or S3 helper APIs such as
`Object.get ~consume:(Reader.to_string ~max_bytes)`.

## Development

Install dependencies and run the build and test suite:

```sh
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune test
```

Optional MinIO contract tests:

```sh
docker compose up -d
opam exec -- dune build @minio-contract
```

The MinIO contract runner defaults to `http://127.0.0.1:9000` with the
`minioadmin` credentials from `docker-compose.yml`. Override with:

```text
AWSKIT_S3_MINIO_ENDPOINT
AWSKIT_S3_MINIO_ACCESS_KEY_ID
AWSKIT_S3_MINIO_SECRET_ACCESS_KEY
AWSKIT_S3_MINIO_REGION
```

## Contributing

Issues and pull requests are welcome. For code changes, please run:

```sh
opam exec -- dune build
opam exec -- dune test
```

Keep changes focused, include tests for behavior changes, and prefer the
existing package split when adding new runtime-specific functionality.

## License

MIT. See [LICENSE](LICENSE).
