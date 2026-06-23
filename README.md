# Awskit

Awskit is a runtime-agnostic AWS toolkit for OCaml.

It gives OCaml applications the building blocks needed to talk to AWS:
credentials, regions, endpoints, SigV4 signing, retry handling, request and
response metadata, runtime adapters, and an S3 client. The core packages stay
independent of Unix, Eio, Lwt, and Cohttp; HTTP execution lives in adapter
packages that match the runtime used by your application.

Awskit currently focuses on AWS S3.

## What You Get

- Runtime-agnostic AWS core types for credentials, regions, endpoints, signing,
  errors, retries, and HTTP metadata.
- Ready-to-use adapters for Eio and Lwt applications.
- S3 bucket, object, multipart upload, policy, tagging, versioning, endpoint,
  and presigned request artifact support.
- A deterministic in-memory S3 simulator for fast tests.
- Streaming request and response bodies with explicit size, hash, and
  replayability metadata.
- Unix helpers for AWS environment variables, shared credentials, and shared
  config files.

## Installation

Awskit is split into small packages so applications only depend on the runtime
they actually use. From a source checkout, install dependencies and build with
Dune:

```sh
opam install . --deps-only --with-test
opam exec -- dune build
```

When installing released packages from opam, choose the S3 adapter that matches
your runtime.

```sh
opam install awskit-s3-eio
```

or:

```sh
opam install awskit-s3-lwt-unix
```

Use `awskit-s3-eio` when your application already runs on Eio and can provide
an HTTPS connector. Use `awskit-s3-lwt-unix` when you want a ready-to-use Lwt
client that can read the standard AWS environment and profile configuration.

## Packages

| Package | Description |
| --- | --- |
| `awskit` | Runtime-agnostic AWS core: credentials, regions, endpoints, SigV4 signing, retries, request/response types, errors, and the runtime module type. |
| `awskit-unix` | Unix helpers for clocks, environment variables, shared AWS credentials, and config files. |
| `awskit-lwt` | Generic Lwt runtime adapter over a caller-supplied Cohttp Lwt client. |
| `awskit-lwt-unix` | Ready-to-use Lwt + Unix runtime adapter using Cohttp Lwt Unix. |
| `awskit-eio` | Direct-style Eio runtime adapter using Cohttp Eio and a caller-provided HTTPS policy. |
| `awskit-s3` | Runtime-agnostic AWS S3 core: buckets, objects, multipart upload, presigned request artifacts, policies, and endpoint resolution. |
| `awskit-s3-sim` | Deterministic in-memory S3 implementation for tests. |
| `awskit-s3-lwt` | S3 adapter over the generic Awskit Lwt runtime. |
| `awskit-s3-lwt-unix` | Ready-to-use S3 client for Lwt + Unix applications. |
| `awskit-s3-eio` | Direct-style S3 client for Eio applications using a caller-provided HTTPS policy. |

The `awskit` and `awskit-s3` packages do not depend on Unix, Eio, Lwt, or
Cohttp runtime packages. Use `awskit-s3-sim` in tests when you want deterministic
S3 behavior without HTTP requests. Adapter packages carry runtime dependencies.

## Error Handling

Awskit APIs return errors as values. Runtime-backed operations use the shape
`('a, Awskit.Error.t) result io`, where `io` is supplied by the selected runtime
adapter. Pure constructors return `('a, Awskit.Error.t) result`.

`Awskit.Error.t` carries structured AWS context such as the operation, resource,
HTTP status, AWS error code, request id, retry class, and retry attempts. Use
`Awskit.Error.pp` or `Awskit.Error.to_string_hum` for human-readable logs, and
`Awskit.Error.sexp_of_t` for structured diagnostics and tests.

Application code should inspect, classify, and print errors returned by Awskit
operations. Constructing `Awskit.Error.t` values is reserved for Awskit
implementations under `Awskit.Error.Internal`.

Functions ending in `_exn` raise `Awskit.Error.Awskit_error` on SDK validation
or construction failure. Prefer the result-returning form in libraries,
services, and other long-running code. Cancellation and user callback
exceptions are not converted into SDK errors.

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
      let bucket = Awskit_s3.Bucket_name.of_string_exn "my-bucket" in
      let key = Awskit_s3.Object_key.of_string_exn "hello.txt" in
      match
        Awskit_s3_eio.Object.put_string s3
          ~bucket
          ~key
          ~contents:"Hello, S3!"
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
      let bucket = Awskit_s3.Bucket_name.of_string_exn "my-bucket" in
      let key = Awskit_s3.Object_key.of_string_exn "hello.txt" in
      let* result =
        Awskit_s3_lwt_unix.Object.get_string s3
          ~bucket
          ~key
          ~max_bytes:1_048_576L
          ()
      in
      match result with
      | Ok result -> Lwt_io.printl result.value
      | Error error -> Lwt_io.eprintf "S3 error: %a\n" Awskit_s3.Error.pp error
```

When arguments are omitted, the Unix adapter reads the standard AWS
configuration sources:

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

Use `Awskit_s3.Endpoint_config` when testing against a local service or another
S3-compatible endpoint. Plain HTTP is explicit: loopback endpoints use
`local_plaintext`, and non-local HTTP requires `unsafe_plaintext`.

## S3

`awskit-s3` exposes AWS S3 operations for:

- bucket creation, deletion, listing, and configuration;
- object put, get, head, delete, copy, ranges, metadata, tags, and versions;
- multipart upload and local-file transfer helpers;
- presigned request artifacts;
- bucket policies and related XML/JSON wire types;
- S3 endpoint and addressing configuration;
- structured S3 error classifiers.

Awskit targets AWS S3 semantics. S3-compatible services such as MinIO are useful
for local contract testing, but application behavior should be written against
AWS S3 semantics unless a provider-specific difference is intentional.

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
let bucket = Awskit_s3.Bucket_name.of_string_exn "test" in
let key = Awskit_s3.Object_key.of_string_exn "hello" in

Awskit_s3_sim.Bucket.create conn ~bucket () |> ignore;
Awskit_s3_sim.Object.put_string conn
  ~bucket
  ~key
  ~contents:"world"
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
and falls back to path-style otherwise. Local test services commonly need an
explicit endpoint policy:

```ocaml
let endpoint_config =
  Awskit_s3.Endpoint_config.local_plaintext
    ~endpoint:(Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9000 ())
    ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
    ~addressing_style:`Path
    ()
  |> Result.get_ok
```

## Streaming

Runtime request bodies carry a descriptor with `content_length`,
`payload_hash`, and `replayable` metadata. S3 `PutObject` and `UploadPart`
currently require a known `content_length`; Awskit does not implement
unknown-length SigV4 aws-chunked streaming.

For already-buffered data, prefer string or bytes body helpers. Custom stream
bodies must emit exactly the declared number of bytes, and producer callback
errors are reported as request body failures. Mark custom streams replayable
only when the stream can actually be recreated from the beginning for a retry.

Response bodies are streaming and scoped to the runtime response callback.
Runtime adapters expose `with_response`; inside that callback, consume bodies
through `Response_body.with_reader` or S3 helper APIs such as
`Object.get ~consume:(Reader.to_string ~max_bytes)`.

For custom streaming uploads, build a `Body` with an exact length and an honest
replayability flag, then pass it to the same `Object.put` operation:

```ocaml
module S3 = Awskit_s3_eio

let upload_chunk s3 ~bucket ~key ~content_length chunk =
  match
    S3.Body.of_stream ~content_length ~replayable:false
      ~write:(fun writer -> S3.Body.Writer.write_string writer chunk)
  with
  | Error error -> Error error
  | Ok body -> S3.Object.put s3 ~bucket ~key ~body ()
```

For large downloads, keep processing inside the scoped `consume` callback:

```ocaml
module S3 = Awskit_s3_eio

let process chunk =
  output_string stdout (Bytes.to_string chunk);
  Ok ()

let download_stream s3 ~bucket ~key =
  S3.Object.get s3 ~bucket ~key
    ~consume:(S3.Reader.iter ~f:process)
    ()
```

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
AWSKIT_S3_MINIO_UNSAFE_HTTP
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
