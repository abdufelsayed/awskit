# awskit

AWS infrastructure for OCaml, focused today on an AWS S3 SDK: pure core,
runtime adapters, deterministic simulation, and local MinIO contract tests.

## Packages

| Package | Description |
|---------|-------------|
| **awskit** | Pure AWS infrastructure — SigV4 signing, credentials, regions, endpoints, error types, HTTP request/response types, `Runtime` module type. Optional `awskit-eio`, `awskit-lwt`, `awskit-lwt-unix`, `awskit-unix` adapters. |
| **awskit-s3** | AWS S3 client core — objects, buckets, multipart uploads, presigned URLs, policies, simulation. Optional `awskit-s3-eio`, `awskit-s3-lwt`, and `awskit-s3-lwt-unix` adapters. |

## S3 client surface

`awskit-s3` provides AWS S3 bucket/object storage workflows: object
operations, object versioning, multipart uploads, presigned URLs, bucket
policy documents, bucket configuration helpers, endpoint configuration,
retries, streaming runtime adapters, deterministic simulation, and local
MinIO contract tests.

## Quick start

```bash
opam install awskit-s3 cohttp-eio        # Eio
opam install awskit-s3 cohttp-lwt-unix   # Lwt + Unix
```

### Eio (OCaml 5, direct-style)

```lisp
; dune
(libraries awskit-s3 awskit-s3-eio eio_main)
```

```ocaml
open Eio.Std

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let credentials =
    Awskit.Credentials.create_exn
      ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
      ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  let s3 = Awskit_s3_eio.create ~sw ~env ~region ~credentials () in
  let result =
    Awskit_s3_eio.Object.Buffer.put_string s3
      ~bucket:"my-bucket"
      ~key:"hello.txt"
      "Hello, S3!"
  in
  match result with
  | Ok uploaded ->
      Fmt.pr "Uploaded. ETag: %a@."
        (Fmt.option Awskit_s3.Object.Etag.pp)
        uploaded.etag
  | Error err -> Fmt.epr "Error: %a" Awskit_s3.Error.pp err
```

### Lwt + Unix

```lisp
; dune
(libraries awskit-s3 awskit-s3-lwt-unix)
```

```ocaml
open Lwt.Syntax

let run () =
  match Awskit_s3_lwt_unix.create () with
  | Error err -> Lwt_io.eprintf "%a\n" Awskit_s3.Error.pp err
  | Ok s3 ->
      let* result =
        Awskit_s3_lwt_unix.Object.Buffer.get_string s3
          ~bucket:"my-bucket"
          ~key:"hello.txt"
          ~max_size:1_048_576L
          ()
      in
      match result with
      | Ok (_info, body) -> Lwt_io.printl body
      | Error err -> Lwt_io.eprintf "%a\n" Awskit_s3.Error.pp err
```

When arguments are omitted, the Unix stack reads standard AWS configuration
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

Endpoint overrides are explicit; pass `endpoint` to `create` when using a
custom S3 endpoint.

### Simulation testing (no network)

```ocaml
let () =
  let credentials =
    Awskit.Credentials.create_exn
      ~access_key_id:"AK"
      ~secret_access_key:"SK"
      ()
  in
  let clock = Awskit_s3.Sim.Clock.create () in
  let store = Awskit_s3.Sim.create_store ~clock () in
  let conn = Awskit_s3.Sim.connect store ~credentials in

  (* Deterministic in-memory S3; no AWS account needed. *)
  Awskit_s3.Sim.Bucket.create conn ~bucket:"test" () |> ignore;
  Awskit_s3.Sim.Object.Buffer.put_string conn
    ~bucket:"test"
    ~key:"hello"
    "world"
  |> ignore
```

## Build & test

```bash
opam install . --deps-only --with-test
dune build
dune test
```

Optional local MinIO contract tests:

```bash
docker compose up -d
dune build @minio-contract
```

The MinIO contract runner defaults to `http://127.0.0.1:9000` with
`minioadmin` credentials from `docker-compose.yml`. Override with
`AWSKIT_S3_MINIO_ENDPOINT`, `AWSKIT_S3_MINIO_ACCESS_KEY_ID`,
`AWSKIT_S3_MINIO_SECRET_ACCESS_KEY`, and `AWSKIT_S3_MINIO_REGION`.

## Project layout

```
packages/
├── awskit/              # Pure core + runtime adapters
│   ├── eio/             # Eio adapter
│   ├── lwt/             # Generic Lwt layer + Unix backend
│   └── unix/            # OS helpers (clock, standard AWS env vars)
├── awskit-s3/           # S3 client core + adapters
│   ├── eio/             # S3 via Eio
│   ├── lwt/             # Generic Lwt adapter + Unix backend
│   └── sim/             # In-memory S3 for testing
doc/                     # Unified odoc landing page
test/                    # Tests for all packages
```

## Design

**Pure core / impure edge.** Every module in `awskit` and `awskit-s3` is pure — no IO, no OS dependencies. You pick a runtime adapter at the application boundary and pass it in. The same S3 client code works with Eio, Lwt, or a custom adapter.

## License

Proprietary — see individual `.opam` files.
