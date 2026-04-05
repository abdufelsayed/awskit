# awskit

AWS infrastructure for OCaml — pure core, optional runtime adapters.

## Packages

| Package | Description |
|---------|-------------|
| **awskit** | Pure AWS infrastructure — SigV4 signing, credentials, endpoints, error types, HTTP request/response types, `Runtime` module type. Optional `awskit.eio`, `awskit.lwt`, `awskit.lwt_unix`, `awskit.unix` adapters. |
| **awskit-s3** | Pure S3 client core — objects, buckets, multipart uploads, presigned URLs, policies, simulation. Optional `awskit-s3.eio` and `awskit-s3.lwt_unix` adapters. |

## Quick start

```bash
opam install awskit awskit-s3 cohttp-eio   # Eio
opam install awskit awskit-s3 cohttp-lwt-unix  # Lwt + Unix
```

### Eio (OCaml 5, direct-style)

```ocaml
open Eio.Std

let () =
  Eio_main.run @@ fun env ->
  let credentials = Awskit.Credentials.make
    ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
    ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    ()
  in
  let conn = Awskit_eio.create
    ~env ~region:"us-east-1" ~credentials ()
  in
  match Awskit_s3_eio.Object.put conn
    ~bucket:"my-bucket" ~key:"hello.txt" "Hello, S3!"
  with
  | Ok { Awskit_s3.Object.Put_result.etag } -> Fmt.pr "Uploaded! ETag: %s" etag
  | Error err -> Fmt.epr "Error: %a" Awskit_s3.Error.pp err
```

### Lwt + Unix

```ocaml
let credentials = Awskit.Credentials.make
  ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
  ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  ()

let conn = Awskit_lwt_unix.create
  ~region:"us-east-1" ~credentials ()

let* result = Awskit_s3_lwt_unix.Object.put conn
  ~bucket:"my-bucket" ~key:"hello.txt" "Hello, S3!"
```

### Simulation testing (no network)

```ocaml
let clock = Awskit_s3.Sim.Clock.create ()
let store = Awskit_s3.Sim.create_store ~clock ()
let conn = Awskit_s3.Sim.connect store ~credentials

(* Deterministic in-memory S3 — no AWS account needed *)
Awskit_s3.Sim.Bucket.create conn ~bucket:"test" () |> ignore
Awskit_s3.Sim.Object.put conn ~bucket:"test" ~key:"hello" "world" |> ignore
```

## Build & test

```bash
opam install . --deps-only --with-test
dune build
dune test
```

Integration tests need MinIO:

```bash
docker compose up -d
dune test
```

## Project layout

```
packages/
├── awskit/              # Pure core + runtime adapters
│   ├── eio/             # Eio adapter
│   ├── lwt/             # Generic Lwt functor
│   ├── lwt_unix/        # Lwt + Cohttp + Unix
│   └── unix/            # OS helpers (clock, env credentials)
├── awskit-s3/           # S3 client core + adapters
│   ├── eio/             # S3 via Eio
│   ├── lwt_unix/        # S3 via Lwt + Unix
│   └── sim/             # In-memory S3 for testing
doc/                     # Unified odoc landing page
test/                    # Tests for all packages
```

## Design

**Pure core / impure edge.** Every module in `awskit` and `awskit-s3` is pure — no IO, no OS dependencies. You pick a runtime adapter at the application boundary and pass it in. The same S3 client code works with Eio, Lwt, or a custom adapter.

## License

Proprietary — see individual `.opam` files.