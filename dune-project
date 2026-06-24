(lang dune 3.22)

(name awskit)

(generate_opam_files true)

(source
 (github abdufelsayed/awskit))

(documentation "https://abdufelsayed.github.io/awskit/")

(authors "Abdullah Elsayed <hey@abdufelsayed.dev>")

(maintainers "Abdullah Elsayed <hey@abdufelsayed.dev>")

(license MIT)

(package
 (name awskit)
 (synopsis
  "AWS infrastructure — signing, credentials, endpoints, and core types")
 (description
  "SigV4 signing, credentials, endpoints, error types, request/response types, and the Runtime module type. Runtime adapters are available as separate packages.")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (>= v0.16.3))
  (digestif
   (>= 1.3.0))
  ptime
  (fmt
   (>= 0.11.0))
  ppx_deriving
  ppx_sexp_conv
  (dune-release :with-dev-setup)
  (opam-publish :with-dev-setup)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name awskit-unix)
 (synopsis "Unix helpers for Awskit")
 (description
  "Unix-specific AWS helpers for wall-clock time, environment variables, and shared credential files.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (base
   (>= v0.16.3))
  ptime
  (fmt
   (>= 0.11.0))))

(package
 (name awskit-lwt)
 (synopsis "Lwt runtime adapter for Awskit")
 (description
  "Generic Lwt runtime adapter for Awskit over a caller-supplied Cohttp Lwt client.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (base
   (>= v0.16.3))
  lwt
  (cohttp
   (>= 6.2.1))
  (cohttp-lwt
   (>= 6.2.1))
  uri
  ptime
  (fmt
   (>= 0.11.0))
  logs
  ppx_deriving
  (cohttp-lwt-unix
   (and
    :with-test
    (>= 6.2.1)))
  (alcotest :with-test)))

(package
 (name awskit-lwt-unix)
 (synopsis "Lwt Unix runtime adapter for Awskit")
 (description
  "Ready-to-use Lwt and Unix runtime adapter for Awskit using Cohttp Lwt Unix.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (awskit-unix
   (= :version))
  (awskit-lwt
   (= :version))
  (cohttp
   (>= 6.2.1))
  (cohttp-lwt
   (>= 6.2.1))
  (cohttp-lwt-unix
   (>= 6.2.1))
  lwt
  uri
  ptime
  yojson
  (alcotest :with-test)))

(package
 (name awskit-eio)
 (synopsis "Eio runtime adapter for Awskit")
 (description
  "Direct-style Eio runtime adapter for Awskit with caller-owned transport and HTTPS policy.")
 (depends
  (ocaml
   (>= 5.2))
  (awskit
   (= :version))
  (base
   (>= v0.16.3))
  (eio
   (>= 1.3))
  (cohttp
   (>= 6.2.1))
  (cohttp-eio
   (>= 6.2.1))
  uri
  ptime
  (fmt
   (>= 0.11.0))
  logs
  ppx_deriving
  (eio_main
   (and
    :with-test
    (>= 1.3)))
  (alcotest :with-test)))

(package
 (name awskit-s3)
 (synopsis
  "S3 client core — objects, buckets, multipart, and policies")
 (description
  "S3 operations, bucket configuration, multipart uploads, presigned URLs, policies, and XML wire types. Runtime adapters are available as separate packages.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (digestif
   (>= 1.3.0))
  ptime
  (fmt
   (>= 0.11.0))
  yojson
  (base64
   (>= 3.5.2))
  uucp
  uri
  ezxmlm
  ppx_protocol_conv_xmlm
  ppx_deriving
  ppx_yojson_conv
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name awskit-s3-sim)
 (synopsis "In-memory simulator for Awskit S3 tests")
 (description
  "Deterministic in-memory S3 implementation for tests, including buckets, objects, multipart uploads, versioning, checksums, and fault injection.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (awskit-s3
   (= :version))
  (digestif
   (>= 1.3.0))
  ptime
  (fmt
   (>= 0.11.0))
  (base64
   (>= 3.5.2))
  uri
  (alcotest :with-test)))

(package
 (name awskit-s3-lwt)
 (synopsis "Lwt adapter for Awskit S3")
 (description "S3 client adapter over the generic Awskit Lwt runtime.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (awskit-lwt
   (= :version))
  (awskit-s3
   (= :version))
  (cohttp-lwt
   (>= 6.2.1))
  lwt
  ptime
  (cohttp-lwt-unix
   (and
    :with-test
    (>= 6.2.1)))
  (alcotest :with-test)))

(package
 (name awskit-s3-lwt-unix)
 (synopsis "Lwt Unix adapter for Awskit S3")
 (description
  "Ready-to-use S3 client adapter over the Awskit Lwt Unix runtime.")
 (depends
  (ocaml
   (>= 4.14))
  (awskit
   (= :version))
  (awskit-lwt-unix
   (= :version))
  (awskit-s3
   (= :version))
  (awskit-s3-lwt
   (= :version))
  (cohttp-lwt-unix
   (>= 6.2.1))
  lwt
  (fmt
   (>= 0.11.0))
  ptime
  (alcotest :with-test)))

(package
 (name awskit-s3-eio)
 (synopsis "Eio adapter for Awskit S3")
 (description
  "S3 client adapter over the Awskit Eio runtime with caller-owned HTTPS transport policy.")
 (depends
  (ocaml
   (>= 5.2))
  (awskit
   (= :version))
  (awskit-eio
   (= :version))
  (awskit-s3
   (= :version))
  (fmt
   (>= 0.11.0))
  (eio
   (>= 1.3))
  ptime
  (awskit-unix
   (and
    :with-test
    (= :version)))
  (eio_main
   (and
    :with-test
    (>= 1.3)))
  (digestif
   (and
    :with-test
    (>= 1.3.0)))
  (alcotest :with-test)))
