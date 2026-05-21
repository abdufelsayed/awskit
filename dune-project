(lang dune 3.22)

(name awskit)

(generate_opam_files true)

(source
 (github weldr-ai/awskit))

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
  ocaml
  base
  digestif
  ptime
  fmt
  logs
  ppx_deriving
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name awskit-unix)
 (synopsis "Unix helpers for Awskit")
 (description
  "Unix-specific AWS helpers for wall-clock time, environment variables, and shared credential files.")
 (depends
  ocaml
  (awskit
   (= :version))
  base
  ptime
  fmt))

(package
 (name awskit-lwt)
 (synopsis "Lwt runtime adapter for Awskit")
 (description
  "Generic Lwt runtime adapter for Awskit over a caller-supplied Cohttp Lwt client.")
 (depends
  ocaml
  (awskit
   (= :version))
  base
  lwt
  (cohttp
   (>= 6.2.1))
  (cohttp-lwt
   (>= 6.2.1))
  uri
  ptime
  fmt
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
  ocaml
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
  "Direct-style Eio runtime adapter for Awskit using Cohttp Eio.")
 (depends
  ocaml
  (awskit
   (= :version))
  base
  eio
  (cohttp
   (>= 6.2.1))
  (cohttp-eio
   (>= 6.2.1))
  uri
  ptime
  fmt
  logs
  ca-certs
  domain-name
  tls
  tls-eio
  mirage-crypto-rng
  ppx_deriving
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name awskit-s3)
 (synopsis
  "S3 client core — objects, buckets, multipart, policies, and simulation")
 (description
  "S3 operations, bucket configuration, multipart uploads, presigned URLs, policies, simulation, and XML wire types. Runtime adapters are available as separate packages.")
 (depends
  ocaml
  (awskit
   (= :version))
  base
  digestif
  ptime
  fmt
  logs
  yojson
  base64
  uri
  ezxmlm
  ppx_protocol_conv_xmlm
  ppx_deriving
  ppx_yojson_conv
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name awskit-s3-lwt)
 (synopsis "Lwt adapter for Awskit S3")
 (description
  "S3 client adapter over the generic Awskit Lwt runtime.")
 (depends
  ocaml
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
  ocaml
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
  fmt
  ptime
  (alcotest :with-test)))

(package
 (name awskit-s3-eio)
 (synopsis "Eio adapter for Awskit S3")
 (description
  "S3 client adapter over the Awskit Eio runtime.")
 (depends
  ocaml
  (awskit
   (= :version))
  (awskit-eio
   (= :version))
  (awskit-s3
   (= :version))
  fmt
  eio
  ptime
  (awskit-unix
   (and
    :with-test
    (= :version)))
  (eio_main :with-test)
  (digestif :with-test)
  (alcotest :with-test)))
