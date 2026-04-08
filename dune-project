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
  "AWS infrastructure — signing, credentials, endpoints, and runtime adapters")
 (description
  "SigV4 signing, credentials, endpoints, error types, request/response types, and the Runtime module type. An Eio runtime adapter, a generic Lwt runtime layer, and ready-to-use Unix backends are available as separate sub-libraries.")
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
  (qcheck-alcotest :with-test)
  (eio :with-test)
  (eio_main :with-test)
  (cohttp :with-test)
  (cohttp-eio :with-test)
  (ca-certs :with-test)
  (domain-name :with-test)
  (tls :with-test)
  (tls-eio :with-test)
  (mirage-crypto-rng :with-test)
  (lwt :with-test)
  (cohttp-lwt :with-test)
  (cohttp-lwt-unix :with-test))
 (depopts
  eio
  cohttp
  cohttp-eio
  ca-certs
  domain-name
  tls
  tls-eio
  mirage-crypto-rng
  lwt
  cohttp-lwt
  cohttp-lwt-unix
  uri))

(package
 (name awskit-s3)
 (synopsis
  "S3 client — objects, buckets, multipart, policies, and simulation")
 (description
  "S3 operations, bucket configuration, multipart uploads, presigned URLs, policies, simulation, and XML wire types. Eio, generic Lwt, and Lwt + Unix adapters are available as separate sub-libraries.")
 (depends
  ocaml
  awskit
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
  (qcheck-alcotest :with-test)
  (eio_main :with-test)
  (cohttp-eio :with-test)
  (lwt :with-test)
  (cohttp-lwt-unix :with-test)))
