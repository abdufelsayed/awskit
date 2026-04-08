(** Pure AWS infrastructure — signing, credentials, endpoints, error types, HTTP
    request/response types, and the {!Runtime} module type.

    No IO. Concurrency/platform code lives in sub-libraries:
    - [awskit.eio] — Eio runtime adapter
    - [awskit.lwt] — generic Lwt runtime functor
    - [awskit.lwt_unix] — ready-to-use Lwt + Unix backend
    - [awskit.unix] — Unix credential sources and clock *)

module Credentials = Credentials
(** AWS credentials for request signing. *)

module Endpoint = Endpoint
(** AWS service endpoint configuration. *)

module Error = Error
(** Base error types — polymorphic variants that compose across services. *)

module Signing = Signing
(** AWS Signature Version 4 signing. *)

module Request = Request
(** HTTP request type for AWS API calls. *)

module Response = Response
(** HTTP response with header lookup helpers. *)

module Runtime = Runtime
(** Runtime abstraction for concurrency adapters. *)
