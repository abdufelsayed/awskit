(** Runtime-agnostic AWS infrastructure: signing, credentials, endpoints,
    structured errors, body metadata, HTTP metadata, and the {!Runtime} module
    type.

    The core package performs no IO. Concurrency and platform-specific code live
    in sub-libraries:
    - [awskit-eio] — Eio runtime adapter
    - [awskit-lwt] — generic Lwt runtime functor
    - [awskit-lwt-unix] — ready-to-use Lwt + Unix backend
    - [awskit-unix] — Unix credential sources and clock *)

module Credentials = Credentials
(** AWS credentials for request signing. *)

module Region = Region
(** AWS region names and pure validation. *)

module Endpoint = Endpoint
(** AWS service endpoint configuration. *)

module Error = Error
(** Structured core error type. *)

module Body = Body
(** Runtime-agnostic body metadata. *)

module Retry = Retry
(** Shared retry policy for AWS service packages. *)

module Signing = Signing
(** AWS Signature Version 4 signing. *)

module Request = Request
(** Body-free HTTP request metadata. *)

module Response = Response
(** Body-free HTTP response metadata. *)

module Runtime = Runtime
(** Runtime abstraction for concurrency adapters. *)
