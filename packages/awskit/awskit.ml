(** Pure AWS infrastructure: signing, credentials, endpoints, structured errors,
    body metadata, HTTP metadata, and runtime contracts. No IO. *)

module Credentials = Credentials
module Region = Region
module Endpoint = Endpoint
module Error = Error
module Body = Body
module Retry = Retry
module Timeout = Timeout
module Signing = Signing
module Request = Request
module Response = Response
module Runtime = Runtime
