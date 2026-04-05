(** Base error type. Polymorphic variants that compose across layers — service
    packages extend with service-specific variants.

    {[
    | Error `Access_denied
    | Error (`Request_error (status, body))
    | Error (#Awskit.Error.base as e)  (* catch-all *)
    ]} *)

type base =
  [ `Access_denied  (** HTTP 403 *)
  | `Service_unavailable of string  (** HTTP 503 *)
  | `Request_error of int * string  (** HTTP error with status and body *)
  | `Body_read_error of string
  | `Invalid_response of string
  | `Invalid_header of string * string
  | `Missing_response_header of string
  | `Invalid_xml of string
  | `Invalid_json of string
  | `Invalid_request of string ]

val pp_base : Format.formatter -> base -> unit
val equal_base : base -> base -> bool
