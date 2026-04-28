(** Structured core AWS SDK errors.

    Service packages should preserve this value and add classifier helpers over
    it instead of widening errors into service-specific polymorphic variants. *)

type validation = { field : string option; message : string }
type transport = { message : string; retryable : bool; cause : string option }

type service = {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}

type body = { message : string; limit : int64 option }

type retry_class =
  [ `Retryable
  | `Throttled
  | `Auth
  | `Conflict
  | `Not_found
  | `Fatal
  | `Unknown ]

type t =
  | Validation of validation
  | Signing of string
  | Transport of transport
  | Service of service
  | Decode of string
  | Body of body

val validation : ?field:string -> string -> t
val signing : string -> t
val transport : ?cause:string -> retryable:bool -> string -> t
val service : service -> t
val decode : string -> t
val body : ?limit:int64 -> string -> t
val retry_class : t -> retry_class
val is_not_found : t -> bool
val pp : Format.formatter -> t -> unit
val to_string_hum : t -> string
val equal : t -> t -> bool
