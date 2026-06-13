(** AWS service endpoint.

    Endpoint values are pure configuration. Runtime adapters use them when
    constructing transport requests. *)

module Scheme : sig
  type t = [ `Http | `Https ] [@@deriving show, eq]
  (** Transport scheme used when constructing request URLs. *)

  val to_string : t -> string
  (** Render as ["http"] or ["https"]. *)
end

type t
(** Parsed endpoint without a path. Use request targets for service-specific
    paths and query strings. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print an endpoint as [scheme://authority]. *)

val equal : t -> t -> bool
(** Compare endpoint values structurally. *)

val create :
  scheme:Scheme.t -> host:string -> ?port:int -> unit -> (t, Error.t) result
(** Create an endpoint from structured parts. [host] must be non-empty and must
    not include a scheme, path, or port. *)

val create_exn : scheme:Scheme.t -> host:string -> ?port:int -> unit -> t
(** Like {!val:create}, but raises [Invalid_argument] on validation failure. *)

val of_string : string -> (t, Error.t) result
(** Parse an [http://] or [https://] endpoint URL. Paths and queries are not
    accepted because service packages construct those per request. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Invalid_argument] on validation failure.
*)

val http : host:string -> ?port:int -> unit -> (t, Error.t) result
(** Create an HTTP endpoint. *)

val http_exn : host:string -> ?port:int -> unit -> t
(** Like {!val:http}, but raises [Invalid_argument] on validation failure. *)

val https : host:string -> ?port:int -> unit -> (t, Error.t) result
(** Create an HTTPS endpoint. *)

val https_exn : host:string -> ?port:int -> unit -> t
(** Like {!val:https}, but raises [Invalid_argument] on validation failure. *)

val scheme : t -> Scheme.t
(** Return the endpoint scheme. *)

val host : t -> string
(** Return the host without scheme, path, query, or port. *)

val port : t -> int option
(** Return the explicit port, if one was configured. *)

val authority : t -> string
(** Host plus optional port, suitable for the HTTP [Host] header. *)

val to_url_prefix : t -> string
(** Render [scheme://authority] without a trailing slash. *)
