(** AWS service endpoint.

    Endpoint values are pure configuration. Runtime adapters use them when
    constructing transport requests. *)

module Scheme : sig
  type t = [ `Http | `Https ] [@@deriving show, eq]

  val to_string : t -> string
end

type t

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

val create :
  scheme:Scheme.t -> host:string -> ?port:int -> unit -> (t, Error.t) result

val create_exn : scheme:Scheme.t -> host:string -> ?port:int -> unit -> t
val of_string : string -> (t, Error.t) result
val of_string_exn : string -> t
val http : host:string -> ?port:int -> unit -> (t, Error.t) result
val http_exn : host:string -> ?port:int -> unit -> t
val https : host:string -> ?port:int -> unit -> (t, Error.t) result
val https_exn : host:string -> ?port:int -> unit -> t
val scheme : t -> Scheme.t
val host : t -> string
val port : t -> int option
val authority : t -> string
val to_url_prefix : t -> string
