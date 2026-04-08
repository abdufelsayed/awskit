(** AWS service endpoint. Runtime adapters construct these from the region;
    override for LocalStack, MinIO, etc:
    {[
    Awskit.Endpoint.http ~host:"localhost" ~port:4566 ()
    ]} *)

(** URL scheme. *)
module Scheme : sig
  type t = [ `Http | `Https ] [@@deriving show, eq]

  val to_string : t -> string
end

type t
(** Opaque. [Invalid_argument] on invalid hosts (empty, contains control
    characters, or includes URL syntax) or ports out of range. *)

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

val create : scheme:Scheme.t -> host:string -> ?port:int -> unit -> t
(** [Invalid_argument] if host is empty or port out of range (1–65535). *)

val http : host:string -> ?port:int -> unit -> t
val https : host:string -> ?port:int -> unit -> t
val scheme : t -> Scheme.t
val host : t -> string
val port : t -> int option

val authority : t -> string
(** {v [host:port] or just [host]. v} *)

val to_url_prefix : t -> string
(** e.g. ["https://s3.us-east-1.amazonaws.com"]. *)
