(** Body-free HTTP request metadata.

    Runtime adapters receive request bodies separately through
    {!Runtime.Transport.with_response}. *)

module Method : sig
  type t = [ `GET | `PUT | `POST | `DELETE | `HEAD | `PATCH ]
  (** HTTP methods modeled by Awskit service packages. *)

  val to_string : t -> string
  (** Render the uppercase HTTP method. *)

  val of_string : string -> (t, Error.t) result
  (** Parse a supported HTTP method name case-insensitively. *)

  val of_string_exn : string -> t
  (** Like {!val:of_string}, but raises [Error.Awskit_error] carrying the
      structured validation error on validation failure. *)
end

module Target : sig
  type t = private {
    scheme : Endpoint.Scheme.t;
    host : string;
    port : int option;
    path : string;
    query : (string * string list) list;
  }
  (** Request target split into endpoint authority, path, and structured query
      parameters. Query values are kept as lists so repeated AWS parameters are
      preserved for signing. *)

  val create :
    scheme:Endpoint.Scheme.t ->
    host:string ->
    ?port:int ->
    path:string ->
    ?query:(string * string list) list ->
    unit ->
    (t, Error.t) result
  (** Create a target. [path] must be absolute and already encoded for
      transport. [query] is kept structured and encoded during signing or
      request serialization. *)

  val create_exn :
    scheme:Endpoint.Scheme.t ->
    host:string ->
    ?port:int ->
    path:string ->
    ?query:(string * string list) list ->
    unit ->
    t
  (** Like {!val:create}, but raises [Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val authority : t -> string
  (** Host plus optional port. *)

  val path_and_query : t -> string
  (** Render the encoded path and structured query string for transport. Query
      order is preserved; signing applies SigV4 canonical sorting separately. *)
end

type t = private {
  method_ : Method.t;
  target : Target.t;
  headers : (string * string) list;
}
(** Request metadata without a request body. Runtime adapters receive the body
    separately so service packages can stream without buffering. *)

val validate_headers : (string * string) list -> (unit, Error.t) result
(** Validate header names and values before signing or transport. *)

val create :
  method_:Method.t ->
  target:Target.t ->
  ?headers:(string * string) list ->
  unit ->
  (t, Error.t) result
(** Create request metadata. Headers are preserved in caller-provided order
    except where signing later canonicalizes them. *)

val create_exn :
  method_:Method.t ->
  target:Target.t ->
  ?headers:(string * string) list ->
  unit ->
  t
(** Like {!val:create}, but raises [Error.Awskit_error] carrying the structured
    validation error on validation failure. *)

val with_headers : t -> (string * string) list -> (t, Error.t) result
(** Replace all headers after validating them. *)

val add_header : t -> name:string -> value:string -> (t, Error.t) result
(** Append one header after validation. *)
