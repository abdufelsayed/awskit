(** Runtime abstraction for concurrency adapters.

    Runtime adapters own concrete request and response body values. Service
    packages use this module type without depending on Eio, Lwt, or Unix. *)

module type S = sig
  type +'a t
  (** Runtime effect type, such as ['a] for direct-style Eio adapters or
      ['a Lwt.t] for Lwt adapters. *)

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  type connection
  (** Runtime connection/configuration handle passed to service operations. *)

  type request_body
  (** Runtime-owned request body. *)

  type response_body
  (** Runtime-owned response body. Must be consumed through scoped helpers. *)

  type request_body_writer
  type response_body_reader

  val now : connection -> Ptime.t
  (** Current signing time. *)

  val region : connection -> Region.t
  (** AWS region used for signing and default endpoint resolution. *)

  val credentials : connection -> (Credentials.t, Error.t) result t
  (** Resolve credentials for the next signed request. *)

  val endpoint : connection -> Endpoint.t option
  (** Optional generic endpoint override. Service packages may expose richer
      endpoint configuration when addressing rules are service-specific. *)

  val retry_policy : connection -> Retry.t
  (** Retry policy used by service packages. *)

  val sleep : connection -> Ptime.Span.t -> unit t
  (** Sleep between retry attempts. *)

  module Request_body : sig
    val empty : request_body
    (** Empty replayable body. *)

    val of_string : string -> request_body
    (** Replayable body backed by a string. *)

    val of_bytes : bytes -> request_body
    (** Replayable body backed by bytes. *)

    val of_stream :
      Body.Request.descriptor ->
      write:(request_body_writer -> (unit, Error.t) result t) ->
      request_body
    (** Streaming body backed by a writer callback.

        The descriptor must accurately describe the stream. Mark the body as
        replayable only if calling [write] again sends the same bytes. *)

    val descriptor : request_body -> Body.Request.descriptor
    (** Return the signing and retry metadata for a request body. *)

    val write_string : request_body_writer -> string -> (unit, Error.t) result t
    (** Write a chunk to a streaming request body. *)
  end

  module Response_body : sig
    val read :
      response_body_reader ->
      bytes ->
      off:int ->
      len:int ->
      (int, Error.t) result t
    (** Returns [0] at end-of-body. *)

    val with_reader :
      response_body ->
      consume:(response_body_reader -> ('a, Error.t) result t) ->
      ('a, Error.t) result t
    (** Scope a response body reader to [consume]. Callers must not retain the
        reader after [consume] returns. *)

    val discard : response_body -> (unit, Error.t) result t
    (** Drain and ignore the response body. *)
  end

  val with_response :
    connection ->
    Request.t ->
    request_body ->
    f:(Response.t -> response_body -> ('a, Error.t) result t) ->
    ('a, Error.t) result t
  (** Send a request and provide the response metadata and body to [f].

      Service packages use this primitive for all HTTP operations. Runtime
      adapters own connection reuse, body lifetime, and any post-consumer drain
      behavior. *)
end
