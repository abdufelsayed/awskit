(** Runtime abstraction for concurrency adapters.

    Runtime adapters own concrete request and response body values. Service
    packages use this module type without depending on Eio, Lwt, or Unix. *)

module type IO = sig
  type +'a t
  (** Runtime effect type, such as ['a] for direct-style Eio adapters or
      ['a Lwt.t] for Lwt adapters. *)

  val return : 'a -> 'a t
  (** Lift a pure value into the runtime effect. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Sequence runtime effects. *)
end

module type Request_body = sig
  type +'a io

  type t
  (** Runtime-owned request body value. *)

  type writer
  (** Scoped request-body writer passed only to [of_stream] callbacks. *)

  val empty : t
  (** Empty replayable body. *)

  val of_string : string -> t
  (** Replayable body backed by a string. *)

  val of_bytes : bytes -> t
  (** Replayable body backed by bytes. *)

  val of_stream :
    Body.Request.descriptor -> write:(writer -> (unit, Error.t) result io) -> t
  (** Streaming body backed by a writer callback.

      The descriptor must accurately describe the stream. Mark the body as
      replayable only if calling [write] again sends the same bytes. Runtime
      adapters may call [write] once per retry attempt, and may cancel it when
      the request attempt is abandoned or times out. *)

  val descriptor : t -> Body.Request.descriptor
  (** Return the signing and retry metadata for a request body. *)

  val content_length : t -> int64 option
  (** Return the declared body length, when known. *)

  val write_string : writer -> string -> (unit, Error.t) result io
  (** Write a chunk to a streaming request body. The writer is valid only during
      the [of_stream] callback invocation. *)

  val write_bytes : writer -> bytes -> (unit, Error.t) result io
  (** Write a bytes chunk to a streaming request body. The writer is valid only
      during the [of_stream] callback invocation. *)

  val write_subbytes :
    writer -> bytes -> off:int -> len:int -> (unit, Error.t) result io
  (** Write a bounded bytes slice to a streaming request body. Invalid bounds
      return [Error Body]. *)
end

module type Response_body = sig
  type +'a io

  type t
  (** Runtime-owned response body value. *)

  type reader
  (** Scoped response-body reader. It must not be used outside
      {!val:with_reader}. *)

  val read : reader -> bytes -> off:int -> len:int -> (int, Error.t) result io
  (** Read into [bytes]. Returns [Ok 0] at end-of-body. Invalid bounds return
      [Error Body]. Native runtime cancellation remains native. After a read
      error, timeout, or cancellation, the reader is no longer valid. *)

  val next : ?chunk_size:int -> reader -> (bytes option, Error.t) result io
  (** Read the next chunk, or [None] at end-of-body. [chunk_size] must be
      positive. *)

  val with_reader :
    t -> consume:(reader -> ('a, Error.t) result io) -> ('a, Error.t) result io
  (** Scope a response body reader to [consume].

      Callers must not retain the reader after [consume] returns. Runtime
      adapters must attempt to drain or discard the response body after
      [consume]. If [consume] returns [Ok _], a drain failure is returned. If
      [consume] returns [Error _], that error wins over cleanup failures. If
      [consume] raises or native cancellation occurs, cleanup is attempted and
      the original exception or cancellation is preserved. *)

  val discard : t -> (unit, Error.t) result io
  (** Drain and ignore the response body, subject to the runtime drain timeout
      and byte limit. *)
end

module type Transport = sig
  type +'a io
  type connection
  type request_body
  type response_body

  val with_response :
    connection ->
    Request.t ->
    body:request_body ->
    consume:(Response.t -> response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io
  (** Send a request and provide the response metadata and body to [consume].

      The request body is owned by the runtime for the duration of one attempt.
      The response body is valid only inside [consume]; callers should scope
      reads with [Response_body.with_reader]. Callback exceptions and native
      cancellation must be preserved by adapters. *)
end

module type Clock = sig
  type connection

  val now : connection -> Ptime.t
  (** Return the runtime clock used for signing and credential freshness. *)
end

module type Sleeper = sig
  type +'a io
  type connection

  val sleep : connection -> Ptime.Span.t -> unit io
  (** Sleep for retry backoff and timeout support. *)
end

module type Random = sig
  type connection

  val float : connection -> upper_bound:float -> float
  (** Return a random float from zero up to [upper_bound] for retry jitter. *)
end

module type Credentials_capability = sig
  type +'a io
  type connection

  val resolve : connection -> (Credentials.t, Error.t) result io
  (** Resolve credentials for the next signed request. *)
end

module type Endpoint_capability = sig
  type connection

  val region : connection -> Region.t
  (** Return the configured AWS region. *)

  val endpoint : connection -> Endpoint.t option
  (** Return the explicit endpoint override, if configured. *)
end

module type Retry = sig
  type connection

  val policy : connection -> Retry.t
  (** Return the retry policy for service operations. *)
end

module type Timeout = sig
  type connection

  val policy : connection -> Timeout.policy
  (** Return the runtime timeout policy. *)
end

module type S = sig
  type +'a t
  type connection
  type request_body
  type response_body
  type request_body_writer
  type response_body_reader

  module IO : IO with type 'a t = 'a t

  module Request_body :
    Request_body
      with type 'a io = 'a t
       and type t = request_body
       and type writer = request_body_writer

  module Response_body :
    Response_body
      with type 'a io = 'a t
       and type t = response_body
       and type reader = response_body_reader

  module Transport :
    Transport
      with type 'a io = 'a t
       and type connection = connection
       and type request_body = request_body
       and type response_body = response_body

  module Clock : Clock with type connection = connection

  module Sleeper :
    Sleeper with type 'a io = 'a t and type connection = connection

  module Random : Random with type connection = connection

  module Credentials :
    Credentials_capability
      with type 'a io = 'a t
       and type connection = connection

  module Endpoint : Endpoint_capability with type connection = connection
  module Retry : Retry with type connection = connection
  module Timeout : Timeout with type connection = connection
end
