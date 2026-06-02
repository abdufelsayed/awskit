module type S = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  type connection
  type request_body
  type response_body
  type request_body_writer
  type response_body_reader

  val now : connection -> Ptime.t
  val region : connection -> Region.t
  val credentials : connection -> (Credentials.t, Error.t) result t
  val endpoint : connection -> Endpoint.t option
  val retry_policy : connection -> Retry.t
  val sleep : connection -> Ptime.Span.t -> unit t

  module Request_body : sig
    val empty : request_body
    val of_string : string -> request_body
    val of_bytes : bytes -> request_body

    val of_stream :
      Body.Request.descriptor ->
      write:(request_body_writer -> (unit, Error.t) result t) ->
      request_body

    val descriptor : request_body -> Body.Request.descriptor
    val write_string : request_body_writer -> string -> (unit, Error.t) result t
  end

  module Response_body : sig
    val read :
      response_body_reader ->
      bytes ->
      off:int ->
      len:int ->
      (int, Error.t) result t

    val with_reader :
      response_body ->
      consume:(response_body_reader -> ('a, Error.t) result t) ->
      ('a, Error.t) result t

    val discard : response_body -> (unit, Error.t) result t
  end

  val with_response :
    connection ->
    Request.t ->
    request_body ->
    f:(Response.t -> response_body -> ('a, Error.t) result t) ->
    ('a, Error.t) result t
end
