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
  val empty_request_body : request_body
  val string_request_body : string -> request_body
  val bytes_request_body : bytes -> request_body

  val stream_request_body :
    Body.Request.descriptor ->
    write:(request_body_writer -> (unit, Error.t) result t) ->
    request_body

  val request_body_descriptor : request_body -> Body.Request.descriptor

  val write_request_body_string :
    request_body_writer -> string -> (unit, Error.t) result t

  val read_response_body :
    response_body_reader ->
    bytes ->
    off:int ->
    len:int ->
    (int, Error.t) result t

  val with_response_body :
    response_body ->
    consume:(response_body_reader -> ('a, Error.t) result t) ->
    ('a, Error.t) result t

  val discard_response_body : response_body -> (unit, Error.t) result t

  val with_response :
    connection ->
    Request.t ->
    request_body ->
    f:(Response.t -> response_body -> ('a, Error.t) result t) ->
    ('a, Error.t) result t
end
