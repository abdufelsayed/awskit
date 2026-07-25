module Error = S3_error

module type RUNTIME = Execution_runtime.S

module type S = sig
  module R : RUNTIME

  type session
  type connection = R.connection
  type 'a io = 'a R.t
  type request_body = R.request_body
  type response_body_reader = R.response_body_reader

  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
  val return_ok : 'a -> ('a, Error.t) result io
  val return_error : Error.t -> ('a, Error.t) result io
  val endpoint_config : connection -> Endpoint_resolver.t
  val region : connection -> Awskit.Region.t
  val now : connection -> Ptime.t
  val credentials : connection -> (Awskit.Credentials.t, Error.t) result io

  val object_request :
    connection ->
    bucket:string ->
    key:string ->
    (Endpoint_resolver.Request.t, Error.t) result

  val bucket_request :
    connection ->
    bucket:string ->
    suffix:string ->
    signing_suffix:string ->
    (Endpoint_resolver.Request.t, Error.t) result

  val root_request : connection -> (Endpoint_resolver.Request.t, Error.t) result

  val read_body :
    response_body_reader -> max_size:int64 -> (string, Error.t) result io

  val read_body_bytes :
    response_body_reader -> max_size:int64 -> (bytes, Error.t) result io

  val read_response_body :
    R.response_body -> max_size:int64 -> (string, Error.t) result io

  val discard_response_body : R.response_body -> (unit, Error.t) result io
  val content_md5 : string -> string

  val with_operation :
    connection ->
    operation:Operation.t ->
    (session -> ('a, Error.t) result io) ->
    ('a, Error.t) result io

  val with_response_in_session :
    connection ->
    session:session ->
    method_:Awskit.Request.Method.t ->
    request:Endpoint_resolver.Request.t ->
    query:(string * string list) list ->
    headers:(string * string) list ->
    payload_hash:Awskit.Body.Payload_hash.t ->
    request_body ->
    f:(Awskit.Response.t -> R.response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io

  val with_discarded_response_in_session :
    connection ->
    session:session ->
    method_:Awskit.Request.Method.t ->
    request:Endpoint_resolver.Request.t ->
    query:(string * string list) list ->
    headers:(string * string) list ->
    payload_hash:Awskit.Body.Payload_hash.t ->
    request_body ->
    f:(Awskit.Response.t -> R.response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io

  val with_empty_response_in_session :
    connection ->
    session:session ->
    method_:Awskit.Request.Method.t ->
    request:Endpoint_resolver.Request.t ->
    query:(string * string list) list ->
    headers:(string * string) list ->
    f:(Awskit.Response.t -> R.response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io

  val with_empty_discarded_response_in_session :
    connection ->
    session:session ->
    method_:Awskit.Request.Method.t ->
    request:Endpoint_resolver.Request.t ->
    query:(string * string list) list ->
    headers:(string * string) list ->
    f:(Awskit.Response.t -> R.response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io

  val with_retryable_embedded_response_in_session :
    connection ->
    session:session ->
    method_:Awskit.Request.Method.t ->
    request:Endpoint_resolver.Request.t ->
    query:(string * string list) list ->
    headers:(string * string) list ->
    payload_hash:Awskit.Body.Payload_hash.t ->
    request_body ->
    f:(Awskit.Response.t -> R.response_body -> ('a, Error.t) result io) ->
    ('a, Error.t) result io
end
