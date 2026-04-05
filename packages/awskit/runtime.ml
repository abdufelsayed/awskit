module type S = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  type connection

  val region : connection -> string
  val credentials : connection -> Credentials.t
  val clock : connection -> Ptime.t
  val endpoint_host : connection -> string option
  val endpoint_port : connection -> int option
  val call : connection -> Request.t -> (Response.t, Error.base) result t
end
