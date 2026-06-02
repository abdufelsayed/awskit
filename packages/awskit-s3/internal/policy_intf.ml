open Common

module type POLICY = sig
  type t

  val of_json : string -> (t, Error.t) result
  val to_json : t -> string
end
