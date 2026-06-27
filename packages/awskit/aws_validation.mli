val invalid : ?field:string -> string -> ('a, Error.t) result
val has_control_or_delete : string -> bool
val has_leading_or_trailing_whitespace : string -> bool
val validate_port : int option -> (unit, Error.t) result

module Host : sig
  val validate : ?allow_ipv6_brackets:bool -> string -> (unit, Error.t) result
end

module Header : sig
  val validate_name : string -> (unit, Error.t) result
  val validate_value : string -> string -> (unit, Error.t) result
  val validate_list : (string * string) list -> (unit, Error.t) result
end
