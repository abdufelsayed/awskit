(** Body-free HTTP response metadata. *)

type t = private {
  status : int;
  headers : (string * string) list;
  request_id : string option;
  host_id : string option;
}

val create :
  status:int -> ?headers:(string * string) list -> unit -> (t, Error.t) result

val create_exn : status:int -> ?headers:(string * string) list -> unit -> t
val status : t -> int
val headers : t -> (string * string) list
val header : t -> string -> string option
val required_header : t -> string -> (string, Error.t) result
val header_int : t -> string -> (int option, Error.t) result
val is_success : t -> bool
val request_id : t -> string option
val host_id : t -> string option
