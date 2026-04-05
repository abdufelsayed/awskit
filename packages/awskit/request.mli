(** HTTP request type. Service packages build these; runtime adapters send them.
    Exposed for custom adapters. *)

type meth = GET | PUT | POST | DELETE | HEAD [@@deriving show, eq]

type t = {
  meth : meth;
  host : string;
  port : int option;
  path : string;  (** Includes query string *)
  headers : (string * string) list;
  body : string;
}
[@@deriving show, eq]

val meth_to_string : meth -> string
