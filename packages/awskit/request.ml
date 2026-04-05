open Base

type meth = GET | PUT | POST | DELETE | HEAD [@@deriving show, eq]

type t = {
  meth : meth;
  host : string;
  port : int option;
  path : string;
  headers : (string * string) list;
  body : string;
}
[@@deriving show, eq]

let meth_to_string = function
  | GET -> "GET"
  | PUT -> "PUT"
  | POST -> "POST"
  | DELETE -> "DELETE"
  | HEAD -> "HEAD"
