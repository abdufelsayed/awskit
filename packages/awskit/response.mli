(** HTTP response with case-insensitive header lookup and typed accessors. *)

type t = { status : int; headers : (string * string) list; body : string }
[@@deriving show, eq]

val header : t -> string -> string option
(** Case-insensitive. [None] if absent. *)

val header_exn :
  t ->
  string ->
  ( string,
    [> `Invalid_header of string * string | `Missing_response_header of string ]
  )
  result
(** Required header. [Error] if missing or empty. *)

val is_success : t -> bool
(** Status 2xx. *)

val header_int :
  t -> string -> (int option, [> `Invalid_header of string * string ]) result
(** Optional integer header. [Ok None] if absent. *)

val header_int_exn :
  t ->
  string ->
  ( int,
    [> `Invalid_header of string * string | `Missing_response_header of string ]
  )
  result
(** Required integer header. [Error] if missing or unparseable. *)
