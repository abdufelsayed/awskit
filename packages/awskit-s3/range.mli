(** Byte range for partial object reads.

    Use with [Object.get] to download only part of an object:
    {[
    (* First 1024 bytes *)
    let range = Range.Bytes (0, 1023)

    (* Everything from offset 4096 onward *)
    let range = Range.From 4096

    (* Last 1024 bytes *)
    let range = Range.Suffix 1024
    ]} *)

(** A byte range.

    - [Bytes (start, end_)] — inclusive range from [start] to [end_]
    - [From offset] — everything from [offset] onward
    - [Suffix length] — the last [length] bytes *)
type t = Bytes of int * int | From of int | Suffix of int
[@@deriving show, eq]

val to_header : t -> (string, [> `Invalid_request of string ]) result
(** Convert to an HTTP [Range] header value.

    Returns [Error] if the range is invalid (e.g., negative offsets, start >
    end). *)
