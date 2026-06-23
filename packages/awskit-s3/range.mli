(** HTTP byte-range requests for S3 object reads. *)

type t
(** Validated HTTP byte range. *)

type view =
  | Bytes of int64 * int64
  | From of int64
  | Suffix of int64
      (** Inspectable range shape. [Bytes (start, finish)] is inclusive. *)

val bytes : start:int64 -> finish:int64 -> (t, Awskit.Error.t) result
(** Range from [start] through inclusive [finish]. *)

val bytes_exn : start:int64 -> finish:int64 -> t
(** Like {!val:bytes}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val from : int64 -> (t, Awskit.Error.t) result
(** Range from a starting byte offset through the end of the object. *)

val from_exn : int64 -> t
(** Like {!val:from}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val suffix : int64 -> (t, Awskit.Error.t) result
(** Range for the final byte count of an object. *)

val suffix_exn : int64 -> t
(** Like {!val:suffix}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val to_header : t -> string
(** Render the [Range] header value. *)

val view : t -> view
(** Return the structured range shape. *)

module Content_range : sig
  type t = {
    start : int64;
    finish : int64;
    complete_length : int64 option;
        (** [None] when S3 returned an unknown complete length ([*]). *)
  }
  (** Parsed [Content-Range] response header for successful ranged reads. *)

  val of_header : string -> (t, Awskit.Error.t) result
  (** Parse a [Content-Range] response header such as [bytes 2-5/10]. *)

  val to_header : t -> string
  (** Render a [Content-Range] response header value. *)
end
