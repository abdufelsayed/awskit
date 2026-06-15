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
