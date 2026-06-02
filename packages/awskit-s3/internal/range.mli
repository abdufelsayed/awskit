(** HTTP byte-range requests for S3 object reads. *)

type t
type view = Bytes of int64 * int64 | From of int64 | Suffix of int64

val bytes : start:int64 -> finish:int64 -> (t, Awskit.Error.t) result
val bytes_exn : start:int64 -> finish:int64 -> t
val from : int64 -> (t, Awskit.Error.t) result
val from_exn : int64 -> t
val suffix : int64 -> (t, Awskit.Error.t) result
val suffix_exn : int64 -> t
val to_header : t -> string
val view : t -> view
