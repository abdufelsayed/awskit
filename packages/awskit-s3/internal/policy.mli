(** Opaque validated bucket-policy JSON payloads. *)

type t

val of_json : string -> (t, Awskit.Error.t) result
val to_json : t -> string
