(** Opaque validated bucket-policy JSON payloads. *)

type t
(** Opaque bucket policy JSON. Awskit validates that the payload is
    syntactically JSON but leaves policy semantics to AWS and the application.
*)

val of_json : string -> (t, Awskit.Error.t) result
(** Validate and wrap a JSON policy document. *)

val to_json : t -> string
(** Return the original JSON policy payload. *)
