(** AWS credentials.

    Values are opaque to prevent accidental logging of secret access keys. *)

type t

val create :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  unit ->
  (t, Error.t) result

val create_exn :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  unit ->
  t

val access_key_id : t -> string
val session_token : t -> string option

module Provider : sig
  type credentials = t
  type t

  val create : (unit -> (credentials, Error.t) result) -> t
  val resolve : t -> (credentials, Error.t) result
  val static : credentials -> t
  val chain : t list -> t
end

val signing_key :
  t -> datestamp:string -> region:Region.t -> service:string -> string
(** Derive a SigV4 signing key without exposing the raw secret access key. This
    is primarily for {!Awskit.Signing} and custom AWS signers. *)
