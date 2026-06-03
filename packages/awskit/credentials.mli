(** AWS credentials.

    Values are opaque to prevent accidental logging of secret access keys. *)

type t
(** Opaque AWS credentials. The secret access key is retained for signing but is
    not exposed by accessors. *)

val create :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  unit ->
  (t, Error.t) result
(** Create credentials from AWS access key material.

    Empty access keys, empty secret keys, and invalid session token values are
    rejected with {!Awskit.Error.Validation}. *)

val create_exn :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  unit ->
  t
(** Like {!val:create}, but raises [Invalid_argument] on validation failure. *)

val access_key_id : t -> string
(** Return the non-secret access key id. *)

val session_token : t -> string option
(** Return the optional session token for temporary credentials. *)

module Provider : sig
  type credentials = t
  (** Lazy credential provider used by runtime adapters that can refresh or
      retry credential lookup for each signed request. *)

  type t

  val create : (unit -> (credentials, Error.t) result) -> t
  (** Wrap a credential lookup function. The function is called each time
      {!val:resolve} is called. *)

  val resolve : t -> (credentials, Error.t) result
  (** Resolve credentials from the provider. *)

  val static : credentials -> t
  (** Provider that always returns the same credentials. *)

  val chain : t list -> t
  (** Try providers in order and return the first successful credentials.
      Validation failures from earlier providers do not prevent later providers
      from being tried. *)
end

val signing_key :
  t -> datestamp:string -> region:Region.t -> service:string -> string
(** Derive a SigV4 signing key without exposing the raw secret access key. This
    is primarily for {!Awskit.Signing} and custom AWS signers. *)
