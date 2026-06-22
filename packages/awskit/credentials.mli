(** AWS credentials.

    Values are opaque to prevent accidental logging of secret access keys. *)

type t
(** Opaque AWS credentials. The secret access key is retained for signing but is
    not exposed by accessors. *)

type source =
  [ `Static
  | `Env
  | `Shared_file of string
  | `Config_file of string
  | `Container
  | `Imds
  | `Custom of string ]
(** Credential source that produced, skipped, or failed resolution. *)

module Provider : sig
  type credentials = t
  (** AWS credentials resolved by a provider. *)

  type nonrec source = source
  (** Credential source that produced, skipped, or failed resolution. *)

  type unavailable = { source : source; reason : string }
  (** Provider was not configured or not applicable, so a chain may continue. *)

  (** Credential resolution outcome.

      [Unavailable] means the provider is not configured and a chain should try
      the next provider. [Invalid] means configured credential material is
      malformed or unsupported. [Failed] means an applicable provider could not
      complete lookup. Chains stop on every outcome except [Unavailable]. *)
  type resolution =
    | Resolved of credentials
    | Unavailable of unavailable
    | Invalid of Error.t
    | Failed of Error.t

  type t
  (** Lazy credential provider used by runtime adapters that can refresh or
      retry credential lookup for each signed request. *)

  val create : (unit -> resolution) -> t
  (** Wrap a credential lookup function. The function is called each time
      {!val:resolve} is called. *)

  val resolve : t -> resolution
  (** Resolve credentials from the provider. *)

  val static : credentials -> t
  (** Provider that always returns the same credentials. *)

  val chain : t list -> t
  (** Try providers in order. The chain continues only when a provider returns
      [Unavailable]; [Resolved], [Invalid], and [Failed] stop resolution. *)

  val source_label : source -> string
  (** Stable, human-readable label for a credential provider source. *)
end

val create :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  ?source:Provider.source ->
  ?expires_at:Ptime.t ->
  unit ->
  (t, Error.t) result
(** Create credentials from AWS access key material.

    Empty access keys, empty secret keys, and invalid session token values are
    rejected with a validation error. Optional [source] and [expires_at]
    metadata are preserved for provider chains, refresh, and telemetry. *)

val create_exn :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  ?source:Provider.source ->
  ?expires_at:Ptime.t ->
  unit ->
  t
(** Like {!val:create}, but raises [Error.Awskit_error] carrying the structured
    validation error on validation failure. *)

val access_key_id : t -> string
(** Return the non-secret access key id. *)

val session_token : t -> string option
(** Return the optional session token for temporary credentials. *)

val source : t -> Provider.source option
(** Return the credential source metadata, if the provider supplied one. *)

val source_label : t -> string option
(** Return a stable, human-readable source label, if source metadata exists. *)

val expires_at : t -> Ptime.t option
(** Return the credential expiration timestamp, if the provider supplied one. *)

val validate_fresh : t -> now:Ptime.t -> (unit, Error.t) result
(** Return [Ok ()] when credentials are still usable at [now].

    Credentials without expiration metadata are treated as fresh. Expired
    credentials return a credentials error with source metadata when available.
*)

val signing_key :
  t -> datestamp:string -> region:Region.t -> service:string -> string
(** Derive a SigV4 signing key without exposing the raw secret access key. This
    is primarily for {!Awskit.Signing} and custom AWS signers. *)
