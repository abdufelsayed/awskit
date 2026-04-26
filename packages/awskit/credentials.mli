(** AWS credentials. Opaque — prevents accidental logging of secret keys.

    {[
    Awskit.Credentials.make ~access_key_id:"AKIAIOSFODNN7EXAMPLE"
      ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY" ()
      (* Temporary (STS/role) — session_token triggers x-amz-security-token *)
      Awskit.Credentials.make ~access_key_id:"ASIATESTSESSIONTOKEN"
      ~secret_access_key:"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      ~session_token:"FwoGZXIvYXdzEBY..." ()
    ]} *)

type t

val make :
  access_key_id:string ->
  secret_access_key:string ->
  ?session_token:string ->
  unit ->
  t
(** Create credentials. [session_token] for STS/role temporary credentials.
    Raises [Invalid_argument] if required fields are empty, contain control
    characters, or have leading/trailing whitespace. *)

val access_key_id : t -> string
val session_token : t -> string option

val signing_key :
  t -> datestamp:string -> region:string -> service:string -> string
(** Derive a SigV4 signing key without exposing the raw secret access key. This
    is primarily for {!Awskit.Signing} and custom AWS signers. *)
