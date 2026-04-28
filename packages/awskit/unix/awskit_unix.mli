(** Unix-specific helpers. No network code — use alongside a runtime adapter.

    {[
    let credentials = Awskit_unix.Credentials.from_env () |> Result.get_ok
    let clock = Awskit_unix.Clock.now
    ]} *)

module Clock : sig
  val now : unit -> Ptime.t
  (** Returns the current wall-clock time from the OS. *)
end

module Credentials : sig
  (** Credential loading from AWS environment variables and shared AWS profile
      files. *)

  module Env : sig
    type 'a t = ('a, Awskit.Error.t) Result.t
    (** A result type for environment operations. *)

    type getenv = string -> string option
    (** Environment lookup function used by injectable providers. *)

    module Let_syntax : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    val required : ?getenv:getenv -> string -> string t
    (** Read a required environment variable. Returns an error if missing or
        empty. *)

    val optional : ?getenv:getenv -> string -> string option t
    (** Read an optional environment variable. Returns [None] if unset. *)
  end

  val from_env :
    ?getenv:Env.getenv ->
    unit ->
    (Awskit.Credentials.t, Awskit.Error.t) Result.t
  (** Load AWS credentials from the standard environment variables:
      - [AWS_ACCESS_KEY_ID] (required)
      - [AWS_SECRET_ACCESS_KEY] (required)
      - [AWS_SESSION_TOKEN] (optional, for temporary credentials)

      @return [Ok credentials] or [Error (Awskit.Error.Validation _)] *)

  val from_profile :
    ?getenv:Env.getenv ->
    ?home:string ->
    ?credentials_file:string ->
    ?config_file:string ->
    ?profile:string ->
    unit ->
    (Awskit.Credentials.t, Awskit.Error.t) Result.t
  (** Load static credentials from shared AWS profile files.

      Defaults:
      - profile: [AWS_PROFILE], then ["default"]
      - credentials file: [AWS_SHARED_CREDENTIALS_FILE], then
        [$HOME/.aws/credentials]
      - config file: [AWS_CONFIG_FILE], then [$HOME/.aws/config]

      Both [~/.aws/credentials] sections like [[dev]] and [~/.aws/config]
      sections like [[profile dev]] are supported. Assume-role and other
      non-static profile types are reported as unsupported. *)

  val default_provider :
    ?getenv:Env.getenv -> ?home:string -> unit -> Awskit.Credentials.Provider.t
  (** Default local provider chain. Static environment variables are preferred
      when present; otherwise shared AWS profile files are used. *)

  val default_chain :
    ?getenv:Env.getenv ->
    ?home:string ->
    unit ->
    (Awskit.Credentials.t, Awskit.Error.t) Result.t
  (** Resolve {!val:default_provider}. *)
end

module Region : sig
  (** Environment-based region loading. Reads the standard [AWS_REGION] first,
      then [AWS_DEFAULT_REGION]. *)

  val from_env : unit -> (Awskit.Region.t, Awskit.Error.t) Result.t
  (** Load the AWS region from standard AWS environment variables. *)
end
