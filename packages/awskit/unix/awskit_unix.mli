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
  (** Environment-based credential loading. Reads the standard
      [AWS_ACCESS_KEY_ID], [AWS_SECRET_ACCESS_KEY], and [AWS_SESSION_TOKEN]
      environment variables. *)

  module Env : sig
    type 'a t = ('a, Awskit.Error.base) Result.t
    (** A result type for environment operations. *)

    module Let_syntax : sig
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    val required : string -> string t
    (** Read a required environment variable. Returns an error if missing or
        empty. *)

    val optional : string -> string option t
    (** Read an optional environment variable. Returns [None] if unset. *)
  end

  val from_env : unit -> (Awskit.Credentials.t, Awskit.Error.base) Result.t
  (** Load AWS credentials from the standard environment variables:
      - [AWS_ACCESS_KEY_ID] (required)
      - [AWS_SECRET_ACCESS_KEY] (required)
      - [AWS_SESSION_TOKEN] (optional, for temporary credentials)

      @return [Ok credentials] or [Error (`Invalid_request msg)] *)
end

module Region : sig
  (** Environment-based region loading. Reads the standard [AWS_REGION] first,
      then [AWS_DEFAULT_REGION]. *)

  val from_env : unit -> (Awskit.Region.t, Awskit.Error.base) Result.t
  (** Load the AWS region from standard AWS environment variables. *)
end
