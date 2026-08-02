(** S3-owned observability identity, Logs sources, and narrow sibling-package
    roles. Applications configure delivery through a runtime observer. *)

module Sources : sig
  (** Domain-specific Logs sources. *)

  val operation : Logs.src
  val attempt : Logs.src
  val signing : Logs.src
  val artifact : Logs.src
  val artifact_signing : Logs.src
  val retry : Logs.src
  val transfer : Logs.src
end

module For_transfer : sig
  (** Narrow role used by the high-level transfer runtime packages. *)

  type summary = { logical_bytes : int64; parts : int }

  module Make (Runtime : Awskit.Observability.For_service.Observer) : sig
    val with_upload :
      Runtime.connection ->
      summarize:('a -> summary) ->
      (unit -> ('a, Awskit.Error.t) Result.t Runtime.io) ->
      ('a, Awskit.Error.t) Result.t Runtime.io

    val with_download :
      Runtime.connection ->
      summarize:('a -> summary) ->
      (unit -> ('a, Awskit.Error.t) Result.t Runtime.io) ->
      ('a, Awskit.Error.t) Result.t Runtime.io
  end
end

module For_simulator : sig
  (** Narrow constructor used by the simulator to reproduce the real logical
      completion shape without fabricating physical attempts. *)

  val complete :
    operation:Operation.t ->
    outcome:Awskit.Observability.Outcome.t ->
    ?retry_class:Awskit.Error.retry_class ->
    ?logical_request_bytes:int64 ->
    ?logical_response_bytes:int64 ->
    ?region:string ->
    ?bucket:string ->
    unit ->
    Awskit.Observability.For_projection.Operation.Completion.t

  val complete_artifact :
    operation:Artifact_operation.t ->
    outcome:Awskit.Observability.Outcome.t ->
    ?region:string ->
    ?bucket:string ->
    unit ->
    Awskit.Observability.For_projection.Operation.Completion.t
  (** Pure completion for one connection-bound presigned-artifact operation.

      The value contains only the bounded artifact identity and outcome; it has
      no HTTP-attempt, status, connector-byte, or bearer-artifact fields. *)

  val complete_artifact_signing :
    operation:Artifact_operation.t ->
    outcome:Awskit.Observability.Outcome.t ->
    unit ->
    Awskit.Observability.For_projection.Operation.Completion.t
  (** Pure completion for the signing child of one presigned artifact. *)

  val complete_credential_resolution :
    credentials:Awskit.Credentials.t ->
    unit ->
    Awskit.Observability.For_projection.Operation.Completion.t
  (** Pure successful credential-resolution completion for simulator presigning.

      Only the reviewed credential-source dimension can be projected; secret
      credential material is not part of the returned value. *)
end
