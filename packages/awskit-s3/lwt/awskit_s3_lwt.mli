(** Lwt S3 adapter functor.

    This package exposes streaming primitive operations and in-memory buffer
    helpers. Unix local-path helpers are provided by [awskit-s3-lwt-unix]. *)

module Make (Client : Cohttp_lwt.S.Client) : sig
  (** Build an S3 adapter over a caller-supplied Cohttp Lwt client. *)
  type t
  (** Configured S3 client for the supplied Cohttp client. *)

  val create :
    ?ctx:Client.ctx ->
    ?endpoint_config:Awskit_s3.Endpoint_config.t ->
    region:string ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    ?random_float:(upper_bound:float -> float) ->
    ?timeout_policy:Awskit.Timeout.policy ->
    ?max_response_drain_bytes:int ->
    unit ->
    (t, Awskit.Error.t) result
  (** Create a generic Lwt S3 client.

      Use this when the application owns the Cohttp client module/context. The
      ready-to-use Unix variant is [awskit-s3-lwt-unix]. When retries are
      enabled, custom Lwt backends must pass real [sleep] and [random_float]
      capabilities or use [Awskit.Retry.disabled]. Explicit timeout policies
      with configured spans also require [sleep]. Use [endpoint_config] for AWS
      endpoint variants, local S3-compatible tests, or explicit endpoints that
      use S3-compatible signing/addressing rules. [max_response_drain_bytes]
      controls how much response body the runtime drains after successful
      consumers. *)

  module Body : sig
    type t

    include Awskit_s3.BODY with type 'a io := 'a Lwt.t and type t := t

    val of_lwt_stream :
      content_length:int64 -> string Lwt_stream.t -> (t, Awskit.Error.t) result
    (** Build a non-replayable request body from an existing Lwt stream.

        [content_length] must match the bytes yielded by the stream, and the
        stream must remain valid until the request finishes. *)
  end

  module Reader : sig
    type t

    include Awskit_s3.READER with type 'a io := 'a Lwt.t and type t := t
  end

  (** Object operations returning [Lwt.t]. *)
  module Object :
    Awskit_s3.OBJECT
      with type client := t
       and type 'a io := 'a Lwt.t
       and type request_body := Body.t
       and type response_body_reader := Reader.t

  (** Bucket operations returning [Lwt.t]. *)
  module Bucket :
    Awskit_s3.BUCKET with type client := t and type 'a io := 'a Lwt.t

  (** Multipart operations returning [Lwt.t]. *)
  module Multipart :
    Awskit_s3.MULTIPART
      with type client := t
       and type 'a io := 'a Lwt.t
       and type request_body := Body.t

  (** Presigned request artifact helpers returning [Lwt.t]. *)
  module Presigned :
    Awskit_s3.PRESIGNED with type client := t and type 'a io := 'a Lwt.t
end
