(** Lwt S3 adapter functor.

    This package exposes streaming primitive operations and in-memory buffer
    helpers. Unix local-path helpers are provided by [awskit-s3-lwt-unix]. *)

module Make (Client : Cohttp_lwt.S.Client) : sig
  (** Build an S3 adapter over a caller-supplied Cohttp Lwt client. *)
  type t
  (** S3 connection handle for the supplied Cohttp client. *)

  module Runtime :
    Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t and type connection = t

  val create :
    ?ctx:Client.ctx ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:Awskit_s3.addressing_style ->
    ?endpoint_variant:Awskit_s3.endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    clock:(unit -> Ptime.t) ->
    ?retry_policy:Awskit.Retry.t ->
    ?sleep:(Ptime.Span.t -> unit Lwt.t) ->
    unit ->
    t
  (** Create a generic Lwt S3 client.

      Use this when the application owns the Cohttp client module/context. The
      ready-to-use Unix variant is [awskit-s3-lwt-unix]. [sleep] defaults to a
      no-op unless supplied, so production callers that want retry backoff
      should pass a real sleep function. *)

  (** Object operations returning [Lwt.t]. *)
  module Object :
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type request_body := Runtime.request_body
       and type response_body_reader := Runtime.response_body_reader

  (** Bucket operations returning [Lwt.t]. *)
  module Bucket :
    Awskit_s3.BUCKET with type connection := t and type 'a io := 'a Lwt.t

  (** Multipart operations returning [Lwt.t]. *)
  module Multipart :
    Awskit_s3.MULTIPART
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type request_body := Runtime.request_body

  (** Presigned URL helpers returning [Lwt.t]. *)
  module Presigned :
    Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
end
