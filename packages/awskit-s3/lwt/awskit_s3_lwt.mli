(** Lwt S3 adapter functor.

    This package exposes streaming primitive operations and in-memory buffer
    helpers. Unix local-path helpers are provided by [awskit-s3-lwt-unix]. *)

module Make (Client : Cohttp_lwt.S.Client) : sig
  type t

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

  module Object :
    Awskit_s3.OBJECT
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type upload_body := Runtime.upload_body
       and type download_reader := Runtime.download_reader

  module Bucket :
    Awskit_s3.BUCKET with type connection := t and type 'a io := 'a Lwt.t

  module Multipart :
    Awskit_s3.MULTIPART
      with type connection := t
       and type 'a io := 'a Lwt.t
       and type upload_body := Runtime.upload_body

  module Presigned :
    Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a Lwt.t
end
