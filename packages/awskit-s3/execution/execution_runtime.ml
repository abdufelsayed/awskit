(** Structural runtime capability consumed by the private S3 execution kernel.

    This is intentionally independent of the public [Awskit_s3.RUNTIME] module
    type so the kernel does not depend on the root facade while it is compiled.
    Its shape remains the protocol-only runtime contract plus S3 endpoint
    resolution. *)

module type S = sig
  include Awskit.Runtime.S

  module S3_endpoint : sig
    type nonrec connection = connection

    val s3_endpoint_config : connection -> Endpoint_resolver.t
  end
end
