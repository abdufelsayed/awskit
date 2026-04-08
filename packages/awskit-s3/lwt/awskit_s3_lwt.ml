module Make (Client : Cohttp_lwt.S.Client) = struct
  module Aws = Awskit_lwt.Make (Client)

  type t = Aws.t

  module Runtime = Aws.Runtime

  let create ?ctx ?endpoint ~region ~credentials ~clock ?max_response_body_bytes
      () =
    Aws.create ?ctx ?endpoint ~region ~credentials ~clock
      ?max_response_body_bytes ()

  include Awskit_s3.Make (Aws.Runtime)
end
