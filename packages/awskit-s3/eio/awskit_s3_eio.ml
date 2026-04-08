type t = Awskit_eio.t

module Runtime = Awskit_eio.Runtime

let create ~env ~region ~credentials ?clock ?endpoint ?max_response_body_bytes
    () =
  Awskit_eio.create ~env ~region ~credentials ?clock ?endpoint
    ?max_response_body_bytes ()

include Awskit_s3.Make (Awskit_eio.Runtime)
