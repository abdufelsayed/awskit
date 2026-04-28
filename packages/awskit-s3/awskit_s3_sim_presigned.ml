open Awskit_s3_core
open Awskit_s3_sim_support

module Presigned = struct
  type connection = t
  type 'a io = 'a

  let get_object conn ~bucket ~key ?options () =
    Presigned.get_object_with_endpoint_config ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~endpoint_config:(Runtime.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let put_object conn ~bucket ~key ?options () =
    Presigned.put_object_with_endpoint_config ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~endpoint_config:(Runtime.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let head_object conn ~bucket ~key ?options () =
    Presigned.head_object_with_endpoint_config ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~endpoint_config:(Runtime.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let delete_object conn ~bucket ~key ?expires_in () =
    Presigned.delete_object_with_endpoint_config ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~endpoint_config:(Runtime.s3_endpoint_config conn)
      ~bucket ~key ?expires_in ()
end
