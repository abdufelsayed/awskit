open Awskit_s3_core
open Awskit_s3_sim_support

module Presigned = struct
  type connection = t
  type 'a io = 'a

  let get_object conn ~bucket ~key ?options () =
    Presigned.get_object ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~provider:(Runtime.s3_provider conn) ~bucket ~key ?options ()

  let put_object conn ~bucket ~key ?options () =
    Presigned.put_object ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~provider:(Runtime.s3_provider conn) ~bucket ~key ?options ()

  let head_object conn ~bucket ~key ?options () =
    Presigned.head_object ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~provider:(Runtime.s3_provider conn) ~bucket ~key ?options ()

  let delete_object conn ~bucket ~key ?expires_in () =
    Presigned.delete_object ~region:(Runtime.region conn)
      ~credentials:conn.credentials ~now:(now conn)
      ~provider:(Runtime.s3_provider conn) ~bucket ~key ?expires_in ()
end
