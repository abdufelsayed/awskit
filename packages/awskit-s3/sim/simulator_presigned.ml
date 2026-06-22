open Awskit_s3
open Simulator_support
open Simulator_state
open Simulator_runtime

module Presigned = struct
  type connection = t
  type 'a io = 'a

  let get_object conn ~bucket ~key ?options () =
    Presigned.get_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let put_object conn ~bucket ~key ?options () =
    Presigned.put_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let head_object conn ~bucket ~key ?options () =
    Presigned.head_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let delete_object conn ~bucket ~key ?options () =
    Presigned.delete_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let upload_part conn ~bucket ~key ~upload_id ~part_number ?options () =
    Presigned.upload_part_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ~upload_id ~part_number ?options ()
end
