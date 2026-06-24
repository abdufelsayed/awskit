open Simulator_support
open Simulator_state
open Simulator_runtime
module Presigned_model = Awskit_s3.Presigned

module Presigned = struct
  type connection = t
  type 'a io = 'a

  let get_object conn ~bucket ~key ?options () =
    Presigned_model.get_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let put_object conn ~bucket ~key ?options () =
    Presigned_model.put_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let head_object conn ~bucket ~key ?options () =
    Presigned_model.head_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let delete_object conn ~bucket ~key ?options () =
    Presigned_model.delete_object_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~bucket ~key ?options ()

  let upload_part conn ~upload ~part_number ?options () =
    Presigned_model.upload_part_with_endpoint_config
      ~region:(Runtime.Endpoint.region conn)
      ~credentials:(credentials conn) ~now:(now conn)
      ~endpoint_config:(Runtime.S3_endpoint.s3_endpoint_config conn)
      ~upload ~part_number ?options ()
end
