(** Eio adapter. [Awskit_s3.Make(Awskit_eio.Runtime)].

    {[
    open Eio.Std

    let () =
      Eio_main.run @@ fun env ->
      let conn = Awskit_eio.create ~env ~region:"us-east-1" ~credentials () in
      let result =
        Awskit_s3_eio.Object.put conn ~bucket:"my-bucket" ~key:"hello.txt"
          "Hello, S3!"
      in
      match result with
      | Ok _ -> ()
      | Error err -> Fmt.epr "Error: %a" Awskit_s3.Error.pp err
    ]} *)

include module type of Awskit_s3.Make (Awskit_eio.Runtime)
module Error = Awskit_s3.Error
module Storage_class = Awskit_s3.Storage_class
module Tag = Awskit_s3.Tag
module Range = Awskit_s3.Range
module Metadata = Awskit_s3.Metadata
module Policy = Awskit_s3.Policy
module Sim = Awskit_s3.Sim
