(** Lwt + Unix adapter. [Awskit_s3.Make(Awskit_lwt_unix.Runtime)].

    {[
    let conn = Awskit_lwt_unix.create ~region:"us-east-1" ~credentials () in
    let* result =
      Awskit_s3_lwt_unix.Object.get conn ~bucket:"my-bucket" ~key:"hello.txt" ()
    in
    match result with
    | Ok _ -> Lwt.return_unit
    | Error err ->
        Fmt.epr "Error: %a" Awskit_s3.Error.pp err;
        Lwt.return_unit
    ]} *)

include module type of Awskit_s3.Make (Awskit_lwt_unix.Runtime)
module Error = Awskit_s3.Error
module Storage_class = Awskit_s3.Storage_class
module Tag = Awskit_s3.Tag
module Range = Awskit_s3.Range
module Metadata = Awskit_s3.Metadata
module Policy = Awskit_s3.Policy
module Sim = Awskit_s3.Sim
