(** Eio adapter. [Awskit_s3.Make(Awskit_eio.Runtime)].

    {[
    open Eio.Std

    let () =
      Eio_main.run @@ fun env ->
      let conn =
        Awskit_s3_eio.create ~env ~region:"us-east-1"
          ~credentials:
            (Awskit_s3.Credentials.make ~access_key_id:"..."
               ~secret_access_key:"..." ())
          ~endpoint:(Awskit_s3.Endpoint.http ~host:"localhost" ~port:9000 ())
          ()
      in
      let result =
        Awskit_s3_eio.Object.put conn ~bucket:"my-bucket" ~key:"hello.txt"
          "Hello, S3!"
      in
      match result with
      | Ok _ -> ()
      | Error err -> Fmt.epr "Error: %a" Awskit_s3.Error.pp err
    ]} *)

type t
(** S3 connection handle. *)

(** Underlying runtime module. *)
module Runtime : Awskit.Runtime.S with type 'a t = 'a and type connection = t

val create :
  env:< net : _ Eio.Net.t ; .. > ->
  region:string ->
  credentials:Awskit_s3.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?endpoint:Awskit_s3.Endpoint.t ->
  ?max_response_body_bytes:int ->
  unit ->
  t
(** Create an S3 connection without referencing [Awskit_eio] directly. *)

include module type of Awskit_s3.Make (Runtime)
