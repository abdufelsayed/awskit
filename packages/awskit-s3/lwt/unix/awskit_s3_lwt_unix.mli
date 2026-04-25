(** Ready-to-use Lwt + Unix adapter. Thin wrapper over
    [Awskit_s3_lwt.Make(Cohttp_lwt_unix.Client)].

    {[
    let conn =
      Awskit_s3_lwt_unix.create ~region:"us-east-1"
        ~credentials:
          (Awskit_s3.Credentials.make ~access_key_id:"..."
             ~secret_access_key:"..." ())
        ~endpoint:(Awskit_s3.Endpoint.http ~host:"localhost" ~port:9000 ())
        ()
      |> Result.get_ok
    in
    let* result =
      Awskit_s3_lwt_unix.Object.get conn ~bucket:"my-bucket" ~key:"hello.txt" ()
    in
    match result with
    | Ok _ -> Lwt.return_unit
    | Error err ->
        Fmt.epr "Error: %a" Awskit_s3.Error.pp err;
        Lwt.return_unit
    ]} *)

type t
(** S3 connection handle. *)

(** Underlying runtime module. *)
module Runtime :
  Awskit.Runtime.S with type 'a t = 'a Lwt.t and type connection = t

val create :
  ?ctx:Cohttp_lwt_unix.Client.ctx ->
  ?endpoint:Awskit_s3.Endpoint.t ->
  ?region:string ->
  ?credentials:Awskit_s3.Credentials.t ->
  ?clock:(unit -> Ptime.t) ->
  ?max_response_body_bytes:int ->
  unit ->
  (t, Awskit_s3.Error.t) result
(** Create an S3 connection without referencing [Awskit_lwt_unix] directly.

    If [region] or [credentials] are omitted, standard AWS environment variables
    are resolved by [awskit.unix]. *)

include module type of Awskit_s3.Make (Runtime)
