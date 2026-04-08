(** Presigned URL generation. Pure — no IO.

    URLs embed a SigV4 signature that expires after a configurable time. Default
    expiry: 3600s (1 hour). Max: 604800s (7 days).

    {[
    let endpoint =
      Awskit_s3.Endpoint.https ~host:"s3.us-east-1.amazonaws.com" ()
    in
    (* Download URL *)
    Presigned.get_object ~region:"us-east-1" ~credentials
      ~now:(Ptime_clock.now ()) ~endpoint ~bucket:"my-bucket" ~key:"report.pdf"
      ()
      (* Upload URL, 5 minutes *)
      Presigned.put_object ~region:"us-east-1" ~credentials
      ~now:(Ptime_clock.now ()) ~endpoint ~bucket:"uploads" ~key:"user-file.csv"
      ~expires_in:300 ~content_type:"text/csv" ()
    ]} *)

val get_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint:Awskit.Endpoint.t ->
  bucket:string ->
  key:string ->
  ?expires_in:int ->
  unit ->
  (string, [> `Invalid_request of string ]) result
(** [expires_in]: default 3600, max 604800. *)

val put_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  endpoint:Awskit.Endpoint.t ->
  bucket:string ->
  key:string ->
  ?expires_in:int ->
  ?content_type:string ->
  unit ->
  (string, [> `Invalid_request of string ]) result
(** [content_type] is signed as a required request header for the upload. *)
