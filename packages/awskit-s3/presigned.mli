(** Presigned URL generation. Pure — no IO.

    URLs embed a SigV4 signature that expires after a configurable time. Default
    expiry: 3600s (1 hour). Max: 604800s (7 days).

    {[
    (* Download URL *)
    Presigned.get_object ~region:"us-east-1" ~credentials
      ~now:(Ptime_clock.now ()) ~host:"s3.us-east-1.amazonaws.com"
      ~bucket:"my-bucket" ~key:"report.pdf" ()
      (* Upload URL, 5 minutes *)
      Presigned.put_object ~region:"us-east-1" ~credentials
      ~now:(Ptime_clock.now ()) ~host:"s3.us-east-1.amazonaws.com"
      ~bucket:"uploads" ~key:"user-file.csv" ~expires_in:300
      ~content_type:"text/csv" ()
    ]} *)

val get_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  host:string ->
  ?port:int ->
  bucket:string ->
  key:string ->
  ?expires_in:int ->
  unit ->
  (string, [> `Invalid_request of string ]) result
(** [port] implies HTTP scheme. [expires_in]: default 3600, max 604800. *)

val put_object :
  region:string ->
  credentials:Awskit.Credentials.t ->
  now:Ptime.t ->
  host:string ->
  ?port:int ->
  bucket:string ->
  key:string ->
  ?expires_in:int ->
  ?content_type:string ->
  unit ->
  (string, [> `Invalid_request of string ]) result
(** [content_type] is baked into the URL as a required header for the upload. *)
