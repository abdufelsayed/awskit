val set_response :
  ?headers:(string * string) list -> status:int -> string -> unit

val set_responses : (int * (string * string) list * string) list -> unit
val set_pending : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t -> unit
val reset_calls : unit -> unit
val call_count : unit -> int

module S3 : sig
  type t
end

val connection :
  ?retry_policy:Awskit.Retry.t ->
  ?observability:Awskit_lwt.Observability.t ->
  unit ->
  S3.t

val get :
  S3.t -> (string Awskit_s3.Object.Get.result, Awskit.Error.t) result Lwt.t

val get_value : S3.t -> (string, Awskit.Error.t) result Lwt.t

val put_string :
  S3.t -> string -> (Awskit_s3.Object.Put.result, Awskit.Error.t) result Lwt.t

val presign_get :
  S3.t -> (Awskit_s3.Presigned.result, Awskit.Error.t) result Lwt.t
