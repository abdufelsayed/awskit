(** Runtime-backed S3 bucket request implementation functor.

    Most callers use this through {!module:Awskit_s3.Make} or an adapter
    package. *)

(** Build bucket operations over a request context. *)
module Make (C : Request_context.S) :
  Awskit_s3_intf.BUCKET
    with type connection = C.connection
     and type 'a io = 'a C.io
