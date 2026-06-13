(** Runtime-backed presigned URL helper functor.

    Most callers use this through {!module:Awskit_s3.Make} or an adapter
    package. *)

(** Build presigned URL helpers over a request context. *)
module Make (C : Request_context.S) :
  Awskit_s3_intf.PRESIGNED
    with type connection = C.connection
     and type 'a io = 'a C.io
