(** Runtime-backed S3 object request implementation functor.

    Most callers use this through {!module:Awskit_s3.Make} or an adapter
    package. *)

(** Build object operations over a request context. *)
module Make (C : Request_context.S) :
  Awskit_s3_intf.OBJECT
    with type connection = C.connection
     and type 'a io = 'a C.io
     and type request_body = C.request_body
     and type response_body_reader = C.response_body_reader
