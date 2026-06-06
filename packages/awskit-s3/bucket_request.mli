module Make (C : Request_context.S) :
  Awskit_s3_intf.BUCKET
    with type connection = C.connection
     and type 'a io = 'a C.io
