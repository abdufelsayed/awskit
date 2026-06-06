module Make (C : Request_context.S) :
  Awskit_s3_intf.PRESIGNED
    with type connection = C.connection
     and type 'a io = 'a C.io
