module Make (C : Request_context.S) :
  Awskit_s3_intf.MULTIPART
    with type connection = C.connection
     and type 'a io = 'a C.io
     and type request_body = C.request_body
