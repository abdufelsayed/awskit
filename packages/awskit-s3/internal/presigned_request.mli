open Core

module Make (C : Request_context.S) :
  PRESIGNED with type connection = C.connection and type 'a io = 'a C.io
