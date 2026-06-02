open Core

module Make (C : Request_context.S) :
  BUCKET with type connection = C.connection and type 'a io = 'a C.io
