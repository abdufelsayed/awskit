(** Mutable state owned by one logical S3 service-operation invocation.

    The session deliberately contains no observation values. It is updated by
    the execution kernel and read by the optional observation projection only
    when the operation closes. *)

type t = {
  operation : Operation.t;
  mutable attempts : int;
  mutable logical_request_bytes : int64 option;
  mutable logical_response_bytes_candidate : int64 option;
  mutable logical_response_bytes : int64 option;
}

let create operation =
  {
    operation;
    attempts = 0;
    logical_request_bytes = None;
    logical_response_bytes_candidate = None;
    logical_response_bytes = None;
  }

let operation t = t.operation
let attempts t = t.attempts
let record_attempt t = t.attempts <- t.attempts + 1
let set_logical_request_bytes t bytes = t.logical_request_bytes <- bytes
let logical_request_bytes t = t.logical_request_bytes

let set_logical_response_bytes t bytes =
  t.logical_response_bytes_candidate <- bytes

let commit_logical_response_bytes t =
  t.logical_response_bytes <- t.logical_response_bytes_candidate

let logical_response_bytes t = t.logical_response_bytes
