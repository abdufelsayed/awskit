open Awskit_s3
open Simulator_support
open Simulator_state

let service ?message ?(headers = []) ~status ~code () =
  Awskit.Error.service
    {
      status;
      code = Some code;
      message;
      request_id = None;
      host_id = None;
      headers;
      body = None;
    }

let no_such_bucket () = service ~status:404 ~code:"NoSuchBucket" ()
let no_such_key () = service ~status:404 ~code:"NoSuchKey" ()
let no_such_upload () = service ~status:404 ~code:"NoSuchUpload" ()

let method_not_allowed ?headers () =
  service ?headers ~status:405 ~code:"MethodNotAllowed" ()

let precondition_failed () = service ~status:412 ~code:"PreconditionFailed" ()
let not_modified () = service ~status:304 ~code:"NotModified" ()
let response ?headers status = Awskit.Response.create_exn ~status ?headers ()

let record ?(faulted = false) t op bucket key =
  record_operation ~faulted t op bucket key

let fault_error = function
  | Slow_down -> service ~status:503 ~code:"SlowDown" ()
  | Internal_error -> service ~status:500 ~code:"InternalError" ()
  | Connection_reset ->
      Awskit.Error.transport ~retryable:true "simulated connection reset"
  | Response_lost -> Awskit.Error.body "simulated response body loss"

let take_fault t = Simulator_state.take_fault t

let operation_fault t op bucket key =
  match take_fault t with
  | None ->
      record t op bucket key;
      None
  | Some fault ->
      record ~faulted:true t op bucket key;
      Some (fault_error fault)
