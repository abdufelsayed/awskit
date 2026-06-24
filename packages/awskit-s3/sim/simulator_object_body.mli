(** Internal object-body helpers for simulator reads and writes. *)

val ranged_body :
  string ->
  Awskit_s3.Range.t option ->
  (string * int * (string * string) list, Awskit.Error.t) result
(** Select a full or ranged body and return the body, status code, and response
    headers. *)

val request_body_result :
  Simulator_runtime.Runtime.request_body -> (string, Awskit.Error.t) result
(** Materialize a simulator request body as a string. *)
