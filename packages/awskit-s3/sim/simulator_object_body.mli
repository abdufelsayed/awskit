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

val consume_string :
  max_bytes:int64 ->
  Simulator_runtime.Runtime.response_body_reader ->
  (string, Awskit.Error.t) result
(** Consume a simulator response body as a bounded string. *)

val consume_bytes :
  max_bytes:int64 ->
  Simulator_runtime.Runtime.response_body_reader ->
  (bytes, Awskit.Error.t) result
(** Consume a simulator response body as bounded bytes. *)
