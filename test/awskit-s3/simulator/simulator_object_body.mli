val ranged_body :
  string ->
  Range.t option ->
  (string * int * (string * string) list, Awskit.Error.t) result

val request_body_result :
  Simulator_runtime.Runtime.request_body -> (string, Awskit.Error.t) result

val consume_string :
  max_bytes:int64 ->
  Simulator_runtime.Runtime.response_body_reader ->
  (string, Awskit.Error.t) result
