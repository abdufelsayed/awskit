open Common

type t = string

let of_json body =
  try
    match Yojson.Safe.from_string body with
    | `Assoc _ -> Ok body
    | _ -> invalid ~field:"policy" "policy must be a JSON object"
  with Yojson.Json_error message -> Error (Awskit.Error.decode message)

let to_json policy = policy
