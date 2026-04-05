open Base

type t =
  [ Awskit.Error.base
  | `Not_found
  | `Precondition_failed
  | `Bucket_already_exists
  | `Bucket_not_empty
  | `No_such_bucket ]
[@@deriving show, eq]

module Service_error = struct
  let code_of_body body =
    try
      let _, nodes = Ezxmlm.from_string body in
      let _, data = Ezxmlm.member_with_attr "Code" nodes in
      Some (Ezxmlm.data_to_string data)
    with exn ->
      ignore exn;
      None
end

let of_aws_error (error : Awskit.Error.base) : t = (error :> t)

let of_status status body : t =
  match (status, Service_error.code_of_body body) with
  | 403, _ -> `Access_denied
  | 404, Some "NoSuchBucket" -> `No_such_bucket
  | 404, _ -> `Not_found
  | 409, (Some "BucketAlreadyOwnedByYou" | Some "BucketAlreadyExists") ->
      `Bucket_already_exists
  | 409, Some "BucketNotEmpty" -> `Bucket_not_empty
  | 409, _ -> `Request_error (status, body)
  | 412, _ -> `Precondition_failed
  | 503, _ -> `Service_unavailable body
  | _, _ -> `Request_error (status, body)
