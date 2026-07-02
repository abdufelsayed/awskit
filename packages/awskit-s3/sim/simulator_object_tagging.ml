open Simulator_support
open Simulator_error
open Simulator_store
module Object = Awskit_s3.Object
module Tag = Awskit_s3.Tag

module Tagging = struct
  let with_tagging_operation ~operation ~bucket ~key error =
    S3_error_context.with_s3_operation ~operation ~bucket ~key error

  let tagging_error ~operation ~bucket ~key error =
    Error (with_tagging_operation ~operation ~bucket ~key error)

  let require_tagging_object conn ~operation ~bucket ~key =
    match validate_bucket_key bucket key with
    | Error error -> tagging_error ~operation ~bucket ~key error
    | Ok () -> (
        match require_object conn bucket key with
        | Error error -> tagging_error ~operation ~bucket ~key error
        | Ok obj -> Ok obj)

  let get conn ~bucket ~key ?options:_ () =
    match
      require_tagging_object conn ~operation:"GetObjectTagging" ~bucket ~key
    with
    | Error _ as error -> error
    | Ok obj -> Ok { Object.Tagging.tags = obj.tags; response = response 200 }

  let put conn ~bucket ~key ?options:_ ~tags () =
    let operation = "PutObjectTagging" in
    match validate_bucket_key bucket key with
    | Error error -> tagging_error ~operation ~bucket ~key error
    | Ok () -> (
        match validate_tags tags with
        | Error error -> tagging_error ~operation ~bucket ~key error
        | Ok () -> (
            match require_object conn bucket key with
            | Error error -> tagging_error ~operation ~bucket ~key error
            | Ok obj ->
                obj.tags <- tags;
                Ok (response 200)))

  let delete conn ~bucket ~key ?options:_ () =
    match
      require_tagging_object conn ~operation:"DeleteObjectTagging" ~bucket ~key
    with
    | Error _ as error -> error
    | Ok obj ->
        obj.tags <- Tag.Set.empty;
        Ok (response 204)
end
