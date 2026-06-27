open Simulator_support
open Simulator_error
open Simulator_store
module Object = Awskit_s3.Object
module Tag = Awskit_s3.Tag

module Tagging = struct
  let get conn ~bucket ~key ?options:_ () =
    match require_object conn bucket key with
    | Error error -> Error error
    | Ok obj -> Ok { Object.Tagging.tags = obj.tags; response = response 200 }

  let put conn ~bucket ~key ?options:_ ~tags () =
    match require_object conn bucket key with
    | Error error -> Error error
    | Ok obj -> (
        match validate_tags tags with
        | Error error -> Error error
        | Ok () ->
            obj.tags <- tags;
            Ok (response 200))

  let delete conn ~bucket ~key ?options:_ () =
    match require_object conn bucket key with
    | Error error -> Error error
    | Ok obj ->
        obj.tags <- Tag.Set.empty;
        Ok (response 204)
end
