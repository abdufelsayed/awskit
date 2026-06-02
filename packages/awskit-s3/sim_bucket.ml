open Core
open Sim_support

module Bucket = struct
  type connection = t
  type 'a io = 'a

  let create conn ~bucket ?options:_ () =
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () ->
        if Hashtbl.mem conn.store.buckets bucket then
          Error (service ~status:409 ~code:"BucketAlreadyOwnedByYou" ())
        else begin
          Hashtbl.add conn.store.buckets bucket
            {
              created_at = now conn;
              objects = Hashtbl.create 17;
              versions = Hashtbl.create 17;
              multipart_uploads = Hashtbl.create 17;
              policy = None;
              bucket_tags = [];
              versioning = None;
              encryption = None;
              cors = None;
              public_access_block = None;
              ownership_controls = None;
            };
          Ok { Create_bucket.response = response 200 }
        end

  let delete ?expected_bucket_owner:_ conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok state ->
        if Hashtbl.length state.objects > 0 then
          Error (service ~status:409 ~code:"BucketNotEmpty" ())
        else begin
          Hashtbl.remove conn.store.buckets bucket;
          Ok { Delete_bucket.response = response 204 }
        end

  let head ?expected_bucket_owner:_ conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ ->
        Ok
          {
            Head_bucket.name = bucket;
            region = Some (Runtime.region conn);
            response = response 200;
          }

  let exists ?expected_bucket_owner conn ~bucket =
    match head ?expected_bucket_owner conn ~bucket with
    | Ok _ -> Ok true
    | Error error when Error.is_not_found error -> Ok false
    | Error error -> Error error

  let list conn =
    let buckets =
      Hashtbl.to_seq conn.store.buckets
      |> Seq.map (fun (name, state) ->
          { Bucket.name; creation_date = Some state.created_at })
      |> List.of_seq
      |> List.sort (fun (a : Bucket.info) b -> String.compare a.name b.name)
    in
    Ok buckets

  let get_location ?expected_bucket_owner:_ conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ -> Ok (Some (Runtime.region conn))

  module Policy = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.policy with
          | Some p -> Ok p
          | None -> Error (no_such_key ()))

    let put conn ~bucket ?expected_bucket_owner:_ policy =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- Some policy;
          Ok (response 200)

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- None;
          Ok (response 204)
  end

  module Versioning = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket.Versioning.status = state.versioning;
              response = response 200;
            }

    let put conn ~bucket ?expected_bucket_owner:_ status =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.versioning <- Some status;
          Ok (response 200)
  end

  module Tagging = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            { Bucket.Tagging.tags = state.bucket_tags; response = response 200 }

    let put conn ~bucket ?expected_bucket_owner:_ tags =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match validate_tags tags with
          | Error error -> Error error
          | Ok () ->
              state.bucket_tags <- tags;
              Ok (response 200))

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.bucket_tags <- [];
          Ok (response 204)
  end

  module Encryption = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.encryption with
          | None ->
              Error
                (service ~status:404
                   ~code:"ServerSideEncryptionConfigurationNotFoundError" ())
          | Some config ->
              Ok { Bucket.Encryption.config; response = response 200 })

    let put conn ~bucket ?expected_bucket_owner:_ config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- Some config;
          Ok (response 200)

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- None;
          Ok (response 204)
  end

  module Cors = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.cors with
          | None -> Error (no_such_key ())
          | Some config -> Ok { Bucket.Cors.config; response = response 200 })

    let put conn ~bucket ?expected_bucket_owner:_ config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- Some config;
          Ok (response 200)

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- None;
          Ok (response 204)
  end

  module Public_access_block = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.public_access_block with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok { Bucket.Public_access_block.config; response = response 200 })

    let put conn ~bucket ?expected_bucket_owner:_ config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- Some config;
          Ok (response 200)

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- None;
          Ok (response 204)
  end

  module Ownership_controls = struct
    let get ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.ownership_controls with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok { Bucket.Ownership_controls.config; response = response 200 })

    let put conn ~bucket ?expected_bucket_owner:_ config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.ownership_controls <- Some config;
          Ok (response 200)

    let delete ?expected_bucket_owner:_ conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.ownership_controls <- None;
          Ok (response 204)
  end
end
