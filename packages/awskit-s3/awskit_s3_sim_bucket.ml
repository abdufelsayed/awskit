open Awskit_s3_core
open Awskit_s3_sim_support

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
              policy = None;
              bucket_tags = [];
              versioning = None;
              encryption = None;
              cors = None;
              website = None;
              public_access_block = None;
              ownership_controls = None;
              request_payment = None;
              accelerate = None;
              logging = Bucket.Logging.disabled;
            };
          Ok { Bucket.Create.request = response 200 }
        end

  let delete conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok state ->
        if Hashtbl.length state.objects > 0 then
          Error (service ~status:409 ~code:"BucketNotEmpty" ())
        else begin
          Hashtbl.remove conn.store.buckets bucket;
          Ok { Bucket.Delete.request = response 204 }
        end

  let head conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ ->
        Ok
          {
            Bucket.Head.name = bucket;
            region = Some (Runtime.region conn);
            request = response 200;
          }

  let exists conn ~bucket =
    match head conn ~bucket with
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

  let get_location conn ~bucket =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ -> Ok (Some (Runtime.region conn))

  module Policy = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.policy with
          | Some p -> Ok p
          | None -> Error (no_such_key ()))

    let put conn ~bucket policy =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- Some policy;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- None;
          Ok (response 204)
  end

  module Policy_status = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok _ ->
          Ok
            {
              Bucket.Policy_status.is_public = Some false;
              request = response 200;
            }
  end

  module Versioning = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket.Versioning.status = state.versioning;
              request = response 200;
            }

    let put conn ~bucket status =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.versioning <- Some status;
          Ok (response 200)
  end

  module Tagging = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok { Bucket.Tagging.tags = state.bucket_tags; request = response 200 }

    let put conn ~bucket tags =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match validate_tags tags with
          | Error error -> Error error
          | Ok () ->
              state.bucket_tags <- tags;
              Ok (response 200))

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.bucket_tags <- [];
          Ok (response 204)
  end

  module Encryption = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.encryption with
          | None ->
              Error
                (service ~status:404
                   ~code:"ServerSideEncryptionConfigurationNotFoundError" ())
          | Some config ->
              Ok { Bucket.Encryption.config; request = response 200 })

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- Some config;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- None;
          Ok (response 204)
  end

  module Cors = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.cors with
          | None -> Error (no_such_key ())
          | Some config -> Ok { Bucket.Cors.config; request = response 200 })

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- Some config;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- None;
          Ok (response 204)
  end

  module Website = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.website with
          | None -> Error (no_such_key ())
          | Some config -> Ok { Bucket.Website.config; request = response 200 })

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.website <- Some config;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.website <- None;
          Ok (response 204)
  end

  module Public_access_block = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.public_access_block with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok { Bucket.Public_access_block.config; request = response 200 })

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- Some config;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- None;
          Ok (response 204)
  end

  module Ownership_controls = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.ownership_controls with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok { Bucket.Ownership_controls.config; request = response 200 })

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.ownership_controls <- Some config;
          Ok (response 200)

    let delete conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.ownership_controls <- None;
          Ok (response 204)
  end

  module Request_payment = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket.Request_payment.payer = state.request_payment;
              request = response 200;
            }

    let put conn ~bucket payer =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.request_payment <- Some payer;
          Ok (response 200)
  end

  module Accelerate = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket.Accelerate.status = state.accelerate;
              request = response 200;
            }

    let put conn ~bucket status =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.accelerate <- Some status;
          Ok (response 200)
  end

  module Logging = struct
    let get conn ~bucket =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok { Bucket.Logging.config = state.logging; request = response 200 }

    let put conn ~bucket config =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.logging <- config;
          Ok (response 200)
  end
end
