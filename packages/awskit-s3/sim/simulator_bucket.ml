open Simulator_support
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_runtime
module Bucket_model = Awskit_s3.Bucket
module Bucket_name = Awskit_s3.Bucket_name
module Error = Awskit_s3.Error
module Tag = Awskit_s3.Tag

module Bucket = struct
  type connection = t
  type 'a io = 'a

  let create conn ~bucket ?options:_ () =
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () ->
        let state_store = store conn in
        if bucket_exists state_store bucket then
          Error (service ~status:409 ~code:"BucketAlreadyOwnedByYou" ())
        else begin
          add_bucket state_store bucket
            {
              created_at = now conn;
              objects = Hashtbl.create 17;
              versions = Hashtbl.create 17;
              multipart_uploads = Hashtbl.create 17;
              policy = None;
              bucket_tags = Tag.Set.empty;
              versioning = None;
              encryption = None;
              cors = None;
              public_access_block = None;
              ownership_controls = None;
            };
          Ok { Bucket_model.Create.response = response 200 }
        end

  let delete conn ~bucket ?options:_ () =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok state ->
        if Hashtbl.length state.objects > 0 then
          Error (service ~status:409 ~code:"BucketNotEmpty" ())
        else begin
          remove_bucket (store conn) bucket;
          Ok { Bucket_model.Delete.response = response 204 }
        end

  let head conn ~bucket ?options:_ () =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ ->
        Ok
          {
            Bucket_model.Head.name = Bucket_name.of_string_exn bucket;
            region = Some (Runtime.Endpoint.region conn);
            response = response 200;
          }

  let exists conn ~bucket ?options () =
    match head conn ~bucket ?options () with
    | Ok _ -> Ok true
    | Error error when Error.is_not_found error -> Ok false
    | Error error -> Error error

  let list conn =
    let buckets =
      buckets (store conn)
      |> List.map (fun (name, state) ->
          {
            Bucket_model.name = Bucket_name.of_string_exn name;
            creation_date = Some state.created_at;
          })
    in
    Ok { Bucket_model.List_buckets.buckets; response = response 200 }

  let get_location conn ~bucket ?options:_ () =
    match require_bucket conn bucket with
    | Error error -> Error error
    | Ok _ ->
        Ok
          {
            Bucket_model.Get_location.region = Runtime.Endpoint.region conn;
            response = response 200;
          }

  module Policy = struct
    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.policy with
          | Some p -> Ok p
          | None -> Error (no_such_key ()))

    let put conn ~bucket ?options:_ ~policy () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- Some policy;
          Ok (response 200)

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.policy <- None;
          Ok (response 204)
  end

  module Versioning = struct
    let validate_status = function
      | Bucket_model.Versioning.Status.Enabled | Suspended -> Ok ()
      | Unknown value ->
          invalid ~field:"status"
            "unknown bucket versioning status %S cannot be sent" value

    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket_model.Versioning.status = state.versioning;
              response = response 200;
            }

    let put conn ~bucket ?options:_ ~status () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match validate_status status with
          | Error error -> Error error
          | Ok () ->
              set_versioning state status;
              Ok (response 200))
  end

  module Tagging = struct
    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          Ok
            {
              Bucket_model.Tagging.tags = state.bucket_tags;
              response = response 200;
            }

    let put conn ~bucket ?options:_ ~tags () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match validate_tags tags with
          | Error error -> Error error
          | Ok () ->
              state.bucket_tags <- tags;
              Ok (response 200))

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.bucket_tags <- Tag.Set.empty;
          Ok (response 204)
  end

  module Encryption = struct
    let observe_default = function
      | Bucket_model.Encryption.Default_encryption.Sse_s3 ->
          ( {
              Bucket_model.Encryption.Observed.algorithm =
                Some Bucket_model.Encryption.Observed.Algorithm.Aes256;
              kms_key_id = None;
            },
            None )
      | Sse_kms { key_id; bucket_key_enabled } ->
          ( {
              Bucket_model.Encryption.Observed.algorithm =
                Some Bucket_model.Encryption.Observed.Algorithm.Aws_kms;
              kms_key_id = key_id;
            },
            bucket_key_enabled )
      | Dsse_kms { key_id } ->
          ( {
              Bucket_model.Encryption.Observed.algorithm =
                Some Bucket_model.Encryption.Observed.Algorithm.Aws_kms_dsse;
              kms_key_id = key_id;
            },
            None )

    let observe_sse_c_policy = function
      | Bucket_model.Encryption.Sse_c_policy.Allow ->
          Bucket_model.Encryption.Observed.Sse_c_policy.Allow
      | Block -> Bucket_model.Encryption.Observed.Sse_c_policy.Block

    let observe_rule rule =
      let default, sse_c_policy =
        match rule with
        | Bucket_model.Encryption.Rule.Default default -> (Some default, None)
        | Sse_c policy -> (None, Some policy)
        | Default_and_sse_c { default_encryption; sse_c_policy } ->
            (Some default_encryption, Some sse_c_policy)
      in
      let default_encryption, bucket_key_enabled =
        match default with
        | None -> (None, None)
        | Some default ->
            let observed, bucket_key_enabled = observe_default default in
            (Some observed, bucket_key_enabled)
      in
      let sse_c_policies =
        match sse_c_policy with
        | None -> []
        | Some policy -> [ observe_sse_c_policy policy ]
      in
      {
        Bucket_model.Encryption.Observed.default_encryption;
        bucket_key_enabled;
        sse_c_policies;
      }

    let observe_config (config : Bucket_model.Encryption.Config.t) =
      {
        Bucket_model.Encryption.Observed.rules =
          List.map observe_rule config.rules;
      }

    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.encryption with
          | None ->
              Error
                (service ~status:404
                   ~code:"ServerSideEncryptionConfigurationNotFoundError" ())
          | Some config ->
              Ok { Bucket_model.Encryption.config; response = response 200 })

    let put conn ~bucket ?options:_ ~config () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- Some (observe_config config);
          Ok (response 200)

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.encryption <- None;
          Ok (response 204)
  end

  module Cors = struct
    let observe_rule (rule : Bucket_model.Cors.Rule.t) =
      {
        Bucket_model.Cors.Observed.id = rule.id;
        allowed_origins = rule.allowed_origins;
        allowed_methods =
          List.map
            (fun method_ -> Bucket_model.Cors.Method.Known method_)
            rule.allowed_methods;
        allowed_headers = rule.allowed_headers;
        expose_headers = rule.expose_headers;
        max_age_seconds = rule.max_age_seconds;
      }

    let observe_config (config : Bucket_model.Cors.Config.t) =
      { Bucket_model.Cors.Observed.rules = List.map observe_rule config.rules }

    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.cors with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok { Bucket_model.Cors.config; response = response 200 })

    let put conn ~bucket ?options:_ ~config () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- Some (observe_config config);
          Ok (response 200)

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.cors <- None;
          Ok (response 204)
  end

  module Public_access_block = struct
    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.public_access_block with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok
                {
                  Bucket_model.Public_access_block.config;
                  response = response 200;
                })

    let put conn ~bucket ?options:_ ~config () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- Some config;
          Ok (response 200)

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.public_access_block <- None;
          Ok (response 204)
  end

  module Ownership_controls = struct
    let validate_config (config : Bucket_model.Ownership_controls.config) =
      match config.object_ownership with
      | Bucket_owner_enforced | Bucket_owner_preferred | Object_writer -> Ok ()
      | Unknown value ->
          invalid ~field:"object_ownership"
            "unknown object ownership %S cannot be sent" value

    let get conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match state.ownership_controls with
          | None -> Error (no_such_key ())
          | Some config ->
              Ok
                {
                  Bucket_model.Ownership_controls.config;
                  response = response 200;
                })

    let put conn ~bucket ?options:_ ~config () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state -> (
          match validate_config config with
          | Error error -> Error error
          | Ok () ->
              state.ownership_controls <- Some config;
              Ok (response 200))

    let delete conn ~bucket ?options:_ () =
      match require_bucket conn bucket with
      | Error error -> Error error
      | Ok state ->
          state.ownership_controls <- None;
          Ok (response 204)
  end
end
