module S3_error = Error
open Base
module Credentials = Awskit.Credentials

module Clock = struct
  type t = { mutable now : Ptime.t }

  let default_epoch =
    Ptime.of_date_time ((2026, 1, 1), ((0, 0, 0), 0))
    |> Option.value ~default:Ptime.epoch

  let create ?(now = default_epoch) () = { now }
  let now t = t.now

  let advance t span =
    Ptime.add_span t.now span |> Option.iter ~f:(fun t' -> t.now <- t')

  let advance_ms t ms =
    let secs = max 1 (ms / 1000) in
    advance t (Ptime.Span.of_int_s secs)
end

(* ── Internal object model ──────────────────────────────────────── *)

type stored_object = {
  value : string;
  etag : string;
  size : int;
  last_modified : Ptime.t;
  content_type : string option;
  storage_class : Storage_class.t option;
  metadata : Metadata.t;
  tags : Tag.t list;
}

let md5_hex s = Digestif.MD5.(digest_string s |> to_hex)

let make_object ~clock ?(content_type = None) ?(storage_class = None)
    ?(metadata = []) ?(tags = []) value =
  {
    value;
    etag = "\"" ^ md5_hex value ^ "\"";
    size = String.length value;
    last_modified = Clock.now clock;
    content_type;
    storage_class;
    metadata;
    tags;
  }

let format_time t =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time t in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

type sim_bucket = {
  objects : (string, stored_object) Hashtbl.t;
  mutable policy : Policy.t option;
  creation_date : Ptime.t;
}

(* ── Store & client ─────────────────────────────────────────────── *)

type op_record = {
  op : [ `Put | `Get | `Head | `Delete | `List | `Copy | `Delete_batch ];
  bucket : string;
  key : string;
  timestamp : Ptime.t;
  faulted : bool;
}

type config = { max_list_keys : int }

let default_config = { max_list_keys = 1000 }

type store = {
  buckets : (string, sim_bucket) Hashtbl.t;
  clock : Clock.t;
  config : config;
  mutable history : op_record list;
}

let create_store ?(config = default_config) ~clock () =
  { buckets = Hashtbl.create (module String); clock; config; history = [] }

let record store op bucket key faulted =
  store.history <-
    store.history
    @ [ { op; bucket; key; timestamp = Clock.now store.clock; faulted } ]

type fault = Slow_down | Internal_error | Connection_reset | Response_lost
type buggify_config = { rng : Random.State.t; prob : float }

type t = {
  store : store;
  credentials : Awskit.Credentials.t;
  mutable faults : fault list;
  mutable buggify : buggify_config option;
}

let connect store ~credentials =
  { store; credentials; faults = []; buggify = None }

let store t = t.store
let inject_fault t fault = t.faults <- t.faults @ [ fault ]
let inject_faults t faults = t.faults <- t.faults @ faults
let clear_faults t = t.faults <- []

let enable_buggify t ~seed ~prob =
  t.buggify <- Some { rng = Random.State.make [| seed |]; prob }

let disable_buggify t = t.buggify <- None

let random_fault rng =
  match Random.State.int rng 4 with
  | 0 -> Slow_down
  | 1 -> Internal_error
  | 2 -> Connection_reset
  | _ -> Response_lost

let pop_fault t =
  match t.faults with
  | f :: rest ->
      t.faults <- rest;
      Some f
  | [] -> (
      match t.buggify with
      | Some { rng; prob } ->
          if Float.( < ) (Random.State.float rng 1.0) prob then
            Some (random_fault rng)
          else None
      | None -> None)

let fault_to_error : fault -> S3_error.t = function
  | Slow_down -> `Service_unavailable "SlowDown"
  | Internal_error -> `Request_error (500, "InternalError")
  | Connection_reset -> `Request_error (0, "connection reset")
  | Response_lost -> `Request_error (0, "response lost")

let validate_bucket bucket = Internal.Validation.validate_bucket bucket

let validate_bucket_key bucket key =
  let open Internal.Let_syntax in
  let* () = Internal.Validation.validate_bucket bucket in
  let* () = Internal.Validation.validate_key key in
  Ok ()

(* ── Policy check ──────────────────────────────────────────────── *)

let check_access t ~bucket_name ~action ~key =
  match Hashtbl.find t.store.buckets bucket_name with
  | None -> Error `No_such_bucket
  | Some b -> (
      match b.policy with
      | None -> Ok b
      | Some policy -> (
          let access_key_id = Awskit.Credentials.access_key_id t.credentials in
          let target =
            match key with
            | Some k -> Policy.Object_target (bucket_name, k)
            | None -> Policy.Bucket_target bucket_name
          in
          match Policy.evaluate policy ~access_key_id ~action ~target with
          | `Denied -> Error `Access_denied
          | `Allowed | `No_match -> Ok b))

(* ── Sim.Object ─────────────────────────��───────────────────────── *)

module Object = struct
  module Delete_object = struct
    type t = {
      key : string;
      version_id : string option;
      etag : string option;
      last_modified_time : string option;
      size : int option;
    }
    [@@deriving show, eq]

    let v ?version_id ?etag ?last_modified_time ?size key =
      { key; version_id; etag; last_modified_time; size }
  end

  let etag_matches condition object_ =
    match condition with
    | Object_.Etag_condition.Any -> true
    | Object_.Etag_condition.Etag etag -> String.equal etag object_.etag

  let write_preconditions_pass preconditions object_ =
    let open Object_.Preconditions.Write in
    let object_matches condition =
      match object_ with
      | None -> false
      | Some object_ -> etag_matches condition object_
    in
    match preconditions.if_match with
    | Some condition when not (object_matches condition) -> false
    | _ -> (
        match preconditions.if_none_match with
        | Some Object_.Etag_condition.Any when Option.is_some object_ -> false
        | Some condition when object_matches condition -> false
        | _ -> true)

  let read_preconditions_pass preconditions object_ =
    let open Object_.Preconditions.Read in
    write_preconditions_pass
      {
        Object_.Preconditions.Write.if_match = preconditions.if_match;
        if_none_match = preconditions.if_none_match;
      }
      object_

  let delete_preconditions_pass preconditions object_ =
    let open Object_.Preconditions.Delete in
    match object_ with
    | None -> Option.is_none preconditions.if_match
    | Some object_ ->
        let etag_ok =
          match preconditions.if_match with
          | None -> true
          | Some condition -> etag_matches condition object_
        in
        let size_ok =
          match preconditions.if_match_size with
          | None -> true
          | Some size -> size = object_.size
        in
        let last_modified_ok =
          match preconditions.if_match_last_modified_time with
          | None -> true
          | Some t -> String.equal t (format_time object_.last_modified)
        in
        etag_ok && size_ok && last_modified_ok

  let put t ~bucket ~key ?(preconditions = Object_.Preconditions.Write.none)
      ?content_type ?(metadata = []) ?storage_class ?tags ?cache_control:_
      ?content_encoding:_ ?content_disposition:_ body =
    let action = Object_.Action.to_string Object_.Action.Put in
    match
      Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
          let open Internal.Let_syntax in
          let* () = Internal.Validation.validate_metadata metadata in
          match tags with
          | Some tags -> Internal.Validation.validate_tags tags
          | None -> Ok ())
    with
    | Error _ as error -> error
    | Ok () -> (
        match check_access t ~bucket_name:bucket ~action ~key:(Some key) with
        | Error e -> Error e
        | Ok b -> (
            let tags = Option.value ~default:[] tags in
            match pop_fault t with
            | Some Response_lost ->
                let obj =
                  make_object ~clock:t.store.clock ~content_type ~storage_class
                    ~metadata ~tags body
                in
                let current = Hashtbl.find b.objects key in
                if not (write_preconditions_pass preconditions current) then begin
                  record t.store `Put bucket key true;
                  Error `Precondition_failed
                end
                else begin
                  Hashtbl.set b.objects ~key ~data:obj;
                  record t.store `Put bucket key true;
                  Error (fault_to_error Response_lost)
                end
            | Some fault ->
                record t.store `Put bucket key true;
                Error (fault_to_error fault)
            | None ->
                let current = Hashtbl.find b.objects key in
                if not (write_preconditions_pass preconditions current) then begin
                  record t.store `Put bucket key false;
                  Error `Precondition_failed
                end
                else begin
                  let obj =
                    make_object ~clock:t.store.clock ~content_type
                      ~storage_class ~metadata ~tags body
                  in
                  Hashtbl.set b.objects ~key ~data:obj;
                  record t.store `Put bucket key false;
                  Ok { Object_.Put_result.etag = obj.etag }
                end))

  let get t ~bucket ~key ?range:_
      ?(preconditions = Object_.Preconditions.Read.none) () =
    let action = Object_.Action.to_string Object_.Action.Get in
    match validate_bucket_key bucket key with
    | Error _ as error -> error
    | Ok () -> (
        match check_access t ~bucket_name:bucket ~action ~key:(Some key) with
        | Error e -> Error e
        | Ok b -> (
            match pop_fault t with
            | Some Response_lost ->
                record t.store `Get bucket key true;
                Error (fault_to_error Response_lost)
            | Some fault ->
                record t.store `Get bucket key true;
                Error (fault_to_error fault)
            | None -> (
                record t.store `Get bucket key false;
                match Hashtbl.find b.objects key with
                | Some obj ->
                    if not (read_preconditions_pass preconditions (Some obj))
                    then Error `Precondition_failed
                    else
                      Ok
                        {
                          Object_.Get_result.body = obj.value;
                          etag = obj.etag;
                          content_type = obj.content_type;
                          content_length = obj.size;
                          last_modified = format_time obj.last_modified;
                          metadata = obj.metadata;
                        }
                | None -> Error `Not_found)))

  let head t ~bucket ~key ?(preconditions = Object_.Preconditions.Read.none) ()
      =
    let action = Object_.Action.to_string Object_.Action.Head in
    match validate_bucket_key bucket key with
    | Error _ as error -> error
    | Ok () -> (
        match check_access t ~bucket_name:bucket ~action ~key:(Some key) with
        | Error e -> Error e
        | Ok b -> (
            match pop_fault t with
            | Some fault ->
                record t.store `Head bucket key true;
                Error (fault_to_error fault)
            | None -> (
                record t.store `Head bucket key false;
                match Hashtbl.find b.objects key with
                | Some obj ->
                    if not (read_preconditions_pass preconditions (Some obj))
                    then Error `Precondition_failed
                    else
                      Ok
                        (Some
                           {
                             Object_.Info.etag = obj.etag;
                             size = obj.size;
                             last_modified = format_time obj.last_modified;
                             content_type = obj.content_type;
                             storage_class = obj.storage_class;
                             metadata = obj.metadata;
                           })
                | None -> Ok None)))

  let delete t ~bucket ~key ?(preconditions = Object_.Preconditions.Delete.none)
      () =
    let action = Object_.Action.to_string Object_.Action.Delete in
    match validate_bucket_key bucket key with
    | Error _ as error -> error
    | Ok () -> (
        match check_access t ~bucket_name:bucket ~action ~key:(Some key) with
        | Error e -> Error e
        | Ok b -> (
            match pop_fault t with
            | Some Response_lost ->
                let current = Hashtbl.find b.objects key in
                if not (delete_preconditions_pass preconditions current) then
                  Error `Precondition_failed
                else begin
                  Hashtbl.remove b.objects key;
                  record t.store `Delete bucket key true;
                  Error (fault_to_error Response_lost)
                end
            | Some fault ->
                record t.store `Delete bucket key true;
                Error (fault_to_error fault)
            | None ->
                let current = Hashtbl.find b.objects key in
                if not (delete_preconditions_pass preconditions current) then
                  Error `Precondition_failed
                else begin
                  Hashtbl.remove b.objects key;
                  record t.store `Delete bucket key false;
                  Ok ()
                end))

  let delete_batch t ~bucket ~objects =
    let validate_delete_object (object_ : Delete_object.t) =
      let open Internal.Let_syntax in
      let* () = Internal.Validation.validate_key object_.key in
      let* () =
        Internal.Validation.validate_range_header_value "VersionId"
          object_.version_id
      in
      let* () =
        Internal.Validation.validate_range_header_value "ETag" object_.etag
      in
      let* () =
        Internal.Validation.validate_range_header_value "LastModifiedTime"
          object_.last_modified_time
      in
      match object_.size with
      | Some size when size < 0 ->
          Error (`Invalid_request "Size must be non-negative")
      | _ -> Ok ()
    in
    match
      Result.bind (validate_bucket bucket) ~f:(fun () ->
          let open Internal.Let_syntax in
          let* () =
            Internal.Validation.validate_delete_batch_keys
              (List.map objects ~f:(fun (object_ : Delete_object.t) ->
                   object_.key))
          in
          List.fold objects ~init:(Ok ()) ~f:(fun acc object_ ->
              match acc with
              | Error _ as error -> error
              | Ok () -> validate_delete_object object_))
    with
    | Error _ as error -> error
    | Ok () -> (
        match
          check_access t ~bucket_name:bucket
            ~action:(Object_.Action.to_string Delete)
            ~key:None
        with
        | Error e -> Error e
        | Ok b ->
            let deleted, errors =
              List.fold objects ~init:([], [])
                ~f:(fun (deleted, errors) (object_ : Delete_object.t) ->
                  let key = object_.key in
                  let current = Hashtbl.find b.objects key in
                  let preconditions =
                    {
                      Object_.Preconditions.Delete.if_match =
                        Option.map object_.etag ~f:(fun etag ->
                            Object_.Etag_condition.Etag etag);
                      if_match_last_modified_time = object_.last_modified_time;
                      if_match_size = object_.size;
                    }
                  in
                  if delete_preconditions_pass preconditions current then begin
                    Hashtbl.remove b.objects key;
                    ({ Object_.Delete_result.Deleted.key } :: deleted, errors)
                  end
                  else
                    ( deleted,
                      {
                        Object_.Delete_result.Error.key;
                        code = "PreconditionFailed";
                        message = "At least one precondition failed";
                      }
                      :: errors ))
            in
            let deleted = List.rev deleted in
            let errors = List.rev errors in
            record t.store `Delete_batch bucket "" false;
            Ok { Object_.Delete_result.deleted; errors })

  let copy t ~src_bucket ~src_key ~dst_bucket ~dst_key
      ?(source_preconditions = Object_.Preconditions.Copy_source.none)
      ?metadata_directive:_ ?(metadata = []) () =
    match
      Result.bind (validate_bucket_key src_bucket src_key) ~f:(fun () ->
          let open Internal.Let_syntax in
          let* () = validate_bucket_key dst_bucket dst_key in
          Internal.Validation.validate_metadata metadata)
    with
    | Error _ as error -> error
    | Ok () -> (
        match
          check_access t ~bucket_name:src_bucket
            ~action:(Object_.Action.to_string Get)
            ~key:(Some src_key)
        with
        | Error e -> Error e
        | Ok src_b -> (
            match
              check_access t ~bucket_name:dst_bucket
                ~action:(Object_.Action.to_string Put)
                ~key:(Some dst_key)
            with
            | Error e -> Error e
            | Ok dst_b -> (
                match pop_fault t with
                | Some fault ->
                    record t.store `Copy dst_bucket dst_key true;
                    Error (fault_to_error fault)
                | None -> (
                    record t.store `Copy dst_bucket dst_key false;
                    match Hashtbl.find src_b.objects src_key with
                    | None -> Error `Not_found
                    | Some src_obj ->
                        let source_write_preconditions =
                          {
                            Object_.Preconditions.Write.if_match =
                              source_preconditions.if_match;
                            if_none_match = source_preconditions.if_none_match;
                          }
                        in
                        if
                          not
                            (write_preconditions_pass source_write_preconditions
                               (Some src_obj))
                        then Error `Precondition_failed
                        else
                          let new_metadata =
                            if not (List.is_empty metadata) then metadata
                            else src_obj.metadata
                          in
                          let obj =
                            make_object ~clock:t.store.clock
                              ~content_type:src_obj.content_type
                              ~storage_class:src_obj.storage_class
                              ~metadata:new_metadata ~tags:src_obj.tags
                              src_obj.value
                          in
                          Hashtbl.set dst_b.objects ~key:dst_key ~data:obj;
                          Ok
                            {
                              Object_.Copy_result.etag = obj.etag;
                              last_modified = format_time obj.last_modified;
                            }))))

  let list t ~bucket ~prefix ?delimiter:_
      ?(max_keys = t.store.config.max_list_keys) ?start_after:_
      ?continuation_token () =
    match
      Result.bind (validate_bucket bucket) ~f:(fun () ->
          let open Internal.Let_syntax in
          let* () = Internal.Validation.validate_max_keys max_keys in
          let* () = Internal.Validation.ensure_no_ctl ~field:"prefix" prefix in
          Internal.Validation.validate_range_header_value "continuation_token"
            continuation_token)
    with
    | Error _ as error -> error
    | Ok () -> (
        match
          check_access t ~bucket_name:bucket
            ~action:(Object_.Action.to_string List)
            ~key:None
        with
        | Error e -> Error e
        | Ok b -> (
            match pop_fault t with
            | Some fault ->
                record t.store `List bucket prefix true;
                Error (fault_to_error fault)
            | None ->
                record t.store `List bucket prefix false;
                let all_keys =
                  Hashtbl.keys b.objects
                  |> List.filter ~f:(fun k -> String.is_prefix k ~prefix)
                  |> List.sort ~compare:String.compare
                in
                let after_token =
                  match continuation_token with
                  | None -> all_keys
                  | Some token ->
                      List.filter
                        ~f:(fun k -> String.compare k token > 0)
                        all_keys
                in
                let page =
                  let rec take n = function
                    | [] -> []
                    | _ when n <= 0 -> []
                    | x :: xs -> x :: take (n - 1) xs
                  in
                  take max_keys after_token
                in
                let is_truncated = List.length after_token > max_keys in
                let next_continuation_token =
                  if is_truncated then
                    match List.rev page with
                    | last :: _ -> Some last
                    | [] -> None
                  else None
                in
                Ok
                  {
                    Object_.List_result.keys = page;
                    is_truncated;
                    next_continuation_token;
                  }))

  module Tagging = struct
    let get t ~bucket ~key =
      match validate_bucket_key bucket key with
      | Error _ as error -> error
      | Ok () -> (
          match
            check_access t ~bucket_name:bucket
              ~action:(Object_.Tagging.Action.to_string Get)
              ~key:(Some key)
          with
          | Error e -> Error e
          | Ok b -> (
              match Hashtbl.find b.objects key with
              | None -> Error `Not_found
              | Some obj -> Ok obj.tags))

    let put t ~bucket ~key tags =
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            Internal.Validation.validate_tags tags)
      with
      | Error _ as error -> error
      | Ok () -> (
          match
            check_access t ~bucket_name:bucket
              ~action:(Object_.Tagging.Action.to_string Put)
              ~key:(Some key)
          with
          | Error e -> Error e
          | Ok b -> (
              match Hashtbl.find b.objects key with
              | None -> Error `Not_found
              | Some obj ->
                  Hashtbl.set b.objects ~key ~data:{ obj with tags };
                  Ok ()))

    let delete t ~bucket ~key =
      match validate_bucket_key bucket key with
      | Error _ as error -> error
      | Ok () -> (
          match
            check_access t ~bucket_name:bucket
              ~action:(Object_.Tagging.Action.to_string Delete)
              ~key:(Some key)
          with
          | Error e -> Error e
          | Ok b -> (
              match Hashtbl.find b.objects key with
              | None -> Error `Not_found
              | Some obj ->
                  Hashtbl.set b.objects ~key ~data:{ obj with tags = [] };
                  Ok ()))
  end
end

(* ── Sim.Bucket ─────────────────────────��───────────────────────── *)

module Bucket = struct
  let create t ~bucket ?region:_ () =
    match validate_bucket bucket with
    | Error _ as error -> error
    | Ok () ->
        if Hashtbl.mem t.store.buckets bucket then Error `Bucket_already_exists
        else begin
          Hashtbl.set t.store.buckets ~key:bucket
            ~data:
              {
                objects = Hashtbl.create (module String);
                policy = None;
                creation_date = Clock.now t.store.clock;
              };
          Ok ()
        end

  let delete t ~bucket =
    match validate_bucket bucket with
    | Error _ as error -> error
    | Ok () -> (
        match Hashtbl.find t.store.buckets bucket with
        | None -> Error `No_such_bucket
        | Some b ->
            if Hashtbl.length b.objects > 0 then Error `Bucket_not_empty
            else begin
              Hashtbl.remove t.store.buckets bucket;
              Ok ()
            end)

  let head t ~bucket =
    match validate_bucket bucket with
    | Error _ as error -> error
    | Ok () -> Ok (Hashtbl.mem t.store.buckets bucket)

  let list t =
    let buckets =
      Hashtbl.fold t.store.buckets ~init:[] ~f:(fun ~key:name ~data:b acc ->
          { Bucket.Info.name; creation_date = format_time b.creation_date }
          :: acc)
      |> List.sort ~compare:(fun a b ->
          String.compare a.Bucket.Info.name b.Bucket.Info.name)
    in
    Ok buckets

  module Policy = struct
    let get t ~bucket =
      match validate_bucket bucket with
      | Error _ as error -> error
      | Ok () -> (
          match Hashtbl.find t.store.buckets bucket with
          | None -> Error `No_such_bucket
          | Some b -> (
              match b.policy with None -> Error `Not_found | Some p -> Ok p))

    let put t ~bucket policy =
      match validate_bucket bucket with
      | Error _ as error -> error
      | Ok () -> (
          match Hashtbl.find t.store.buckets bucket with
          | None -> Error `No_such_bucket
          | Some b ->
              b.policy <- Some policy;
              Ok ())

    let delete t ~bucket =
      match validate_bucket bucket with
      | Error _ as error -> error
      | Ok () -> (
          match Hashtbl.find t.store.buckets bucket with
          | None -> Error `No_such_bucket
          | Some b ->
              b.policy <- None;
              Ok ())
  end
end

(* ── Inspection ──────��─────────────────────────────��────────────── *)

type object_meta = { etag : string; size : int; last_modified : Ptime.t }

let object_meta store ~bucket key =
  match Hashtbl.find store.buckets bucket with
  | None -> None
  | Some b ->
      Hashtbl.find b.objects key
      |> Option.map ~f:(fun (obj : stored_object) ->
          {
            etag = obj.etag;
            size = obj.size;
            last_modified = obj.last_modified;
          })

let keys store ~bucket =
  match Hashtbl.find store.buckets bucket with
  | None -> []
  | Some b -> Hashtbl.keys b.objects |> List.sort ~compare:String.compare

let history store = store.history
let clear_history store = store.history <- []

let dump store ~bucket =
  match Hashtbl.find store.buckets bucket with
  | None -> []
  | Some b ->
      Hashtbl.fold b.objects ~init:[] ~f:(fun ~key:k ~data:v acc ->
          (k, v.value) :: acc)
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
