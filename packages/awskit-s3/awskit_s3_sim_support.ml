open Awskit_s3_core

module Clock = struct
  type t = { mutable now : Ptime.t }

  let create ?(now = Ptime.epoch) () = { now }
  let now t = t.now

  let advance t span =
    t.now <- Option.value ~default:t.now (Ptime.add_span t.now span)

  let advance_ms t ms = advance t (Ptime.Span.of_int_s (ms / 1000))
end

type config = { max_list_keys : int }

let default_config = { max_list_keys = 1000 }

type stored_object = {
  mutable body : string;
  mutable etag : Object.Etag.t;
  mutable content_type : string option;
  mutable metadata : Metadata.t;
  mutable storage_class : Storage_class.t option;
  mutable tags : Tag.t list;
  mutable checksum : Object.Checksum.response option;
  mutable last_modified : Ptime.t;
}

type stored_part = {
  part_number : int;
  body : string;
  etag : Object.Etag.t;
  checksum : Object.Checksum.response option;
  last_modified : Ptime.t;
}

type multipart_upload = {
  upload : Multipart.Upload.t;
  content_type : string option;
  metadata : Metadata.t;
  storage_class : Storage_class.t option;
  tags : Tag.t list;
  checksum_request : Object.Checksum.request option;
  parts : (int, stored_part) Hashtbl.t;
  created_at : Ptime.t;
}

type bucket_state = {
  created_at : Ptime.t;
  objects : (string, stored_object) Hashtbl.t;
  multipart_uploads : (string, multipart_upload) Hashtbl.t;
  mutable policy : Policy.t option;
  mutable bucket_tags : Tag.t list;
  mutable versioning : Bucket.Versioning.Status.t option;
  mutable encryption : Bucket.Encryption.config option;
  mutable cors : Bucket.Cors.config option;
  mutable website : Bucket.Website.config option;
  mutable public_access_block : Bucket.Public_access_block.config option;
  mutable ownership_controls : Bucket.Ownership_controls.config option;
  mutable request_payment : Bucket.Request_payment.Payer.t option;
  mutable accelerate : Bucket.Accelerate.Status.t option;
  mutable logging : Bucket.Logging.config;
}

type op_record = {
  op :
    [ `Put
    | `Get
    | `Head
    | `Delete
    | `List
    | `Copy
    | `Delete_many
    | `Multipart_create
    | `Multipart_upload_part
    | `Multipart_complete
    | `Multipart_abort
    | `Multipart_list_parts ];
  bucket : string;
  key : string option;
  timestamp : Ptime.t;
  faulted : bool;
}

type store = {
  config : config;
  clock : Clock.t;
  buckets : (string, bucket_state) Hashtbl.t;
  mutable history : op_record list;
  mutable next_upload_id : int;
}

let create_store ?(config = default_config) ~clock () =
  {
    config;
    clock;
    buckets = Hashtbl.create 17;
    history = [];
    next_upload_id = 1;
  }

type fault = Slow_down | Internal_error | Connection_reset | Response_lost
type buggify = { random : Random.State.t; prob : float }

type t = {
  store : store;
  credentials : Awskit.Credentials.t;
  mutable faults : fault list;
  mutable buggify : buggify option;
}

let connect store ~credentials =
  { store; credentials; faults = []; buggify = None }

let store t = t.store
let now t = Clock.now t.store.clock

let service ?message ~status ~code () =
  Awskit.Error.service
    {
      status;
      code = Some code;
      message;
      request_id = None;
      host_id = None;
      headers = [];
      body = None;
    }

let no_such_bucket () = service ~status:404 ~code:"NoSuchBucket" ()
let no_such_key () = service ~status:404 ~code:"NoSuchKey" ()
let no_such_upload () = service ~status:404 ~code:"NoSuchUpload" ()
let precondition_failed () = service ~status:412 ~code:"PreconditionFailed" ()
let not_modified () = service ~status:304 ~code:"NotModified" ()
let response ?headers status = Awskit.Response.create_exn ~status ?headers ()

let record ?(faulted = false) t op bucket key =
  t.store.history <-
    { op; bucket; key; timestamp = now t; faulted } :: t.store.history

let fault_error = function
  | Slow_down -> service ~status:503 ~code:"SlowDown" ()
  | Internal_error -> service ~status:500 ~code:"InternalError" ()
  | Connection_reset ->
      Awskit.Error.transport ~retryable:true "simulated connection reset"
  | Response_lost -> Awskit.Error.body "simulated response body loss"

let take_fault t =
  match t.faults with
  | fault :: rest ->
      t.faults <- rest;
      Some fault
  | [] -> (
      match t.buggify with
      | None -> None
      | Some buggify ->
          if Random.State.float buggify.random 1.0 < buggify.prob then
            Some Internal_error
          else None)

let operation_fault t op bucket key =
  match take_fault t with
  | None ->
      record t op bucket key;
      None
  | Some fault ->
      record ~faulted:true t op bucket key;
      Some (fault_error fault)

let bucket_state store bucket = Hashtbl.find_opt store.buckets bucket

let require_bucket t bucket =
  match bucket_state t.store bucket with
  | Some state -> Ok state
  | None -> Error (no_such_bucket ())

let require_object t bucket key =
  let* bucket = require_bucket t bucket in
  match Hashtbl.find_opt bucket.objects key with
  | Some object_ -> Ok object_
  | None -> Error (no_such_key ())

let object_size (obj : stored_object) = Int64.of_int (String.length obj.body)

let etag_condition_matches (obj : stored_object) = function
  | Object.Etag_condition.Any -> true
  | Etag etag -> Object.Etag.equal obj.etag etag

let etag_condition_matches_opt obj condition =
  match obj with
  | None -> false
  | Some obj -> etag_condition_matches obj condition

let ensure_write_preconditions obj (p : Object.Preconditions.Write.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches_opt obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match (p.if_none_match, obj) with
      | Some Object.Etag_condition.Any, Some _ -> Error (precondition_failed ())
      | Some (Etag etag), Some obj when Object.Etag.equal obj.etag etag ->
          Error (precondition_failed ())
      | _ -> Ok ())

let time_leq left right = Ptime.compare left right <= 0
let time_gt left right = Ptime.compare left right > 0

let ensure_read_preconditions (obj : stored_object)
    (p : Object.Preconditions.Read.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match p.if_unmodified_since with
      | Some time when time_gt obj.last_modified time ->
          Error (precondition_failed ())
      | _ -> (
          match p.if_none_match with
          | Some condition when etag_condition_matches obj condition ->
              Error (not_modified ())
          | _ -> (
              match p.if_modified_since with
              | Some time when time_leq obj.last_modified time ->
                  Error (not_modified ())
              | _ -> Ok ())))

let ensure_delete_preconditions (obj : stored_object)
    (p : Object.Preconditions.Delete.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match p.if_match_last_modified_time with
      | Some time when Ptime.compare obj.last_modified time <> 0 ->
          Error (precondition_failed ())
      | _ -> (
          match p.if_match_size with
          | Some size when Int64.compare (object_size obj) size <> 0 ->
              Error (precondition_failed ())
          | _ -> Ok ()))

let delete_preconditions_are_empty (p : Object.Preconditions.Delete.t) =
  Option.is_none p.if_match
  && Option.is_none p.if_match_last_modified_time
  && Option.is_none p.if_match_size

let ensure_copy_source_preconditions (obj : stored_object)
    (p : Object.Preconditions.Copy_source.t) =
  match p.if_match with
  | Some condition when not (etag_condition_matches obj condition) ->
      Error (precondition_failed ())
  | _ -> (
      match p.if_unmodified_since with
      | Some time when time_gt obj.last_modified time ->
          Error (precondition_failed ())
      | _ -> (
          match p.if_none_match with
          | Some condition when etag_condition_matches obj condition ->
              Error (precondition_failed ())
          | _ -> (
              match p.if_modified_since with
              | Some time when time_leq obj.last_modified time ->
                  Error (precondition_failed ())
              | _ -> Ok ())))

let upload_key upload_id = Multipart.Upload_id.to_string upload_id

let require_multipart_upload t ~bucket ~key ~upload_id =
  let* bucket_state = require_bucket t bucket in
  match
    Hashtbl.find_opt bucket_state.multipart_uploads (upload_key upload_id)
  with
  | Some upload when String.equal upload.upload.key key ->
      Ok (bucket_state, upload)
  | _ -> Error (no_such_upload ())

let etag body = "\"" ^ Digestif.MD5.(digest_string body |> to_hex) ^ "\""

let checksum_of_request ~body (request : Object.Checksum.request) =
  let value =
    match request.value with
    | Some value -> Some value
    | None -> (
        match request.algorithm with
        | `SHA1 ->
            Some
              (Digestif.SHA1.(digest_string body |> to_raw_string)
              |> Base64.encode_exn)
        | `SHA256 ->
            Some
              (Digestif.SHA256.(digest_string body |> to_raw_string)
              |> Base64.encode_exn)
        | `CRC32 | `CRC32C | `CRC64NVME -> None)
  in
  Option.map
    (fun value -> { Object.Checksum.algorithm = request.algorithm; value })
    value

let checksum_for_body ~body request =
  Option.bind request (checksum_of_request ~body)

let checksum_response_headers = function
  | None -> []
  | Some (checksum : Object.Checksum.response) ->
      [ (checksum_header_name checksum.algorithm, checksum.value) ]

let next_upload_id t =
  let id = t.store.next_upload_id in
  t.store.next_upload_id <- id + 1;
  Multipart.Upload_id.of_string_exn (Fmt.str "sim-upload-%d" id)

module Runtime = struct
  type connection = t
  type 'a t = 'a

  let return x = x
  let bind x f = f x

  type upload_body = string
  type download_body = { body : string; read_fault : Awskit.Error.t option }
  type upload_writer = Buffer.t

  type download_reader = {
    body : string;
    mutable offset : int;
    mutable read_fault : Awskit.Error.t option;
  }

  let now = now
  let region _ = Awskit.Region.of_string_exn "us-east-1"
  let credentials t = Ok t.credentials
  let endpoint _ = None
  let retry_policy _ = Awskit.Retry.default
  let sleep t span = Clock.advance t.store.clock span
  let s3_endpoint_config _ = default_endpoint_config
  let empty_body = ""
  let string_body value = value
  let bytes_body value = Bytes.to_string value

  let stream_body _descriptor ~write =
    let buffer = Buffer.create 1024 in
    match write buffer with Ok () -> Buffer.contents buffer | Error _ -> ""

  let upload_descriptor body =
    {
      Awskit.Body.Upload.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let write_string writer value =
    Buffer.add_string writer value;
    Ok ()

  let read (reader : download_reader) bytes ~off ~len =
    match reader.read_fault with
    | Some error ->
        reader.read_fault <- None;
        Error error
    | None ->
        if len = 0 then Ok 0
        else
          let remaining = String.length reader.body - reader.offset in
          if remaining <= 0 then Ok 0
          else
            let copied = min len remaining in
            String.blit reader.body reader.offset bytes off copied;
            reader.offset <- reader.offset + copied;
            Ok copied

  let download_body ?read_fault body : download_body = { body; read_fault }

  let rec discard_reader reader =
    let buffer = Bytes.create 8192 in
    match read reader buffer ~off:0 ~len:(Bytes.length buffer) with
    | Error _ as error -> error
    | Ok 0 -> Ok ()
    | Ok _ -> discard_reader reader

  let with_download_body (body : download_body) ~consume =
    let reader =
      { body = body.body; offset = 0; read_fault = body.read_fault }
    in
    match consume reader with
    | Ok _ as result -> (
        match discard_reader reader with
        | Ok () -> result
        | Error _ as error -> error)
    | Error _ as error -> (
        match discard_reader reader with
        | Ok () -> error
        | Error _ as drain_error -> drain_error)

  let discard_download_body body =
    with_download_body body ~consume:(fun reader -> discard_reader reader)

  let call _ _ _ =
    Error
      (Awskit.Error.transport ~retryable:false
         "Sim.Runtime.call is not an HTTP transport")
end

type object_meta = {
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

let object_meta store ~bucket ~key =
  match bucket_state store bucket with
  | None -> None
  | Some bucket -> (
      match Hashtbl.find_opt bucket.objects key with
      | None -> None
      | Some obj ->
          Some
            {
              etag = Some obj.etag;
              size = Some (Int64.of_int (String.length obj.body));
              last_modified = Some obj.last_modified;
            })

let keys store ~bucket =
  match bucket_state store bucket with
  | None -> []
  | Some bucket ->
      Hashtbl.to_seq_keys bucket.objects
      |> List.of_seq
      |> List.sort String.compare

let history store = List.rev store.history
let clear_history store = store.history <- []

let dump_strings store ~bucket =
  match bucket_state store bucket with
  | None -> []
  | Some (bucket : bucket_state) ->
      Hashtbl.to_seq bucket.objects
      |> Seq.map (fun (key, (obj : stored_object)) -> (key, obj.body))
      |> List.of_seq
      |> List.sort compare

let inject_fault t fault = t.faults <- t.faults @ [ fault ]
let inject_faults t faults = t.faults <- t.faults @ faults
let clear_faults t = t.faults <- []

let enable_buggify t ~seed ~prob =
  t.buggify <- Some { random = Random.State.make [| seed |]; prob }

let disable_buggify t = t.buggify <- None

let info_of_object response (obj : stored_object) =
  {
    Object.Get.etag = Some obj.etag;
    content_type = obj.content_type;
    content_length = Some (Int64.of_int (String.length obj.body));
    last_modified = Some obj.last_modified;
    metadata = obj.metadata;
    storage_class = obj.storage_class;
    version_id = None;
    checksum = obj.checksum;
    server_side_encryption = None;
    request = response;
  }
