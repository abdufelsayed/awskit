open Common
include Data_intf
include Operation_data

module type MULTIPART_DATA = sig
  (** Shared multipart data constructors used by runtime-backed and standalone
      multipart APIs. *)
  module Upload_id : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
  end

  module Upload : sig
    type t = private { bucket : string; key : string; upload_id : Upload_id.t }

    val create :
      bucket:string ->
      key:string ->
      upload_id:Upload_id.t ->
      (t, Error.t) result

    val create_exn : bucket:string -> key:string -> upload_id:Upload_id.t -> t
  end

  module Part : sig
    type t = private {
      part_number : int;
      etag : Object.Etag.t;
      checksum : Object.Checksum.value option;
    }

    val create :
      ?checksum:Object.Checksum.value ->
      part_number:int ->
      etag:Object.Etag.t ->
      unit ->
      (t, Error.t) result

    val create_exn :
      ?checksum:Object.Checksum.value ->
      part_number:int ->
      etag:Object.Etag.t ->
      unit ->
      t
  end
end

module type TRANSFER_DATA = sig
  val min_part_size : int
  (** Shared high-level transfer options and result types used by Unix-capable
      S3 adapters. *)

  val default_part_size : int
  val default_multipart_threshold : int64
  val default_concurrency : int
  val max_parts : int

  type upload_options = {
    multipart_threshold : int64;
    part_size : int;
    concurrency : int;
    put_options : Put_object.options;
    create_options : Create_multipart_upload.options;
    upload_part_options : Upload_part.options;
    complete_options : Complete_multipart_upload.options;
    abort_options : Abort_multipart_upload.options;
    list_parts_options : List_parts.options;
  }

  type download_options = {
    multipart_threshold : int64;
    part_size : int;
    concurrency : int;
    get_options : Get_object.options;
  }

  type upload_strategy = [ `Put | `Multipart ]
  type download_strategy = [ `Get | `Ranged ]

  type multipart_upload_result = {
    upload : Multipart.Upload.t;
    parts : Multipart.Part.t list;
    complete : Complete_multipart_upload.result;
  }

  type upload_result =
    | Put of Put_object.result
    | Multipart of multipart_upload_result

  type download_result =
    | Get of Get_object.result
    | Ranged of { info : Head_object.result; parts : int }

  val upload_strategy : upload_result -> upload_strategy
  val download_strategy : download_result -> download_strategy
  val default_upload_options : upload_options
  val default_download_options : download_options
  val validate_upload_options : upload_options -> (unit, Error.t) Stdlib.result

  val validate_upload_multipart_selection :
    upload_options -> (unit, Error.t) Stdlib.result

  val validate_download_options :
    download_options -> (unit, Error.t) Stdlib.result

  val validate_multipart_part_count :
    content_length:int64 -> part_size:int -> (unit, Error.t) Stdlib.result
end

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]
(** Requested S3 bucket addressing style. *)

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]
(** AWS endpoint variant used when no explicit endpoint is supplied. *)

type endpoint_config = Endpoint_resolver.t
(** S3 endpoint and addressing configuration shared by runtime-backed clients
    and presigned URL generation. *)

let endpoint_config ?addressing_style ?endpoint_variant ?scheme ?endpoint () =
  Endpoint_resolver.create ?addressing_style ?endpoint_variant ?scheme ?endpoint
    ()

let default_endpoint_config = Endpoint_resolver.default

module type RUNTIME = sig
  include Awskit.Runtime.S
  (** Runtime implementation plus S3-specific endpoint configuration. *)

  val s3_endpoint_config : connection -> endpoint_config
  (** Return endpoint configuration for bucket/object request resolution. *)
end
