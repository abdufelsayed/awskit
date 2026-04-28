open Common

open struct
  module Object = Object
end

module type MULTIPART_DATA = sig
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
    type t = private { part_number : int; etag : Object.Etag.t }

    val create : part_number:int -> etag:Object.Etag.t -> (t, Error.t) result
    val create_exn : part_number:int -> etag:Object.Etag.t -> t
  end

  module Create : sig
    type options = {
      content_type : string option;
      metadata : Metadata.t;
      storage_class : Storage_class.t option;
      tags : Tag.t list;
      checksum : Object.Checksum.request option;
      server_side_encryption : Object.Encryption.request option;
    }

    type result = { upload : Upload.t; request : Awskit.Response.t }

    val default_options : options
  end

  module Upload_part : sig
    type options = { checksum : Object.Checksum.request option }

    type result = {
      part : Part.t;
      checksum : Object.Checksum.response option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Complete : sig
    type result = {
      etag : Object.Etag.t option;
      version_id : Object.Version_id.t option;
      checksum : Object.Checksum.response option;
      request : Awskit.Response.t;
    }
  end

  module List_parts : sig
    type options = { max_parts : int option; part_number_marker : int option }

    type part_info = {
      part_number : int;
      etag : Object.Etag.t option;
      size : int64 option;
      last_modified : Ptime.t option;
      checksum : Object.Checksum.response option;
    }

    type page = {
      parts : part_info list;
      is_truncated : bool;
      next_part_number_marker : int option;
      request : Awskit.Response.t;
    }

    val default_options : options
  end

  module Managed : sig
    val min_part_size : int
    val default_part_size : int
    val max_parts : int

    type options = {
      part_size : int;
      create_options : Create.options;
      upload_part_options : Upload_part.options;
    }

    type result = {
      upload : Upload.t;
      parts : Part.t list;
      complete : Complete.result;
    }

    val default_options : options
    val validate_options : options -> (unit, Error.t) Stdlib.result
  end
end
