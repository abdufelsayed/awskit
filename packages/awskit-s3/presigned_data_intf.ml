open Common

open struct
  module Multipart = Multipart
  module Object = Object
end

module type PRESIGNED_DATA = sig
  type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

  type endpoint_variant =
    [ `Regional
    | `Dualstack
    | `Fips
    | `Fips_dualstack
    | `Accelerate
    | `Accelerate_dualstack ]

  type method_ = [ `GET | `PUT | `HEAD | `DELETE ]

  type result = {
    url : string;
    method_ : method_;
    headers : (string * string) list;
    expires_at : Ptime.t option;
  }

  module Put_object : sig
    type options = {
      expires_in : Ptime.Span.t option;
      content_type : string option;
      checksum : Object.Checksum.value option;
      server_side_encryption : Object.Encryption.request option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  module Get_object : sig
    type options = {
      expires_in : Ptime.Span.t option;
      response_content_type : string option;
      response_content_disposition : string option;
      version_id : Object.Version_id.t option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  module Upload_part : sig
    type options = {
      expires_in : Ptime.Span.t option;
      checksum : Object.Checksum.value option;
      headers : (string * string) list;
    }

    val default_options : options
  end

  val get_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val put_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val head_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val delete_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    ?expires_in:Ptime.Span.t ->
    unit ->
    (result, Error.t) Stdlib.result

  val upload_part :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?endpoint:Awskit.Endpoint.t ->
    ?addressing_style:addressing_style ->
    ?endpoint_variant:endpoint_variant ->
    ?scheme:Awskit.Endpoint.Scheme.t ->
    bucket:string ->
    key:string ->
    upload_id:Multipart.Upload_id.t ->
    part_number:int ->
    ?options:Upload_part.options ->
    unit ->
    (result, Error.t) Stdlib.result
end
