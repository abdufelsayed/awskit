open Awskit_s3_common

open struct
  module Object = Awskit_s3_object
  module Provider = Awskit_s3_provider
end

module type PRESIGNED_DATA = sig
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
      checksum : Object.Checksum.request option;
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

  val get_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?provider:Provider.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val put_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?provider:Provider.t ->
    bucket:string ->
    key:string ->
    ?options:Put_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val head_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?provider:Provider.t ->
    bucket:string ->
    key:string ->
    ?options:Get_object.options ->
    unit ->
    (result, Error.t) Stdlib.result

  val delete_object :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    now:Ptime.t ->
    ?provider:Provider.t ->
    bucket:string ->
    key:string ->
    ?expires_in:Ptime.Span.t ->
    unit ->
    (result, Error.t) Stdlib.result
end
