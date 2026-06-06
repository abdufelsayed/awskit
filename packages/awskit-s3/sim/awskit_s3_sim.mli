open Awskit_s3

module Clock : sig
  type t

  val create : ?now:Ptime.t -> unit -> t
  val now : t -> Ptime.t
  val advance : t -> Ptime.Span.t -> unit
  val advance_ms : t -> int -> unit
end

type config = { max_list_keys : int }

val default_config : config

type store

val create_store : ?config:config -> clock:Clock.t -> unit -> store

type t

val connect : store -> credentials:Awskit.Credentials.t -> t
val store : t -> store

module Runtime : Awskit_s3.RUNTIME with type 'a t = 'a and type connection = t

type fault = Slow_down | Internal_error | Connection_reset | Response_lost

val inject_fault : t -> fault -> unit
val inject_faults : t -> fault list -> unit
val clear_faults : t -> unit
val enable_random_faults : t -> seed:int -> prob:float -> unit
val disable_random_faults : t -> unit

type operation_record = {
  op :
    [ `Put_object
    | `Get_object
    | `Head_object
    | `Delete_object
    | `List_objects_v2
    | `List_object_versions
    | `Copy_object
    | `Delete_objects
    | `Create_multipart_upload
    | `Upload_part
    | `Complete_multipart_upload
    | `Abort_multipart_upload
    | `List_parts ];
  bucket : string;
  key : string option;
  timestamp : Ptime.t;
  faulted : bool;
}

type object_metadata = {
  etag : Object.Etag.t option;
  size : int64 option;
  last_modified : Ptime.t option;
}

val object_metadata :
  store -> bucket:string -> key:string -> object_metadata option

val keys : store -> bucket:string -> string list
val history : store -> operation_record list
val clear_history : store -> unit
val objects_as_strings : store -> bucket:string -> (string * string) list

include
  S
    with type connection := t
     and type 'a io := 'a
     and type request_body := Runtime.request_body
     and type response_body_reader := Runtime.response_body_reader
