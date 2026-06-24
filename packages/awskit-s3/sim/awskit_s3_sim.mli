(** Deterministic in-memory S3 implementation for tests.

    The simulator implements the same synchronous S3 client shape as
    runtime-backed clients. It stores objects in memory, records operation
    history, exposes inspection helpers, and supports deterministic fault
    injection. *)

module Clock : sig
  type t
  (** Mutable deterministic clock used for timestamps and retry sleeps. *)

  val create : ?now:Ptime.t -> unit -> t
  (** Create a clock. Defaults to a stable initial timestamp when [now] is
      omitted. *)

  val now : t -> Ptime.t
  (** Return the current simulated time. *)

  val advance : t -> Ptime.Span.t -> unit
  (** Advance simulated time by a span. *)

  val advance_ms : t -> int -> unit
  (** Advance simulated time by milliseconds. *)
end

type config = private { max_list_keys : int }
(** Simulator configuration. [max_list_keys] caps list-page sizes when the
    request does not provide a smaller [max_keys]. *)

val config : ?max_list_keys:int -> unit -> (config, Awskit.Error.t) result
(** Validate and create simulator configuration. [max_list_keys] must be between
    [1] and [1000]. *)

val config_exn : ?max_list_keys:int -> unit -> config
(** Like {!val:config}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)

val default_config : config
(** Default simulator configuration. *)

type store
(** Shared in-memory S3 store. Multiple connections can share one store. *)

val create_store : ?config:config -> clock:Clock.t -> unit -> store
(** Create an empty store using a deterministic clock. *)

type t
(** Simulator connection handle. *)

val connect : store -> credentials:Awskit.Credentials.t -> t
(** Connect to a store with credentials used by presigning/signing helpers. *)

val store : t -> store
(** Return the underlying shared store. *)

(** Direct-style runtime used by the simulator. *)
module Runtime : Awskit_s3.RUNTIME with type 'a t = 'a and type connection = t

module Body :
  Awskit_s3.BODY with type 'a io := 'a and type t = Runtime.request_body

module Reader :
  Awskit_s3.READER
    with type 'a io := 'a
     and type t = Runtime.response_body_reader

type fault = Slow_down | Internal_error | Connection_reset | Response_lost

val inject_fault : t -> fault -> unit
(** Queue one fault for the next applicable operation. *)

val inject_faults : t -> fault list -> unit
(** Queue several faults in order. *)

val clear_faults : t -> unit
(** Remove queued faults. *)

val enable_random_faults : t -> seed:int -> prob:float -> unit
(** Enable deterministic pseudo-random fault injection with probability [prob].
*)

val disable_random_faults : t -> unit
(** Disable random fault injection. Queued explicit faults are unchanged. *)

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
      (** Operation kind. *)
  bucket : string;  (** Bucket targeted by the operation. *)
  key : string option;
      (** Object key targeted by object/multipart operations. *)
  timestamp : Ptime.t;  (** Simulated operation time. *)
  faulted : bool;  (** Whether the operation consumed an injected fault. *)
}
(** Recorded simulator operation. *)

type object_metadata = {
  etag : Awskit_s3.Object.Etag.t option;  (** Current object ETag. *)
  size : int64 option;  (** Current object size in bytes. *)
  last_modified : Ptime.t option;  (** Current object last-modified timestamp. *)
}
(** Inspectable metadata for the current version of an object. *)

val object_metadata :
  store ->
  bucket:Awskit_s3.Bucket_name.t ->
  key:Awskit_s3.Object_key.t ->
  object_metadata option
(** Return metadata for the current object version, if present. *)

val keys : store -> bucket:Awskit_s3.Bucket_name.t -> string list
(** Return current object keys in a bucket. *)

val history : store -> operation_record list
(** Return recorded operations in chronological order. *)

val clear_history : store -> unit
(** Clear recorded operation history. *)

val objects_as_strings :
  store -> bucket:Awskit_s3.Bucket_name.t -> (string * string) list
(** Return current bucket objects whose bodies can be decoded as strings. *)

module Object :
  Awskit_s3.OBJECT
    with type connection := t
     and type 'a io := 'a
     and type request_body := Body.t
     and type response_body_reader := Reader.t

module Bucket : Awskit_s3.BUCKET with type connection := t and type 'a io := 'a

module Multipart :
  Awskit_s3.MULTIPART
    with type connection := t
     and type 'a io := 'a
     and type request_body := Body.t

module Presigned :
  Awskit_s3.PRESIGNED with type connection := t and type 'a io := 'a
