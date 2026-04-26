(** Deterministic in-memory S3 simulation with policy enforcement and fault
    injection. No AWS account, no network.

    {[
    let clock = Sim.Clock.create () in
    let store = Sim.create_store ~clock () in
    let credentials =
      Awskit_s3.Credentials.make ~access_key_id:"test" ~secret_access_key:"test"
        ()
    in
    let conn = Sim.connect store ~credentials in

    Sim.Bucket.create conn ~bucket:"test" () |> ignore;
    Sim.Object.put conn ~bucket:"test" ~key:"hello.txt" "Hello!" |> ignore;
    Sim.Object.get conn ~bucket:"test" ~key:"hello.txt" ()
    ]} *)

(** {1 Deterministic clock} *)

(** A clock that only advances when you tell it to. *)
module Clock : sig
  type t

  val create : ?now:Ptime.t -> unit -> t
  (** Defaults to [2026-01-01T00:00:00Z]. *)

  val now : t -> Ptime.t
  val advance : t -> Ptime.Span.t -> unit

  val advance_ms : t -> int -> unit
  (** Truncates to whole seconds, minimum 1 second. E.g. [advance_ms 500]
      advances 1s, [advance_ms 2500] advances 2s. *)
end

(** {1 Store and connections} *)

type config = { max_list_keys : int }
(** Store configuration. *)

val default_config : config

type store
(** Shared in-memory store. Multiple connections can share one store. *)

val create_store : ?config:config -> clock:Clock.t -> unit -> store

type t
(** A simulated connection. Own credentials and fault state, shared store. *)

val connect : store -> credentials:Awskit.Credentials.t -> t

val store : t -> store
(** Access the underlying store for inspection. *)

(** {1 Fault injection} *)

(** Injectable fault types.

    - [Slow_down] — returns [`Service_unavailable]
    - [Internal_error] — returns [`Request_error (500, ...)]
    - [Connection_reset] — returns [`Request_error (0, "connection reset")]
    - [Response_lost] — the operation happens but the response is lost
      (simulates a network drop after S3 processed the request) *)
type fault = Slow_down | Internal_error | Connection_reset | Response_lost

val inject_fault : t -> fault -> unit
(** Next operation fails with this fault. *)

val inject_faults : t -> fault list -> unit
(** Next N operations fail in order. *)

val clear_faults : t -> unit

val enable_buggify : t -> seed:int -> prob:float -> unit
(** Probabilistic fault injection (FoundationDB-style). Each operation has
    probability [prob] of a random fault. [seed] for reproducibility. *)

val disable_buggify : t -> unit

(** {1 Object operations} *)

module Object : sig
  val put :
    t ->
    bucket:string ->
    key:string ->
    ?preconditions:Object_.Preconditions.Write.t ->
    ?content_type:string ->
    ?metadata:Metadata.t ->
    ?storage_class:Storage_class.t ->
    ?tags:Tag.t list ->
    ?cache_control:string ->
    ?content_encoding:string ->
    ?content_disposition:string ->
    string ->
    (Object_.Put_result.t, Error.t) result

  val get :
    t ->
    bucket:string ->
    key:string ->
    ?range:Range.t ->
    ?preconditions:Object_.Preconditions.Read.t ->
    unit ->
    (Object_.Get_result.t, Error.t) result

  val head :
    t ->
    bucket:string ->
    key:string ->
    ?preconditions:Object_.Preconditions.Read.t ->
    unit ->
    (Object_.Info.t option, Error.t) result

  val delete :
    t ->
    bucket:string ->
    key:string ->
    ?preconditions:Object_.Preconditions.Delete.t ->
    unit ->
    (unit, Error.t) result

  module Delete_object : sig
    type t = {
      key : string;
      version_id : string option;
      etag : string option;
      last_modified_time : string option;
      size : int option;
    }
    [@@deriving show, eq]

    val v :
      ?version_id:string ->
      ?etag:string ->
      ?last_modified_time:string ->
      ?size:int ->
      string ->
      t
  end

  val delete_batch :
    t ->
    bucket:string ->
    objects:Delete_object.t list ->
    (Object_.Delete_result.t, Error.t) result

  val copy :
    t ->
    src_bucket:string ->
    src_key:string ->
    dst_bucket:string ->
    dst_key:string ->
    ?source_preconditions:Object_.Preconditions.Copy_source.t ->
    ?metadata:Object_.Copy_metadata.t ->
    unit ->
    (Object_.Copy_result.t, Error.t) result
  (** Copy an object inside the simulated store. [metadata] matches the real
      client: omitted or [`Copy] keeps source metadata; [`Replace metadata]
      replaces it. *)

  val list :
    t ->
    bucket:string ->
    prefix:string ->
    ?delimiter:string ->
    ?max_keys:int ->
    ?start_after:string ->
    ?continuation_token:string ->
    unit ->
    (Object_.List_result.t, Error.t) result

  module Tagging : sig
    val get : t -> bucket:string -> key:string -> (Tag.t list, Error.t) result

    val put :
      t -> bucket:string -> key:string -> Tag.t list -> (unit, Error.t) result

    val delete : t -> bucket:string -> key:string -> (unit, Error.t) result
  end
end

(** {1 Bucket operations} *)

module Bucket : sig
  val create :
    t -> bucket:string -> ?region:string -> unit -> (unit, Error.t) result

  val delete : t -> bucket:string -> (unit, Error.t) result
  val head : t -> bucket:string -> (bool, Error.t) result
  val list : t -> (Bucket.Info.t list, Error.t) result

  module Policy : sig
    val get : t -> bucket:string -> (Policy.t, Error.t) result
    val put : t -> bucket:string -> Policy.t -> (unit, Error.t) result
    val delete : t -> bucket:string -> (unit, Error.t) result
  end
end

(** {1 Inspection and history} *)

type op_record = {
  op : [ `Put | `Get | `Head | `Delete | `List | `Copy | `Delete_batch ];
  bucket : string;
  key : string;
  timestamp : Ptime.t;
  faulted : bool;
}
(** Record of a single operation performed on the simulation. *)

type object_meta = { etag : string; size : int; last_modified : Ptime.t }
(** Object metadata for inspection (not the same as S3 response types). *)

val object_meta : store -> bucket:string -> string -> object_meta option
(** Look up object metadata (bypasses access checks). *)

val keys : store -> bucket:string -> string list
(** List all keys (bypasses access checks). *)

val history : store -> op_record list
val clear_history : store -> unit

val dump : store -> bucket:string -> (string * string) list
(** Dump all key-value pairs in a bucket. *)
