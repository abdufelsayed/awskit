open Simulator_support

(** Direct-style runtime used by the in-memory simulator. *)

module Runtime : sig
  include
    Awskit_s3.RUNTIME
      with type connection = Simulator_state.t
       and type 'a t = 'a

  val request_body_result : request_body -> (string, Awskit.Error.t) result
  (** Materialize a simulator request body as an in-memory string. *)

  val response_body : ?read_fault:Awskit.Error.t -> string -> response_body
  (** Build an in-memory response body, optionally failing reads with
      [read_fault]. *)
end
