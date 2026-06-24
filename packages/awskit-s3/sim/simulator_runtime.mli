open Simulator_support

module Runtime : sig
  include
    Awskit_s3.RUNTIME
      with type connection = Simulator_state.t
       and type 'a t = 'a

  val request_body_result : request_body -> (string, Awskit.Error.t) result
  val response_body : ?read_fault:Awskit.Error.t -> string -> response_body
end
