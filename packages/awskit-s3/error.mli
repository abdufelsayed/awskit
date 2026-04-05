(** S3-specific error types. Extends {!Awskit.Error.base} with S3 variants. All
    S3 operations return [('a, Error.t) result].

    {[
    | Error `Not_found                (* object doesn't exist *)
    | Error `Precondition_failed       (* if-match/if-none-match failed *)
    | Error `Bucket_already_exists     (* bucket name taken *)
    | Error `Bucket_not_empty          (* can't delete non-empty bucket *)
    | Error `No_such_bucket            (* bucket doesn't exist *)
    | Error (#Awskit.Error.base as e)      (* any other AWS error *)
    ]} *)

type t =
  [ Awskit.Error.base
  | `Not_found  (** HTTP 404 — object does not exist *)
  | `Precondition_failed  (** HTTP 412 — conditional request failed *)
  | `Bucket_already_exists  (** HTTP 409 — bucket name taken *)
  | `Bucket_not_empty  (** HTTP 409 — can't delete non-empty bucket *)
  | `No_such_bucket  (** HTTP 404 — bucket does not exist *) ]

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

val of_aws_error : Awskit.Error.base -> t
(** Widen a base AWS error to an S3 error. *)

val of_status : int -> string -> t
(** Classify an HTTP error response into an S3-specific error. Parses the XML
    body for the error code. *)
