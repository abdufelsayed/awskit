(** S3 bucket policy — typed statements, JSON serialization, and evaluation.

    A policy consists of {b statements}, each with an {!Effect} (Allow/Deny), a
    {!Principal} (who), actions (what), and {!Resource}s (where).

    {[
    (* Build a policy programmatically *)
    let policy : Policy.t =
      {
        version = "2012-10-17";
        statements =
          [
            {
              effect_ = Allow;
              principal = Any;
              actions = [ "s3:GetObject" ];
              resources = [ Bucket_objects "my-bucket" ];
            };
          ];
      }

    (* Or parse from JSON *)
    let policy = Policy.of_json json_string
    ]}

    Evaluate access with {!val:evaluate}:
    {[
      match Policy.evaluate policy ~access_key_id ~action ~target with
      | `Allowed -> (* access granted *)
      | `Denied  -> (* explicit deny *)
      | `No_match -> (* no statement matched, default deny *)
    ]} *)

(** Policy effect: Allow or Deny. *)
module Effect : sig
  type t = Allow | Deny [@@deriving show, eq]

  val to_string : t -> string
  val of_string : string -> t option
  val yojson_of_t : t -> Yojson.Safe.t
end

(** Principal — who the statement applies to.

    - [Any] — matches all principals (the ["*"] wildcard)
    - [Aws arns] — matches specific AWS ARNs
    - [User ids] — matches specific user IDs *)
module Principal : sig
  type t = Any | Aws of string list | User of string list
  [@@deriving show, eq]

  val yojson_of_t : t -> Yojson.Safe.t
end

(** Resource that a statement targets.

    - [Bucket name] — targets the bucket (e.g., [arn:aws:s3:::my-bucket])
    - [Bucket_objects name] — targets objects (e.g., [arn:aws:s3:::my-bucket/*])
*)
module Resource : sig
  type t = Bucket of string | Bucket_objects of string [@@deriving show, eq]

  val to_arn : t -> string
  (** Convert to ARN string. *)

  val of_arn : string -> t option
  (** Parse from ARN string. *)

  val yojson_of_t : t -> Yojson.Safe.t

  type one_or_many = t list [@@deriving show, eq]

  val yojson_of_one_or_many : one_or_many -> Yojson.Safe.t
end

(** Action type (string matching S3 action names). *)
module Action : sig
  type t = string [@@deriving show, eq]

  val yojson_of_t : t -> Yojson.Safe.t

  type one_or_many = t list [@@deriving show, eq]

  val yojson_of_one_or_many : one_or_many -> Yojson.Safe.t
end

(** A single policy statement. *)
module Statement : sig
  type t = {
    effect_ : Effect.t;
    principal : Principal.t;
    actions : Action.one_or_many;
    resources : Resource.one_or_many;
  }
  [@@deriving show, eq, yojson_of]
end

(** The target of a policy evaluation. *)
type target =
  | Bucket_target of string  (** Evaluating access to the bucket itself *)
  | Object_target of string * string
      (** Evaluating access to an object [(bucket, key)] *)

type t = { version : string; statements : Statement.t list }
[@@deriving show, eq, yojson_of]
(** A bucket policy. *)

val to_json : t -> string
(** Serialize a policy to JSON. *)

val of_json : string -> (t, Error.t) result

val evaluate :
  t ->
  access_key_id:string ->
  action:string ->
  target:target ->
  [ `Allowed | `Denied | `No_match ]
(** AWS evaluation semantics: explicit deny wins over allow. No matching
    statement → [`No_match] (default deny). *)
