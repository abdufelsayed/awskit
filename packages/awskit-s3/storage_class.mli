(** S3 object storage classes. *)

type t =
  | Standard
  | Reduced_redundancy
  | Standard_ia
  | Onezone_ia
  | Intelligent_tiering
  | Glacier
  | Glacier_ir
  | Deep_archive
  | Outposts
  | Snow
  | Express_onezone
  | Fsx_openzfs
  | Fsx_ontap
  | Other of string
      (** A storage-class value not modeled by awskit. This covers future AWS
          values and provider-specific S3-compatible storage classes. *)

val to_string : t -> string
(** Render the AWS storage-class spelling used in headers and XML. *)

val of_string : string -> (t, Awskit.Error.t) result
(** Parse a storage-class spelling. Values modeled by awskit use their
    constructor; other non-empty values are preserved as [Other]. *)

val of_string_exn : string -> t
(** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
    structured validation error on validation failure. *)
