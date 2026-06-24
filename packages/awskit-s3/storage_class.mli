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
  | Unknown of string
      (** Unknown storage-class values returned by S3. These values are
          preserved on reads for forward compatibility, but write operations
          reject them before sending a request. *)

val to_string : t -> string
(** Render the AWS storage-class spelling used in headers and XML. *)

val of_string : string -> t
(** Parse an AWS storage-class spelling. Unknown values are preserved. *)
