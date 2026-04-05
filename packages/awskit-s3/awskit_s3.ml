(** Pure S3 client core — types, policies, simulation, XML wire types, and Make
    functor parameterized by Awskit.Runtime.S. No IO dependencies. *)

(* Pure types *)
module Error = Error
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Metadata = Metadata

(* Policy — self-contained *)
module Policy = Policy

(* Resource types (types + XML wire formats; operations live in Make) *)
module Object = Object_
module Bucket = Bucket
module Multipart = Multipart

(* Pure URL generation *)
module Presigned = Presigned

(* Simulation *)
module Sim = Sim

(* Functor — parameterize over a Runtime to get the full S3 API *)
module Make = Client.Make
